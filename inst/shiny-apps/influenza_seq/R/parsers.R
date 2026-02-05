# R/parsers.R

read_samplesheet <- function(samplesheet_path) {
  if (is.na(samplesheet_path) || !fs::file_exists(samplesheet_path)) return(NULL)
  ss <- suppressWarnings(readr::read_csv(samplesheet_path, show_col_types = FALSE))
  if (!("sample" %in% names(ss))) return(NULL)

  ss %>%
    dplyr::rename(sample_id = sample) %>%
    dplyr::mutate(sample_id = as.character(sample_id)) %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)
}

find_first_existing <- function(paths) {
  hit <- paths[fs::file_exists(paths)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

find_nextclade_path <- function(results_dir) {
  candidates <- c(
    fs::path(results_dir, "nextclade", "nextclade.tsv"),
    fs::path(results_dir, "nextclade", "nextclade.csv")
  )
  hit <- find_first_existing(candidates)
  if (!is.na(hit)) return(hit)

  # fallback: any tsv/csv in nextclade dir
  nd <- fs::path(results_dir, "nextclade")
  if (fs::dir_exists(nd)) {
    any <- fs::dir_ls(nd, type = "file", glob = "*.{tsv,csv}", fail = FALSE)
    if (length(any) > 0) return(any[[1]])
  }
  NA_character_
}

normalize_nextclade_id <- function(x) {
  x <- as.character(x)
  x <- sub("\\s.*$", "", x)                         # cut off anything after first whitespace
  x <- sub("\\.gz$", "", x, ignore.case = TRUE)
  x <- sub("\\.(fast|f)q$", "", x, ignore.case = TRUE)
  x <- sub("\\.(fasta|fa|fna)$", "", x, ignore.case = TRUE)

  # common nf-flu / consensus naming patterns
  x <- sub("\\.majority_consensus.*$", "", x, ignore.case = TRUE)
  x <- sub("\\.consensus.*$", "", x, ignore.case = TRUE)
  x <- sub("\\.(irma|bcftools|vadr)(\\..*)?$", "", x, ignore.case = TRUE)

  # sometimes consensus names end with _consensus
  x <- sub("_consensus.*$", "", x, ignore.case = TRUE)

  # keep your existing paired-read normalization too (harmless here)
  x <- normalize_id(x)

  x
}



find_subtyping_path <- function(results_dir) {
  # prefer predictions over raw results
  candidates <- c(
    fs::path(results_dir, "subtyping_report", "subtype_predictions.csv"),
    fs::path(results_dir, "subtyping_report", "subtype_results.csv"),
    # sometimes under irma
    fs::path(results_dir, "irma", "subtyping_report", "subtype_predictions.csv"),
    fs::path(results_dir, "irma", "subtyping_report", "subtype_results.csv")
  )
  hit <- find_first_existing(candidates)
  if (!is.na(hit)) return(hit)

  # fallback: anything plausible in subtyping_report dirs
  sd1 <- fs::path(results_dir, "subtyping_report")
  if (fs::dir_exists(sd1)) {
    any <- fs::dir_ls(sd1, type="file", glob="*.csv", fail=FALSE)
    if (length(any) > 0) return(any[[1]])
  }
  sd2 <- fs::path(results_dir, "irma", "subtyping_report")
  if (fs::dir_exists(sd2)) {
    any <- fs::dir_ls(sd2, type="file", glob="*.csv", fail=FALSE)
    if (length(any) > 0) return(any[[1]])
  }
  NA_character_
}

read_nextclade_raw <- function(results_dir) {
  f <- find_nextclade_path(results_dir)
  if (is.na(f)) return(NULL)
  dbg("nextclade file: ", f)
  if (grepl("\\.csv$", f, ignore.case = TRUE)) {
    return(suppressWarnings(readr::read_csv(f, show_col_types = FALSE)))
  }
  suppressWarnings(readr::read_tsv(f, show_col_types = FALSE))
}

read_subtyping_raw <- function(results_dir) {
  f <- find_subtyping_path(results_dir)
  if (is.na(f)) return(NULL)
  dbg("subtyping file: ", f)
  x <- suppressWarnings(readr::read_csv(f, show_col_types = FALSE))
  dbg("subtyping columns: ", paste(names(x), collapse = ","))
  x
}


read_pass_readcount_raw <- function(results_dir) {
  f <- fs::path(results_dir, "read_count", "pass_read_count_samples_mqc.tsv")
  if (!fs::file_exists(f)) return(NULL)

  rc <- suppressWarnings(readr::read_tsv(f, show_col_types = FALSE))
  if (is.null(rc) || ncol(rc) < 2) {
    rc <- suppressWarnings(readr::read_delim(f, delim = "\\s+", show_col_types = FALSE, trim_ws = TRUE))
  }
  if (is.null(rc) || ncol(rc) < 2) return(NULL)
  rc
}

parse_readcount <- function(rc_raw) {
  if (is.null(rc_raw) || ncol(rc_raw) < 2) {
    return(tibble::tibble(sample_id = character(), read_count = numeric()))
  }

  nms <- names(rc_raw)
  sample_col <- intersect(nms, c("Sample", "sample", "SAMPLE"))
  read_col <- intersect(nms, c("Read count", "Read_count", "Read.count", "read_count", "ReadCount"))

  sample_col <- if (length(sample_col) >= 1) sample_col[[1]] else nms[[1]]
  read_col   <- if (length(read_col) >= 1) read_col[[1]] else nms[[2]]

  out <- rc_raw %>%
    dplyr::transmute(
      sample_id = normalize_id(as.character(.data[[sample_col]])),
      read_count = suppressWarnings(as.numeric(.data[[read_col]]))
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)

  ensure_cols(out, c("sample_id", "read_count")) %>%
    dplyr::select(sample_id, read_count)
}

parse_nextclade_with_key <- function(nx_raw, key_col = c("seqName", "sample")) {
  key_col <- match.arg(key_col)

  empty <- tibble::tibble(
    sample_id = character(), clade = character(), subclade = character(),
    qc_status = character(), qc_score = numeric()
  )
  if (is.null(nx_raw)) return(empty)

  dbg("nextclade columns: ", paste(names(nx_raw), collapse = ","))

  out <- nx_raw

  if (!(key_col %in% names(out))) {
    dbg("parse_nextclade_with_key: missing key_col=", key_col, " -> returning empty")
    return(empty)
  }

  out <- out %>%
    dplyr::mutate(sample_id = normalize_nextclade_id(.data[[key_col]]))

  # clade/subclade are already present in your logs
  if (!("clade" %in% names(out)) && "Clade" %in% names(out)) out <- dplyr::rename(out, clade = Clade)
  if (!("subclade" %in% names(out))) {
    sc <- names(out)[grepl("subclade|sub_clade", names(out), ignore.case = TRUE)][1]
    if (!is.na(sc)) out <- dplyr::rename(out, subclade = dplyr::all_of(sc))
  }

  # QC fields (optional)
  qs <- names(out)[grepl("qc.*status|overallStatus", names(out), ignore.case = TRUE)][1]
  if (!is.na(qs) && !("qc_status" %in% names(out))) out <- dplyr::rename(out, qc_status = dplyr::all_of(qs))
  qsc <- names(out)[grepl("qc.*score|overallScore", names(out), ignore.case = TRUE)][1]
  if (!is.na(qsc) && !("qc_score" %in% names(out))) out <- dplyr::rename(out, qc_score = dplyr::all_of(qsc))

  out <- ensure_cols(out, c("sample_id","clade","subclade","qc_status","qc_score","dataset_name","coverage"))

  out <- out %>%
    dplyr::transmute(
      sample_id   = as.character(sample_id),
      dataset_name = as.character(dataset_name),
      clade       = dplyr::na_if(as.character(clade), ""),
      subclade    = dplyr::na_if(as.character(subclade), ""),
      qc_status   = as.character(qc_status),
      qc_score    = suppressWarnings(as.numeric(qc_score)),
      coverage    = suppressWarnings(as.numeric(coverage))
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "")

  # ---- NEW: choose best row per sample (prefer HA, then any clade, then coverage) ----
  out <- out %>%
    dplyr::mutate(
      is_ha = grepl("/ha(\\b|/)", dataset_name, ignore.case = TRUE) | grepl("segment4", dataset_name, ignore.case = TRUE),
      is_na = grepl("/na(\\b|/)", dataset_name, ignore.case = TRUE) | grepl("segment6", dataset_name, ignore.case = TRUE),
      has_clade = !is.na(clade) | !is.na(subclade)
    ) %>%
    dplyr::arrange(
      dplyr::desc(has_clade),
      dplyr::desc(is_ha),
      dplyr::desc(is_na),
      dplyr::desc(coverage)
    ) %>%
    dplyr::group_by(sample_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(sample_id, clade, subclade, qc_status, qc_score)

  dbg("parse_nextclade_with_key(", key_col, "): parsed unique samples=", nrow(out))
  out
}




parse_subtyping <- function(sub_raw) {
  # fixed columns: sample_id, influenza_type, H, N, subtype
  empty <- tibble::tibble(
    sample_id = character(),
    influenza_type = character(),
    H = character(),
    N = character(),
    subtype = character()
  )
  if (is.null(sub_raw)) return(empty)

  dbg("parse_subtyping: incoming cols: ", paste(names(sub_raw), collapse = ","))

  out <- sub_raw

  # If first column is unnamed, readr calls it ...1 — that's often the sample id
  if (!("sample_id" %in% names(out)) && ("...1" %in% names(out))) {
    out <- dplyr::rename(out, sample_id = `...1`)
  }

  # Otherwise use our standard heuristics (sample_id/seqName/sample/...)
  out <- standardize_sample_id(out)

  # Still nothing? Fall back to first column as sample id
  if (all(is.na(out$sample_id)) && ncol(out) >= 1) {
    out$sample_id <- as.character(out[[1]])
  }

  out <- dplyr::mutate(out, sample_id = normalize_id(sample_id))

  nms <- names(out)

  # influenza type (optional; will stay NA if not present)
  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType", "virusType"))
  if (length(type_col) >= 1 && !("influenza_type" %in% nms)) {
    out <- dplyr::rename(out, influenza_type = dplyr::all_of(type_col[[1]]))
  }

  # ---- STRICT subtype/H/N: use only Subtype Prediction, fallback to VADR Subtype ----
  extract_H  <- function(x) stringr::str_extract(as.character(x), "H\\d+")
  extract_N  <- function(x) stringr::str_extract(as.character(x), "N\\d+")
  extract_HN <- function(x) stringr::str_extract(as.character(x), "H\\d+N\\d+")

  # Ensure these columns exist (some runs/files might not have them)
  if (!("Subtype Prediction" %in% names(out))) out$`Subtype Prediction` <- NA_character_
  if (!("VADR Subtype" %in% names(out)))       out$`VADR Subtype`       <- NA_character_

  subtype1 <- extract_HN(out$`Subtype Prediction`)
  subtype2 <- extract_HN(out$`VADR Subtype`)

  out$subtype <- subtype1
  out$subtype[is.na(out$subtype)] <- subtype2[is.na(out$subtype)]

  out$H <- extract_H(out$subtype)
  out$N <- extract_N(out$subtype)

  # Clean + optional influenza type extraction (only if your Subtype Prediction encodes it; otherwise stays NA)
  out$subtype <- dplyr::na_if(as.character(out$subtype), "")
  if (!("influenza_type" %in% names(out))) out$influenza_type <- NA_character_
  out$influenza_type <- dplyr::na_if(as.character(out$influenza_type), "")

  dbg("parse_subtyping: subtype non-NA=", sum(!is.na(out$subtype)))

  out <- ensure_cols(out, c("sample_id", "influenza_type", "H", "N", "subtype"))

  out2 <- out %>%
    dplyr::transmute(
      sample_id = as.character(sample_id),
      influenza_type = as.character(influenza_type),
      H = as.character(H),
      N = as.character(N),
      subtype = as.character(subtype)
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)

  dbg("parse_subtyping: parsed rows=", nrow(out2))
  out2
}

parse_nextclade_ha <- function(next_raw) {
  empty <- tibble::tibble(
    sample_id = character(),
    clade = character(),
    subclade = character(),
    qc_status = character(),
    qc_score = numeric(),
    coverage = numeric()
  )
  if (is.null(next_raw) || nrow(next_raw) == 0) return(empty)

  dbg("parse_nextclade_ha: incoming cols: ", paste(names(next_raw), collapse = ","))

  x <- next_raw

  # Key column: sample id is in `sample`
  if (!("sample" %in% names(x))) return(empty)
  x <- dplyr::mutate(x, sample_id = normalize_id(.data$sample))

  # Segment detection: prefer seqName, fallback to dataset_name
  seg <- rep(NA_character_, nrow(x))
  if ("seqName" %in% names(x)) {
    seg <- stringr::str_extract(x$seqName, "(PB2|PB1|PA|HA|NP|NA|M|NS)")
  }
  if (all(is.na(seg)) && "dataset_name" %in% names(x)) {
    seg <- toupper(stringr::str_extract(x$dataset_name, "(pb2|pb1|pa|ha|np|na|m|ns)"))
  }
  x$segment <- seg

  # Normalise columns we care about (they exist in your file)
  get_chr <- function(col) if (col %in% names(x)) dplyr::na_if(as.character(x[[col]]), "") else NA_character_
  get_num <- function(col) if (col %in% names(x)) suppressWarnings(as.numeric(x[[col]])) else NA_real_

  # We keep the "classic" clade/subclade first, but also prefer "short-clade" if present
  # because in many flu datasets that’s what people *actually* report (e.g. 2.3.4.4b).
  x <- dplyr::mutate(
    x,
    clade_raw      = get_chr("clade"),
    subclade_raw   = get_chr("subclade"),
    short_clade    = get_chr("short-clade"),
    proposed_sub   = get_chr("proposedSubclade"),
    qc_status_raw  = get_chr("qc.overallStatus"),
    qc_score_raw   = get_num("qc.overallScore"),
    coverage_raw   = get_num("coverage")
  )

  # Choose the HA row if available, otherwise fallback to the row that has any clade info
  # (useful for weird/incomplete runs)
  pick_one <- x %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::group_by(sample_id) %>%
    dplyr::arrange(
      dplyr::desc(segment == "HA"),
      dplyr::desc(!is.na(short_clade) | !is.na(clade_raw) | !is.na(subclade_raw)),
      dplyr::desc(!is.na(qc_status_raw)),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Final reported values:
  # - clade: prefer short-clade (often what you want to show), otherwise clade
  # - subclade: prefer proposedSubclade, otherwise subclade
  out <- pick_one %>%
    dplyr::transmute(
      sample_id = as.character(sample_id),
      clade     = dplyr::coalesce(short_clade, clade_raw),
      subclade  = dplyr::coalesce(proposed_sub, subclade_raw),
      qc_status = as.character(qc_status_raw),
      qc_score  = as.numeric(qc_score_raw),
      coverage  = as.numeric(coverage_raw)
    )

  dbg("parse_nextclade_ha: parsed rows=", nrow(out))
  out
}



