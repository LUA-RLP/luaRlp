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

parse_nextclade <- function(nx_raw) {
  empty <- tibble::tibble(
    sample_id = character(), clade = character(), subclade = character(),
    qc_status = character(), qc_score = numeric()
  )
  if (is.null(nx_raw)) return(empty)

  dbg("nextclade columns: ", paste(names(nx_raw), collapse = ","))

  out <- nx_raw

  # Prefer seqName as sample_id if present (most reliable join key)
  if ("seqName" %in% names(out)) {
    out <- dplyr::rename(out, sample_id = seqName)
  } else {
    out <- standardize_sample_id(out)
  }

  out <- dplyr::mutate(out, sample_id = normalize_id(sample_id))

  # clade/subclade
  if (!("clade" %in% names(out)) && "Clade" %in% names(out)) out <- dplyr::rename(out, clade = Clade)
  if (!("subclade" %in% names(out))) {
    sc <- names(out)[grepl("subclade|sub_clade", names(out), ignore.case = TRUE)][1]
    if (!is.na(sc)) out <- dplyr::rename(out, subclade = dplyr::all_of(sc))
  }

  # qc
  qs <- names(out)[grepl("qc.*status|overallStatus", names(out), ignore.case = TRUE)][1]
  if (!is.na(qs) && !("qc_status" %in% names(out))) out <- dplyr::rename(out, qc_status = dplyr::all_of(qs))
  qsc <- names(out)[grepl("qc.*score|overallScore", names(out), ignore.case = TRUE)][1]
  if (!is.na(qsc) && !("qc_score" %in% names(out))) out <- dplyr::rename(out, qc_score = dplyr::all_of(qsc))

  out <- ensure_cols(out, c("sample_id","clade","subclade","qc_status","qc_score")) %>%
    dplyr::transmute(
      sample_id = as.character(sample_id),
      clade = as.character(clade),
      subclade = as.character(subclade),
      qc_status = as.character(qc_status),
      qc_score = suppressWarnings(as.numeric(qc_score))
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)

  dbg("parse_nextclade: parsed rows=", nrow(out))
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
  if (!("sample_id" %in% names(out)) &&
      ("...1" %in% names(out))) {
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

  # influenza type
  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType", "virusType"))
  if (length(type_col) >= 1 && !("influenza_type" %in% nms)) out <- dplyr::rename(out, influenza_type = dplyr::all_of(type_col[[1]]))

  # subtype (combined)
  sub_col <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype", "subtypePrediction"))
  if (length(sub_col) >= 1 && !("subtype" %in% nms)) out <- dplyr::rename(out, subtype = dplyr::all_of(sub_col[[1]]))

  # H / N columns: be liberal (predictions file naming varies)
  if (!("H" %in% nms)) {
    h_col <- nms[grepl("(^H$)|H.*subtype|H.*pred|HA.*subtype|ha.*subtype", nms, ignore.case = TRUE)][1]
    if (!is.na(h_col)) out <- dplyr::rename(out, H = dplyr::all_of(h_col))
  }
  if (!("N" %in% nms)) {
    n_col <- nms[grepl("(^N$)|N.*subtype|N.*pred|NA.*subtype|na.*subtype", nms, ignore.case = TRUE)][1]
    if (!is.na(n_col)) out <- dplyr::rename(out, N = dplyr::all_of(n_col))
  }

  out <- ensure_cols(out, c("sample_id","influenza_type","H","N","subtype"))

  # derive from subtype if needed
  st <- as.character(out$subtype)
  if (all(is.na(out$H))) out$H <- stringr::str_extract(st, "H\\d+")
  if (all(is.na(out$N))) out$N <- stringr::str_extract(st, "N\\d+")
  if (all(is.na(out$influenza_type))) out$influenza_type <- stringr::str_extract(st, "^[AB]")

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

