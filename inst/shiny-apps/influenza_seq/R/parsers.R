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

read_nextclade_raw <- function(results_dir) {
  f <- fs::path(results_dir, "nextclade", "nextclade.tsv")
  if (!fs::file_exists(f)) return(NULL)
  suppressWarnings(readr::read_tsv(f, show_col_types = FALSE))
}

read_subtyping_raw <- function(results_dir) {
  f <- fs::path(results_dir, "subtyping_report", "subtype_results.csv")
  if (!fs::file_exists(f)) return(NULL)
  suppressWarnings(readr::read_csv(f, show_col_types = FALSE))
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
  if (is.null(nx_raw)) {
    return(tibble::tibble(
      sample_id = character(), clade = character(), subclade = character(),
      qc_status = character(), qc_score = numeric()
    ))
  }

  out <- standardize_sample_id(nx_raw) %>%
    dplyr::mutate(sample_id = normalize_id(sample_id))

  if (!("clade" %in% names(out))) {
    cc <- intersect(names(out), c("Clade"))
    if (length(cc) >= 1) out <- dplyr::rename(out, clade = dplyr::all_of(cc[[1]]))
  }
  if (!("subclade" %in% names(out))) {
    sc <- intersect(names(out), c("Subclade"))
    if (length(sc) >= 1) out <- dplyr::rename(out, subclade = dplyr::all_of(sc[[1]]))
  }

  qs <- intersect(names(out), c("qc.overallStatus", "qc_overallStatus", "overallStatus"))
  if (length(qs) >= 1 && !("qc_status" %in% names(out))) out <- dplyr::rename(out, qc_status = dplyr::all_of(qs[[1]]))

  qsc <- intersect(names(out), c("qc.overallScore", "qc_overallScore", "overallScore"))
  if (length(qsc) >= 1 && !("qc_score" %in% names(out))) out <- dplyr::rename(out, qc_score = dplyr::all_of(qsc[[1]]))

  out <- ensure_cols(out, c("sample_id", "clade", "subclade", "qc_status", "qc_score")) %>%
    dplyr::transmute(
      sample_id = as.character(sample_id),
      clade = as.character(clade),
      subclade = as.character(subclade),
      qc_status = as.character(qc_status),
      qc_score = suppressWarnings(as.numeric(qc_score))
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)

  out
}

parse_subtyping <- function(sub_raw) {
  if (is.null(sub_raw)) {
    return(tibble::tibble(
      sample_id = character(), influenza_type = character(),
      H = character(), N = character(), subtype = character()
    ))
  }

  out <- standardize_sample_id(sub_raw) %>%
    dplyr::mutate(sample_id = normalize_id(sample_id))

  nms <- names(out)

  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType"))
  sub_col  <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype"))

  if (length(type_col) >= 1 && !("influenza_type" %in% names(out))) out <- dplyr::rename(out, influenza_type = dplyr::all_of(type_col[[1]]))
  if (length(sub_col)  >= 1 && !("subtype" %in% names(out))) out <- dplyr::rename(out, subtype = dplyr::all_of(sub_col[[1]]))

  h_col <- intersect(nms, c("H", "H_subtype", "HA", "ha_subtype", "H_type"))
  n_col <- intersect(nms, c("N", "N_subtype", "NA", "na_subtype", "N_type"))
  if (length(h_col) >= 1 && !("H" %in% names(out))) out <- dplyr::rename(out, H = dplyr::all_of(h_col[[1]]))
  if (length(n_col) >= 1 && !("N" %in% names(out))) out <- dplyr::rename(out, N = dplyr::all_of(n_col[[1]]))

  out <- ensure_cols(out, c("sample_id", "influenza_type", "H", "N", "subtype"))

  # derive if possible
  st <- as.character(out$subtype)
  if (all(is.na(out$H))) out$H <- stringr::str_extract(st, "H\\d+")
  if (all(is.na(out$N))) out$N <- stringr::str_extract(st, "N\\d+")
  if (all(is.na(out$influenza_type))) out$influenza_type <- stringr::str_extract(st, "^[AB]")

  out %>%
    dplyr::transmute(
      sample_id = as.character(sample_id),
      influenza_type = as.character(influenza_type),
      H = as.character(H),
      N = as.character(N),
      subtype = as.character(subtype)
    ) %>%
    dplyr::filter(!is.na(sample_id) & sample_id != "") %>%
    dplyr::distinct(sample_id, .keep_all = TRUE)
}
