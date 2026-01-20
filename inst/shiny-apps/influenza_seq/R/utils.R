# R/utils.R

`%||%` <- function(a, b) if (is.null(a)) b else a

`%||str%` <- function(a, b) {
  if (is.null(a) || length(a) < 1) return(b)
  x <- as.character(a[[1]])
  if (is.na(x) || x == "") b else x
}

scalar_chr <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_character_)
  as.character(x[[1]])
}

scalar_lgl <- function(x) {
  if (is.null(x) || length(x) < 1) return(FALSE)
  isTRUE(x[[1]])
}

dbg <- function(...) message(sprintf("[nf-flu shiny] %s", paste0(..., collapse = "")))

ensure_cols <- function(df, cols) {
  for (cc in cols) {
    if (!(cc %in% names(df))) df[[cc]] <- NA
  }
  df
}

rtrim_slash <- function(x) sub("/+$", "", x)

path_to_url <- function(fs_path, data_root, base_url) {
  if (is.null(base_url)) return(NULL)
  rel <- fs::path_rel(fs_path, start = data_root)
  rel <- gsub("\\\\", "/", rel)
  paste0(rtrim_slash(base_url), "/", rel)
}

normalize_id <- function(x) {
  x <- as.character(x)
  stringr::str_replace(x, "_[12]$", "")
}

standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)
  # Prefer seqName over sample if both exist (Nextclade case)
  cand <- intersect(nms, c("sample_id", "seqName", "sample", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) {
    df <- dplyr::rename(df, sample_id = dplyr::all_of(cand[[1]]))
  } else {
    df$sample_id <- NA_character_
  }
  dplyr::mutate(df, sample_id = as.character(sample_id))
}

