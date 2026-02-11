# R/sure_link.R
# SURE linking helpers for nf-flu shiny
# App author: Emanuel Heitlinger

library(readr)
library(dplyr)
library(stringr)
library(openssl)

# ---- CONFIG: set these paths in config.R if you want ----
SURE_PATHS_DEFAULT <- c(
  "/mnt/o/Abteilung Humanmedizin (AHM)/Referat 32/32_6/SURE_DATA_mirror/SURE_Export.csv",
  "/mnt/o/Abteilung Humanmedizin (AHM)/Referat 32/32_6/SURE_DATA_mirror/SURE_Export_Teil1.csv"
)

read_sure_csv <- function(path, encoding = "Windows-1252") {
  df <- readr::read_csv2(
    file = path,
    locale = readr::locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE
  )

  pr <- readr::problems(df)
  if (nrow(pr) > 0) {
    bad <- sort(unique(pr$row))
    message("Dropping malformed row(s): ", paste(head(bad, 20), collapse = ", "),
            if (length(bad) > 20) " ..." else "")
    df <- df[-bad, , drop = FALSE]
  }

  # normalize encoding to prevent later string ops exploding
  df <- df %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~ iconv(.x, from = encoding, to = "UTF-8", sub = "")
    ))

  df
}

clean_sure <- function(df) {
  df %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "\\s+", "_")) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_squish(.x))) %>%
    dplyr::mutate(
      Importdatum     = as.Date(Importdatum),
      Eingangsdatum   = as.Date(Eingangsdatum),
      Probenahmedatum = suppressWarnings(as.Date(Probenahmedatum)),
      Laborbereich    = as.character(Laborbereich),
      Geburtsmonat    = suppressWarnings(as.integer(Geburtsmonat)),
      Geburtsjahr     = suppressWarnings(as.integer(Geburtsjahr)),
      ID              = as.character(ID)
    )
}

# ---- MD5 helper ----
md5_id <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)

  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)

  # IMPORTANT: as.character() on openssl::md5(raw) yields hex string
  out[ok] <- vapply(
    x[ok],
    function(s) as.character(openssl::md5(charToRaw(s))),
    character(1)
  )
  tolower(out)
}

# ---- Load + cache SURE data ----
.sure_cache <- NULL

get_sure_data <- function(paths = SURE_PATHS_DEFAULT, encoding = "Windows-1252", force = FALSE) {
  if (!force && !is.null(.sure_cache)) return(.sure_cache)

  dfs <- lapply(paths, function(p) {
    if (!file.exists(p)) {
      message("SURE file missing: ", p)
      return(NULL)
    }
    clean_sure(read_sure_csv(p, encoding = encoding))
  })
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) {
    .sure_cache <<- tibble::tibble()
    return(.sure_cache)
  }

  common_cols <- Reduce(intersect, lapply(dfs, names))
  sure <- dplyr::bind_rows(lapply(dfs, function(d) dplyr::select(d, dplyr::all_of(common_cols)))) %>%
    dplyr::distinct()

  # normalize ID key
  sure <- sure %>%
    dplyr::mutate(ID = tolower(trimws(as.character(ID)))) %>%
    dplyr::filter(!is.na(ID) & ID != "")

  .sure_cache <<- sure
  sure
}

# ---- This is what epi_data() wants ----
get_sure_ids <- function(...) {
  sure <- get_sure_data(...)
  if (is.null(sure) || nrow(sure) == 0 || !("ID" %in% names(sure))) {
    return(tibble::tibble(ID = character()))
  }
  sure %>% dplyr::distinct(ID) %>% dplyr::filter(!is.na(ID) & ID != "")
}

# ---- Joining helpers ----
join_sure <- function(samples_df, sure_df) {
  if (is.null(samples_df) || nrow(samples_df) == 0) return(samples_df)
  if (is.null(sure_df) || nrow(sure_df) == 0) return(samples_df)

  sure2 <- sure_df %>%
    dplyr::mutate(ID = tolower(trimws(as.character(ID)))) %>%
    dplyr::distinct(ID, .keep_all = TRUE)

  samples_df %>%
    dplyr::mutate(
      sample_id  = as.character(sample_id),
      sample_md5 = md5_id(sample_id)
    ) %>%
    dplyr::left_join(sure2, by = c("sample_md5" = "ID"))
}

samples_only_sure <- function(df) {
  # pick a robust indicator column from SURE (Importdatum exists in your data)
  df %>% dplyr::filter(!is.na(Importdatum))
}
