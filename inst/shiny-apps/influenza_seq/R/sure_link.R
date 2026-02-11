library(readr)
library(dplyr)
library(stringr)
library(openssl)

read_sure_csv <- function(path, encoding = "Windows-1252") {
  df <- read_csv2(
    file = path,
    locale = locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE
  )

  pr <- problems(df)
  if (nrow(pr) > 0) {
    bad <- sort(unique(pr$row))
    message("Dropping malformed row(s): ", paste(head(bad, 20), collapse = ", "),
            if (length(bad) > 20) " ..." else "")
    # problems() row numbers refer to data rows (not header) in readr tibbles
    df <- df[-bad, , drop = FALSE]
  }

  # Normalize text encoding (prevents later string ops from exploding)
  df <- df %>%
    mutate(across(
      where(is.character),
      ~ iconv(.x, from = encoding, to = "UTF-8", sub = "")
    ))

  df
}

clean_sure <- function(df) {
  df %>%
    rename_with(~ str_replace_all(.x, "\\s+", "_")) %>%
    mutate(across(where(is.character), ~ str_squish(.x))) %>%
    mutate(
      Importdatum    = as.Date(Importdatum),
      Eingangsdatum  = as.Date(Eingangsdatum),
      Probenahmedatum = suppressWarnings(as.Date(Probenahmedatum)),
      Laborbereich   = as.character(Laborbereich),
      Geburtsmonat   = suppressWarnings(as.integer(Geburtsmonat)),
      Geburtsjahr    = suppressWarnings(as.integer(Geburtsjahr)),
      ID             = as.character(ID)
    )
}

SURE1 <- read_sure_csv("O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/SURE_DATA_mirror/SURE_Export.csv") |> clean_sure()
SURE2 <- read_sure_csv("O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/SURE_DATA_mirror/SURE_Export.csv")       |> clean_sure()

common_cols <- intersect(names(SURE1), names(SURE2))
SURE <- bind_rows(
  SURE1 %>% select(all_of(common_cols)),
  SURE2 %>% select(all_of(common_cols))
) %>% distinct()


md5_id <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)

  # openssl::md5 returns raw; convert to hex string
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- vapply(
    x[ok],
    function(s) paste0(openssl::md5(charToRaw(s)), collapse = ""),
    character(1)
  )
  out
}


join_sure <- function(samples_df, sure_df) {
  if (is.null(samples_df) || nrow(samples_df) == 0) return(samples_df)
  if (is.null(sure_df) || nrow(sure_df) == 0) return(samples_df)

  sure2 <- sure_df %>%
    dplyr::mutate(ID = tolower(trimws(as.character(ID)))) %>%
    dplyr::distinct(ID, .keep_all = TRUE)

  samples_df %>%
    dplyr::mutate(
      sample_id = as.character(sample_id),
      sample_md5 = tolower(md5_id(sample_id))
    ) %>%
    dplyr::left_join(sure2, by = c("sample_md5" = "ID"))
}

samples_only_sure <- function(df) {
  df %>% dplyr::filter(!is.na(sample_md5) & !is.na(Importdatum))  # or any other SURE field
}




