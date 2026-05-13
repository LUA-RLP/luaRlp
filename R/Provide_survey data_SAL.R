
#' Fragebogen_fuer_GAE
#'
#' @description Diese Funktion wird immer ausgeführt, wenn neue Fragebogendaten für die GÄ zur Verfügung //
#' gestellt werden sollen. Davor muss das File aus EUSurvey im u.g. Ordner abgelegt werden. Die Ergebnisse finden //
#' sich dann hier: "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/Erreger/Salmonellen/Salmonellen_IMS/Fragebogen/Fragebogeninhalte_fuer_GÄ/FragebogenSAL"
#'
#' @import readxl
#' @import openxlsx
#'
#' @export
#'
#' @examples Fragebogen_fuer_GAE()

Fragebogen_fuer_GAE <- function(  base_dir = "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/Erreger/Salmonellen/Salmonellen_IMS/Fragebogen/Fragebogeninhalte_fuer_GÄ",
                                  filename = "Content_Export_FragebogenSalmonellen.xlsx") {

  file <- paste0(base_dir, "/",  filename)

  raw <- read_excel(file, sheet = "Content", col_names = FALSE, skip = 3)

  # erste Zeile extrahieren und 2 Spalten entfernen
  header_row <- raw[1, ] |> unlist() |> as.character()
  header_row_adjusted <- header_row[-c(3, 101)]

  # letzte Klammer extrahieren außer bei den letzten beiden Spalten
  new_names <- header_row_adjusted
  new_names[1:100] <- str_match(header_row_adjusted[1:100], "\\(([^()]*)\\)\\s*$")[,2]
  new_names[101] <- "contributionID"
  new_names[102] <- "creation_date"

  # Daten extrahieren und 2 Spalten entfernen
  data <- raw[-1, ]
  data <- data %>% select(-c(3,101))

  #Spaltennamen hinzufügen
  colnames(data) <- new_names

  #Datumsspalten korrigieren
  data$creation_date <- as.numeric(data$creation_date)
  data$creation_date <- as.POSIXct(
    data$creation_date * 86400,
    origin = "1899-12-30",
    tz = "UTC"
  )


  #Vorbereitung auf den Merge mit den SurvNet-Daten
  data <- data %>%
    mutate(
      aktenzeichen = as.character(Aktenzeichen),
      az_upper = str_to_upper(Aktenzeichen)
    ) %>%
    filter(Aktenzeichen != "AKTENZEICHEN")

  #Validierung des GA-Kürzels mittels der Datei geo_standards
  valid_prefix <- geo_standards$Kürzel %>%
    str_to_upper()

  GA_info <- geo_standards %>%
    mutate(
      Eigentuemer = str_extract(Gesundheitsamt, "(?<=\\()[^()]+(?=\\))")
    ) %>%
    distinct(Eigentuemer, .keep_all = TRUE) %>%
    select(Eigentuemer, Kürzel)

  #Valide Aktenzeichen (Präfix und Suffix) werden zusätzlich in zwei neue Spalten geschrieben
  data <- data %>%
    rowwise() %>%
    mutate(
      prefix = {
        matches <- valid_prefix[str_starts(az_upper, valid_prefix)]
        if (length(matches) == 0) NA_character_ else matches[1]
      },

      suffix = if (!is.na(prefix)) {
        str_remove(az_upper, paste0("^", prefix, "-?"))
      } else {
        NA_character_
      }
    ) %>%
    ungroup()

  #Import der Tagen aus SurvNet zum Abgleich
  Abgleich <- import_SurvNet(build_query(Thema="SerovarSAL"))
  Abgleich <- Abgleich %>%
    left_join(
      GA_info,
      by = "Eigentuemer"
    ) %>%
    select(Aktenzeichen, Kürzel)

  #Merge mit Fragebogendaten, dabei sicherstellen, dass nur Aktenzeichen verbunden werden, die unique sind.

  data_clean <- data %>%
    group_by(Aktenzeichen) %>%
    filter(n() == 1) %>%
    ungroup()

  Abgleich_clean <- Abgleich %>%
    group_by(Aktenzeichen) %>%
    filter(n() <= 1) %>%
    ungroup()

  result <- left_join(data_clean, Abgleich_clean, by = "Aktenzeichen")
  result <- result %>%
    mutate(GA = coalesce(prefix, Kürzel))

  #Name des Gesundheitsamts hinzufügen
  GA_name <- as.data.frame(geo_standards) %>% distinct(Gesundheitsamt, .keep_all = TRUE) %>%
    mutate(GA = Kürzel) %>% select(GA, Gesundheitsamt)
  result <- left_join(result, GA_name, by = "GA")  %>%
    filter(!is.na(GA)) %>%
    select(-aktenzeichen,-az_upper,-prefix,-suffix,-Kürzel,-GA)

  # -------------------------
  # Export vorbereiten
  # -------------------------

  #Pfade definieren

  template_file <- file.path(base_dir, "Template_SAL.xlsx")
  target_dir <- file.path(base_dir, "FragebogenSAL")
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  backup_dir <- file.path(base_dir, paste0("FragebogenSAL_ersetzt_", date_str))

  if (!dir.exists(base_dir)) {
    stop(paste("base_dir: ", base_dir, "nicht erreichbar"))
  }

  #Zielordner
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(backup_dir, showWarnings = FALSE)

  #Alte Dateien sichern
  old_files <- list.files(target_dir, full.names = TRUE)

  if (length(old_files) > 0) {
    file.copy(old_files, backup_dir, overwrite = TRUE)
    file.remove(old_files)

  }

  #Neue Dateien erstellen: Loop über alle GÄ

  ga_list <- unique(result$Gesundheitsamt)

  for (ga in ga_list) {

    # Dateiname bereinigen (wichtig für Windows)
    safe_ga <- gsub("[^A-Za-z0-9_]", "_", ga)

    # Ziel-Datei
    out_file <- file.path(target_dir, paste0(safe_ga, ".xlsx"))


    # Template kopieren
    file.copy(template_file, out_file, overwrite = TRUE)

    df_ga <- result %>%
      filter(Gesundheitsamt == ga)

    wb <- loadWorkbook(out_file)

    #Alte Daten löschen
    writeData(
      wb,
      sheet = "Daten",
      x = NULL,
      startRow = 2
    )

    # Neue Daten schreiben
    writeData(
      wb,
      sheet = "Daten",
      x = df_ga,
      startRow = 2,
      colNames = FALSE
    )

    # Textumbruch
    wrap_style <- createStyle(wrapText = TRUE)

    addStyle(
      wb,
      sheet = "Daten",
      style = wrap_style,
      rows = 2:(nrow(df_ga) + 1),
      cols = 1:ncol(df_ga),
      gridExpand = TRUE
    )

    # Datum formatieren
    date_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm")

    addStyle(
      wb,
      sheet = "Daten",
      style = date_style,
      rows = 2:(nrow(df_ga) + 1),
      cols = which(names(df_ga) == "creation_date"),
      gridExpand = TRUE
    )

    #Speichern
    saveWorkbook(wb, out_file, overwrite = TRUE)

    message(paste("Fertig:", ga))
  }

  message("Alle Dateien erfolgreich erstellt.")

}
