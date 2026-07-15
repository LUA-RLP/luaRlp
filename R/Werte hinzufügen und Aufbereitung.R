#' merge_info
#'
#' @title Merge ausgewählte Variablen aus einer Lookup-Tabelle
#' @description In den Argumenten muss neben dem Datensatznahmen (1. Argument: data) auch der Name des Datensatzes angegeben werden, der
#'              die Informationen enthält (2. Argument: lookup). Darauf folgt die Variable, nach der gemerged werden
#'              soll (3. Argument: by). Darauf können alle Variablen angegeben werden, die aus lookup behalten werden sollen (4.-x. Argument).
#'              Zuletzt gibt es die Möglichkeit, anzugeben, dass ein m:n-Merge vermieden werden soll.
#'
#' @param data Datensatz, der verwendet werden soll
#' @param lookup Dies ist der Datensatz, aus dem die zusätzlich benötigte Information gezogen werden soll,
#'                bspw. IdType_Datensatzkategorie oder geo_standards
#' @param by Dieser Parameter wird in Anführungszeichen angegeben. Es ist die Variable, nach der gemerged werden soll.
#' @param ... Hier können alle aus dem lookup Datensatz benötigten Variablen aufgezählt und, wenn gewünscht,
#'            umbenannt werdeen
#' @param first Standardvorbelegung FALSE. Das bedeutet, dass ein 1:1 oder m:1-Merge problemlos funktioniert.
#'              Sollte die by-Variable im lookup-Datensatz nicht einzigartig sein, erhält man aus der Funktion
#'              eine Fehlermeldung. first=TRUE löst das Problem, indem nur eine Zeile pro by-Variable vor dem
#'              Merge im lookup-Datensatz beibehalten wird.
#'
#' @return  Hineingegebener Datensatz mit zusätzlicher Information aus Dateien des Paktes oder des Workspace
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' data <- merge_info(data,geo_standards,"LK_ID", Meldelandkreis, umriss, centroid, GID_2, Wohnort_GA = Gesundheitsamt)
#' data <- merge_info(data,geo_standards,"Eigentuemer_ID", Kürzel, Eigentümer_GA = Gesundheitsamt,first=TRUE)
#' data <- merge_info(data,IdType_Datensatzkategorie,"IdType", Datensatzkategorie)
#'

merge_info <- function(data, lookup, by, ..., first = FALSE) {

  # gewünschte Variablen ermitteln
  vars <- names(tidyselect::eval_select(
    rlang::expr(c(...)),
    lookup
  ))

  # sf-Geometrie nur behalten, wenn explizit ausgewählt
  if (inherits(lookup, "sf")) {
    geom <- attr(lookup, "sf_column")

    if (!geom %in% vars) {
      lookup <- sf::st_drop_geometry(lookup)
    }
  }

  # Eindeutigkeit des Lookup prüfen
  duplicates <- lookup %>%
    count(across(all_of(by))) %>%
    filter(n > 1)

  if (nrow(duplicates) > 0) {

    if (first) {

      lookup <- lookup %>%
        arrange(across(all_of(by))) %>%
        distinct(across(all_of(by)), .keep_all = TRUE)

    } else {

      stop(
        "Mehrere Zeilen pro by-Variable im Lookup-Datensatz.\n",
        "Bitte das Argument first = TRUE verwenden.\n",
        "Beachte: Nur Variablen, die auf Ebene der by-Variable konstant sind, sollten übernommen werden.",
        call. = FALSE
      )
    }
  }

  data %>%
    left_join(
      lookup %>% select(all_of(by), ...),
      by = by,
      relationship = "many-to-one"
    )
}



#'  Add values
#'
#' @return A data frame with columns, where value labels have been added for specific variables
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' add_values(import_SurvNet(build_query()))
#'
#'
add_values <- function(df) {

  # Standard-Mutates, die immer existieren
  df <- df %>%
    mutate(
      Meldedatum = as.Date(.data$Meldedatum),
      Erkrankungsbeginn = as.Date(.data$Erkrankungsbeginn),
      HospitalisierungStatus = recode(as.character(.data$HospitalisierungStatus),
                                      "10" = "nein", "20" = "ja", "-1" = "-nicht ermittelbar-",
                                      "0" = "-nicht erhoben-"),
      VerstorbenStatus = recode(as.character(.data$VerstorbenStatus),
                                "10" = "nein", "20" = "ja", "-1" = "-nicht ermittelbar-",
                                "0" = "-nicht erhoben-"),
      Geschlecht = recode(as.character(.data$Geschlecht),
                          "1" = "männlich", "2" = "weiblich", "3" = "nicht-binär",
                          "-1" = "-nicht ermittelbar-", "0" = "-nicht erhoben-"),
      Referenzdefinition = recode(as.character(.data$Referenzdefinition),
                                  "1" = "Ja", "0" = "nein")
    ) %>%
    rename(Meldejahr = WeekYear, Meldewoche = Week, AlterBerechnet = AgeComputed)

  # Optionale Spalten nur hinzufügen, wenn sie existieren
  if ("GültigAb_DMYhs" %in% names(df)) {
    df$GültigAb <- as.Date(df$GültigAb_DMYhs)
  }
  if ("AtGACreated" %in% names(df)) {
    df$AtGA <- as.Date(df$AtGACreated)
  }
  if ("AtLSCreated" %in% names(df)) {
    df$AtLS <- as.Date(df$AtLSCreated)
  }
  if ("Falldefkategorie" %in% names(df)) {
    df$Falldefkategorie <- recode(
      trimws(as.character(df$Falldefkategorie)),
      "-1" = "-nicht ermittelbar",
      "1" = "klinisch",
      "2" = "klinisch-epidemiologisch",
      "3" = "klinisch-labordiagnostisch",
      "4" = "laborsdiagnostisch bei nicht erfüllter Klinik",
      "5" = "labordiagnostisch bei unbekannter Klinik",
      "11" = "spezifisch klinisch",
      "31" = "klinisch-labordiagnostisch (C1)"
    )
  }
  if ("P112Relevant_" %in% names(df)) {
    df$P112Relevant_ <- recode(trimws(as.character(df$P112Relevant_)), "1" = "Ja", "0" = "nein")
  }
  if ("StatusPlaceOfInf_" %in% names(df)) {
    df$StatusPlaceOfInf_ <- recode(
      as.character(df$StatusPlaceOfInf_),
      "10" = "nein", "20" = "ja", "-1" = "-nicht ermittelbar-",
      "0" = "-nicht erhoben-")
  }
  if ("StatusPatientSetting_" %in% names(df)) {
    df$StatusPatientSetting_ <- recode(
      trimws(as.character(df$StatusPatientSetting_)),
      "-1" = "-nicht ermittelbar",
      "0" = "-nicht erhoben-",
      "10" = "ohne",
      "19" = "Betreut/untergebracht in Einrichtung gemäß §23",
      "20" = "Tätigkeit in Einrichtungen gemäß §23",
      "21" = "Betreut in Gemeinschaftseinrichtung nach §33",
      "22" = "Tätigkeit in Einrichtungen gemäß §33",
      "23" = "Betreut in Einrichtung nach §36",
      "24" = "Tätigkeit in Einrichtung nach §36",
      "25" = "Tätigkeit mit Lebensmitteln nach §42"
    )
  }

  if ("AusbruchInfo_InterneRef" %in% names(df)) {
    Ausbruch = ifelse(!is.na(df$AusbruchInfo_InterneRef), 1, 0)
  }

  # Merge immer
  #Info Landkreis
  df <- merge_info(df, geo_standards,"LK_ID", umriss, centroid, GID_2,
                   Meldelandkreis, Wohnort_GA = Gesundheitsamt)
  #Info Eigentümer
  df <- merge_info(df,geo_standards,"Eigentuemer_ID", Kürzel,
                   Eigentümer_GA = Gesundheitsamt, first=TRUE)

    #Vergleich Wohnort_GA und Eigentümer_GA
  df <- df %>% mutate(GA_Differenz = if_else(Wohnort_GA == Eigentümer_GA,
                          "Wohnort in Eigentümer_LK", "Achtung: Wohnort außerhalb Eigentümer_LK"))

  #Info Datensatzkategorien
  df <- merge_info(df,IdType_Datensatzkategorie,"IdType", Datensatzkategorie)

  # Merge nur, wenn Region_ID existiert
  if (all(c("ExpKont1", "ExpSubKont1", "ExpLand1", "ExpBL1", "ExpLK1",
            "ExpKont2") %in% names(df))) {
   df <- .combine_exposure(df)
  }

  # Merge nur, wenn Pathogen_ID existiert
  if ("Pathogen_ID" %in% names(df)) {
    df <- merge_info(df,pathogen_dictionary,"Pathogen_ID", Erreger)
    df$Pathogen_ID <- NULL
  }


  ## ## Unnötige oder doppelte Spalten entfernen sind m.E. nicht unnötig
  ##  spalten_entfernen <- c("Eigentuemer", "AtGACreated", "AtLSCreated", "GültigAb_DMYhs", "IdType", "LK_ID", "Kürzel", "PlaceOfInf_")
  ## df <- df[, !(names(df) %in% spalten_entfernen)]

  return(df)


}


#' .combine_exposure
#'
#' Derives a single "Expositionsort" Variable from multiple exposure-related
#' variables avialiable in SurvNet (via luaRlp::import_SurvNet())
#'
#' @param data Usually a tibble generated by \code{\link{import_SurvNet()}}
#'
#' @return A tibble derived the
#' tibble as generated by \code{\link{import_SurvNet()}}, but with
#' Expositionsort standardized
#'
#'@import dplyr
#'@importFrom magrittr %>%
#'
.combine_exposure <- function(data){
  # function starting with ".", only used internally
  # Expositionsort: Combine information from multiple exposure variables into one
  dplyr::mutate(data,
                Expositionsort = case_when(
                  !is.na(.data$ExpKont2) ~ "mehrere Orte",
                  !is.na(.data$ExpLK1) & .data$ExpBL1 == "Rheinland-Pfalz" ~
                    as.character(.data$ExpLK1),
                  !is.na(.data$ExpBL1) & .data$ExpLand1 == "Deutschland" ~
                    as.character(ExpBL1),
                  !is.na(.data$ExpLand1) ~ as.character(.data$ExpLand1),
                  !is.na(.data$ExpSubKont1) ~ as.character(.data$ExpSubKont1),
                  !is.na(.data$ExpKont1) ~ as.character(.data$ExpKont1),
                  TRUE ~ NA_character_
                )
  ) %>%
    dplyr::select(!(ExpKont1:ExpKont2))  # Remove columns from ExpKont1 to ExpKont2
}


