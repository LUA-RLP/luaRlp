# Libraries

#' einlesen_sure
#'
#' @param path Pfad auf dem der LIMS-Export liegt
#'
#' @return Ein datenframe zur Weiterverwendung
#'
#' @import lubridate
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @import readr
#'
#' @export
#'
#' @examples einlesen_sure()
einlesen_sure <- function(path = "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/SURE/Daten") {

  filename1 <- "SURE_Export.csv"
  filename2 <- "SURE_Export_Teil1.csv"
  complete_path1 <- file.path(path, filename1)
  complete_path2 <- file.path(path, filename2)

# Import and combine data
  data_import <- bind_rows(
    read.csv(complete_path1, fileEncoding = "latin1", sep = ";", colClasses = c("Importdatum" = "Date", "Eingangsdatum" = "Date", "Probenahmedatum" = "Date")),
    read.csv(complete_path2, fileEncoding = "latin1", sep = ";", colClasses = c("Importdatum" = "Date", "Eingangsdatum" = "Date", "Probenahmedatum" = "Date"))
  )

  # Change main variable names and add diagnostic lab
  data <- data_import %>%
    rename_with(tolower) %>%
    rename(import=importdatum, eingang=eingangsdatum, abstrichdatum=probenahmedatum)%>%
    mutate(Labor = ifelse(laborbereich == 3161, "Koblenz", ifelse(laborbereich == 3261, "Landau", NA)))

  #------------------------------------------------------------------
  # Data corrections
  #------------------------------------------------------------------

  # Delete duplicates
  n_before <- nrow(data)
  data <- data %>%
    arrange(id,import) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  n_after <- nrow(data)
  differenz <- n_before - n_after
  cat("Anzahl der entfernten Duplikate:", differenz, "\n")

  # Replacements for einsender
  data <- data %>%
    mutate(einsender = ifelse(einsender == "xprdughio", "xprdughob", einsender),
           einsender = ifelse(einsender == "xprerco", "xprscbuco", einsender),
           einsender = ifelse(einsender == "xprschmo", "xprgamo", einsender),
           einsender = ifelse(einsender == "xprgrhi", "xprgrge", einsender))

  # Replacements for invalid dates in abstrichdatum
  data <- data %>%
    mutate(abstrichdatum = case_when(abstrichdatum == as_date("1911-01-01") |
                                       abstrichdatum == as_date("1999-01-01")~ as.Date(NA), TRUE ~ abstrichdatum))

  # Create a problem list with specific cases for later corrections
  data <- data %>%
    mutate(
      lag_eingang = eingang - abstrichdatum,
      lag_ergebnis = import - eingang
    )

  # Fehlerbedingungen definieren
  fehler_abstrich <- with(data,
                          (!is.na(abstrichdatum) & (abstrichdatum > Sys.Date() | abstrichdatum < as_date("2023-01-19"))) |
                            (!is.na(lag_eingang) & lag_eingang > 30)
  )

  fehler_eingang <- with(data,
                         (!is.na(eingang) & (eingang > Sys.Date() | eingang < as_date("2023-01-19"))) |
                           (!is.na(lag_ergebnis) & lag_ergebnis > 30)
  )

  # Anzahl berechnen
  anzahl_abstrich <- sum(fehler_abstrich, na.rm = TRUE)
  anzahl_eingang  <- sum(fehler_eingang, na.rm = TRUE)
  anzahl_gesamt   <- sum(fehler_abstrich | fehler_eingang, na.rm = TRUE)

  # Werte korrigieren
  data <- data %>%
    mutate(
      abstrichdatum = if_else(fehler_abstrich, as.Date(NA), abstrichdatum),
      eingang = if_else(fehler_eingang, as.Date(NA), eingang)
    ) %>%
    select(-lag_eingang, -lag_ergebnis)

  # Message ausgeben
  message(
    "Plausibilitätsprüfung:\n",
    anzahl_abstrich, " fehlerhafte Werte in 'abstrichdatum' mit fehlendem Wert ersetzt\n",
    anzahl_eingang,  " fehlerhafte Werte in 'eingang' mit fehlendem Wert ersetzt\n",
    anzahl_gesamt,   " Datensätze insgesamt betroffen."
  )

  #------------------------------------------------------------------
  # Create date Variables
  #------------------------------------------------------------------

  # Create a main date variable and extract year, month, week, and day of the week
  data <- data %>%
    mutate(base_datum = coalesce(abstrichdatum, eingang, import),
           Jahr = year(base_datum),
           Monat = month(base_datum),
           Wochentag = wday(base_datum, week_start = 1),
           Woche = isoweek(base_datum),
    )
  #correct year to match with the calendar week
  data$Jahr <- ifelse(data$Monat==12 & data$Woche==1, data$Jahr+1,
                      ifelse(data$Monat==1 & data$Woche>=52, data$Jahr-1,data$Jahr))

  #------------------------------------------------------------------
  # Create results Variables
  #------------------------------------------------------------------

  # clean results
  data <- data %>%
    mutate(across(starts_with("ergebnis"),
                  ~str_to_lower(str_remove_all(., " "))))

  # helper function
  detect_virus <- function(data, virus_regex) {

    pos <- rep(FALSE, nrow(data))
    neg <- rep(FALSE, nrow(data))

    for(i in 1:8){

      pm <- data[[paste0("prüfmerkmal", i)]]
      er <- data[[paste0("ergebnis", i)]]

      match <- str_detect(pm, regex(virus_regex, ignore_case = TRUE))

      pos <- pos | (match & str_detect(er, "positiv"))
      neg <- neg | (match & str_detect(er, "negativ"))
    }

    case_when(
      pos ~ 1,
      neg ~ 0,
      TRUE ~ NA_real_
    )
  }

  # create virus variables
  data <- data %>%
    mutate(
      SARS      = detect_virus(., "SARS[- ]?CoV[- ]?2"),
      InfluA    = detect_virus(., "Influenza[- ]?A"),
      InfluB    = detect_virus(., "Influenza[- ]?B"),
      RSV       = detect_virus(., "RSV"),
      Rhino     = detect_virus(., "Rhino"),
      Parainflu = detect_virus(., "Parainflu"),
      Adeno     = detect_virus(., "Adeno"),
      Metapneu  = detect_virus(., "Metapneu"),

      influenza = case_when(
        InfluA == 1 | InfluB == 1 ~ 1,
        InfluA == 0 & InfluB == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )

  #------------------------------------------------------------------
  # Create age of patient
  #------------------------------------------------------------------

  # Age of patient (birthday is set to 15th of respective month)
  data <- data %>%
    mutate(
      birthday = case_when(
        is.na(geburtsjahr) | is.na(geburtsmonat) ~ as.Date(NA),
        geburtsjahr < 1900 | geburtsjahr > lubridate::year(Sys.Date()) ~ as.Date(NA),
        geburtsmonat < 1 | geburtsmonat > 12 ~ as.Date(NA),
        TRUE ~ as.Date(sprintf("%04d-%02d-15", geburtsjahr, geburtsmonat))
      )
    )

data <- data %>%
  mutate(
    Alter = as.numeric(base_datum - birthday) / 365.25,
    Altersgruppen3 = cut(
      Alter,
      breaks = c(0, 2, 6, 18, 40, 60, 120),
      labels = c("<2 Jahre", "2-5 Jahre", "6-17 Jahre", "18-39 Jahre",
                 "40-59 Jahre", "60 Jahre und älter"),
      right = FALSE
    )
  )

#------------------------------------------------------------------
# information from Praxisliste
#------------------------------------------------------------------
  # add information about the practitioners
  data <- left_join(data, Praxisliste, by = "einsender")

#------------------------------------------------------------------
# Extract additional information summarized in the variable "angaben"
#------------------------------------------------------------------

data <- data %>%
  mutate(
    symptome        = str_extract(angaben, "(?<=symptome=)[^#]*"),
    seit           = str_extract(angaben, "(?<=seit=)[^#]*"),
    tage            = str_extract(angaben, "(?<=tage=)[^#]*"),
    wochen          = str_extract(angaben, "(?<=wochen=)[^#]*"),
    beginn          = str_extract(angaben, "(?<=beginn=)[^#]*"),
    schwere         = str_extract(angaben, "(?<=schwere=)[^#]*"),
    test            = str_extract(angaben, "(?<=test=)[^#]*"),
    sarstest        = str_extract(angaben, "(?<=sars=)[^#]*"),
    influenzatest   = str_extract(angaben, "(?<=influenza=)[^#]*"),
    rsvtest         = str_extract(angaben, "(?<=rsv=)[^#]*"),
    grunderkrankung = str_extract(angaben, "(?<=grunderkrankung=)[^#]*"),
    welche          = str_extract(angaben, "(?<=welche=)[^#]*"),
    impfung         = str_extract(angaben, "(?<=impfung=)[^#]*")
  )

  #calculate time since onset of disease (seit)
 data <- data %>%
    mutate(
      across(c(symptome, grunderkrankung, impfung),
             ~if_else(str_detect(., "[A-Za-z]"), "", .)),
        seit1  = suppressWarnings(readr::parse_number(seit)),
        tage   = suppressWarnings(readr::parse_number(str_replace(tage,",","."))),
        wochen = suppressWarnings(readr::parse_number(str_replace(wochen,",",".")))

    )

 data <- data %>%
   mutate(
     seit = case_when(
       seit1 == 1 ~ 0,
       !is.na(tage) ~ tage,
       !is.na(wochen) ~ wochen * 7,
       TRUE ~ NA_real_
     )
   )

  # Specific changes for the variable schwere, which will be used in the subsequent analysis
  data <- data %>%
    mutate(schwere=ifelse(schwere==0, NA, schwere)) %>%
    mutate(schwere = na_if(trimws(schwere), ""))

  return(data)
  }



# Aggregate data frame by calendar week and year and calculate additional variables for presentation

#' Title
#'
#' @param sure_data Datei, die mit der Funktion einlesen_sure() erzeugt wird
#' @param week Die Kalenderwoche, die abgefragt wird. Auswahl zwischen "all", "current", "previous52" und einer Zahl bzw. einer Liste von Zahlen
#' @param year Das Jahr, die abgefragt wird. Auswahl zwischen "all", "current", "previous2" und einer Zahl bzw. einer Liste von Zahlen
#'
#' @return Aggregierter Datenframe mit ausgewählten Wochen
#' @export
#'
#' @examples
#' aggregate_woche(,"current","current")
#' aggregate_woche(,"all","all")
#' aggregate_woche(,c(1,2,3),2025)
#'
aggregate_woche <- function(sure_data,week,year) {

  # Calculate number of mail sendings received per week and year
  einsendungen_data <- sure_data %>%
    group_by(Jahr, Woche) %>%
    summarise(einsendungen = n_distinct(einsender), .groups = "drop")

  # correct pathogen variables for multiple infections
  df_summary <- sure_data %>%
    mutate(
      SARS_crude = SARS,
      InfluA_crude = InfluA,
      InfluB_crude = InfluB,
      influenza_crude = influenza,
      RSV_crude = RSV,
      SARS = ifelse(SARS == 1 & multi == 1, 0, SARS),
      InfluA = ifelse(InfluA == 1 & multi == 1, 0, InfluA),
      InfluB = ifelse(InfluB == 1 & multi == 1, 0, InfluB),
      influenza = ifelse(influenza == 1 & multi == 1, 0, influenza),
      RSV = ifelse(RSV == 1 & multi == 1, 0, RSV)
    )

  # Aggregate data on week and year level
  df_summary <- df_summary %>%
    group_by(Jahr, Woche) %>%
    summarise(
      anzahl = n(),
      SARS = sum(SARS, na.rm = TRUE),
      InfluA = sum(InfluA, na.rm = TRUE),
      InfluB = sum(InfluB, na.rm = TRUE),
      influenza = sum(influenza, na.rm = TRUE),
      RSV = sum(RSV, na.rm = TRUE),
      multi = sum(multi, na.rm = TRUE),
      SARS_crude = sum(SARS_crude, na.rm = TRUE),
      InfluA_crude = sum(InfluA_crude, na.rm = TRUE),
      InfluB_crude = sum(InfluB_crude, na.rm = TRUE),
      influenza_crude = sum(influenza_crude, na.rm = TRUE),
      RSV_crude = sum(RSV_crude, na.rm = TRUE)
    )

  #Make sure, there are no missing weeks and delete potential remaining erroneous date values

  #create complete list of weeks and years
  alle_wochen <- expand.grid(
    Jahr = unique(df_summary$Jahr),
    Woche = 1:52
  )

  # Merge with data frame
  df_summary <- full_join(alle_wochen, df_summary, by = c("Jahr", "Woche")) %>%
    filter(!(Woche < 4 & Jahr <= 2023)) %>%
    filter(!(Woche>lubridate::isoweek(Sys.Date() - 7) & Jahr >= year(Sys.Date() - 7))) %>%
    filter(!Woche==53) #wir hassen KW 53


  # Merge with number of mail sendings per week and year
  df_summary <- merge(df_summary, einsendungen_data, by = c("Jahr", "Woche"), all.x = TRUE)

  # Calculate moving averages (last 4 weeks)
  df_summary <- df_summary %>%
    arrange(Jahr, Woche) %>%
    mutate(
      anzahl_ma = lag(anzahl, 1) + lag(anzahl, 2) + lag(anzahl, 3) + anzahl,
      SARS_ma = lag(SARS, 1) + lag(SARS, 2) + lag(SARS, 3) + SARS,
      InfluA_ma = lag(InfluA, 1) + lag(InfluA, 2) + lag(InfluA, 3) + InfluA,
      InfluB_ma = lag(InfluB, 1) + lag(InfluB, 2) + lag(InfluB, 3) + InfluB,
      RSV_ma = lag(RSV, 1) + lag(RSV, 2) + lag(RSV, 3) + RSV,
      multi_ma = lag(multi, 1) + lag(multi, 2) + lag(multi, 3) + multi,
      einsendungen_ma = lag(einsendungen, 1) + lag(einsendungen, 2) + lag(einsendungen, 3) + einsendungen
    )

  # Calculate proportions (in %) and create variables for graph
  df_summary <- df_summary %>%
    mutate(across(starts_with("SARS_ma"):starts_with("RSV_ma"), ~ . / einsendungen_ma, .names = "prop_{.col}")) %>%
    mutate(across(starts_with("SARS_crude"):starts_with("RSV_crude"), ~ . / anzahl, .names = "anteil_{.col}")) %>%
    mutate(Datum = as.Date(paste(Jahr, Woche, 1, sep = "-"), "%Y-%U-%u")) %>%
    mutate(negativ = anzahl - (SARS + InfluA + InfluB + RSV + multi)) %>%
    mutate(pos_real = (SARS + influenza + RSV),
           prop_real = pos_real / anzahl,
           Numerus_anzahl = ifelse(anzahl == 1, "Probe", "Proben"),
           Numerus_SARS = ifelse(SARS_crude == 1, "Nachweis", "Nachweise"),
           Numerus_influenza = ifelse(influenza_crude == 1, "Nachweis", "Nachweise"),
           Numerus_RSV = ifelse(RSV_crude == 1, "Nachweis", "Nachweise"),
           Numerus_multi = ifelse(multi == 1, "Probe", "Proben")
    )

  #set filter for year and week based on the parameters year and week

  if (length(week) == 1 && week=="current") {
    df_summary <- df_summary %>%
    filter(Woche==lubridate::isoweek(Sys.Date() - 7))
  } else if (length(week) == 1 && week=="all") {
        df_summary <- df_summary
  } else if (length(week) == 1 && week=="previous52") {
    df_summary <- df_summary %>%
      filter(!(Woche <= lubridate::isoweek(Sys.Date() - 7) & Jahr < year(Sys.Date() - 7)))
  } else if (is.numeric(week)) {
        df_summary <- df_summary %>%
        filter(Woche %in% week)
  } else {
    message("Für 'week' bitte 'current', 'all' oder ein numerisches Element angeben!")
  }


  if (length(year) == 1 && year=="current") {
    df_summary <- df_summary %>%
      filter(Jahr==year(Sys.Date() - 7))
  } else if (length(year) == 1 && year=="all") {
    df_summary <- df_summary
  } else if (length(year) == 1 && year=="previous2") {
    df_summary <- df_summary %>%
      filter(Jahr==year(Sys.Date() - 7) | Jahr==year(Sys.Date() - 372))
  } else if (is.numeric(year)) {
    df_summary <- df_summary %>%
      filter(Jahr %in% year)
  } else{
    message("Für 'year' bitte 'current', 'all', 'previous2' oder ein numerisches Element angeben!")
  }

df_summary
}

