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
    Altersgruppen = cut(
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

#' @title Funktion zum Aggregieren der aufbereiteten SURE-Daten aus einlesen_sure() nach KW und Jahr
#'
#' @param sure_data Datei, die mit der Funktion einlesen_sure() erzeugt wird
#' @param week Die Kalenderwoche, die abgefragt wird. Auswahl zwischen "all", "current", "previous52" und einer Zahl bzw. einer Liste von Zahlen \\
#'   Auswahl:
#'   \itemize{
#'     \item "all" - alle Wochen
#'     \item "current" - nur die aktuelle Woche (letzte abgeschlossene KW)
#'     \item "previous52" - die letzten 52 KW (nur sinnvoll mit year = "all")}
#' @param year Das Jahr, die abgefragt wird. Auswahl zwischen "all", "current", "previous2" und einer Zahl bzw. einer Liste von Zahlen \\
#'   Auswahl:
#'   \itemize{
#'     \item "all" - alle Jahre
#'     \item "current" - aktuelles Jahr (letzte abgeschlossene KW)
#'     \item "previous2" - aktuelles Jahr und das vorhergehende}
#' @param group_by_age Für manche Auswertungen müssen die Variablen nicht nur über KW und Jahr sondern auch über die Altersgruppen aggregiert werden
#'                    Standardmäßig ist dieser Parameter auf FALSE gesetzt. Sollte eine Gruppierung nach Altersgruppen gewünscht sein,
#'                    muss hier group_by_age = TRUE eingegeben werden.
#'
#' @return Aggregierter Datenframe mit ausgewählten Wochen
#' @export
#'
#' @examples
#' aggregate_woche(,"current","current")
#' aggregate_woche(,"all","all")
#' aggregate_woche(,"previous52","all")
#' aggregate_woche(,c(1,2,3),2025)
#'
aggregate_woche <- function(sure_data, week, year, group_by_age = FALSE) {

  #Gruppierung mit oder ohne Altersgruppe
  group_vars <- c("Jahr", "Woche")

  if (group_by_age) {
    group_vars <- c(group_vars, "Altersgruppen")
  }

  # Calculate number of mail sendings received per week and year
  einsendungen_data <- sure_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(einsendungen = n_distinct(einsender), .groups = "drop")


  # correct pathogen variables for multiple infections

  virus_vars <- c("SARS", "InfluA", "InfluB", "RSV",
                  "Rhino", "Parainflu", "Adeno", "Metapneu")

   df <- sure_data %>%
    mutate(
      multi = if_else(rowSums(across(all_of(virus_vars)) == 1, na.rm = TRUE) > 1, 1, 0)
    )

  df <- df %>%
    mutate(
      across(all_of(virus_vars),
        ~ if_else(multi == 1, 0, .),
        .names = "{.col}_crude"
      )
    )

  # Aggregate data on week and year level

  df_summary <- df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      anzahl = n(),
      across(all_of(virus_vars), ~ sum(.x, na.rm = TRUE)),
      across(paste0(virus_vars, "_crude"), ~ sum(.x, na.rm = TRUE)),
      multi = sum(multi, na.rm = TRUE),
      .groups = "drop"
    )

  #Make sure, there are no missing weeks and delete potential remaining erroneous date values

  #create complete list of weeks and years
  if (group_by_age) {
    alle_wochen <- expand.grid(
      Jahr = unique(df_summary$Jahr),
      Woche = 1:52,
      Altersgruppen = unique(df_summary$Altersgruppen)
    )
  } else {
    alle_wochen <- expand.grid(
      Jahr = unique(df_summary$Jahr),
      Woche = 1:52
    )
  }

  # Merge with data frame
  df_summary <- full_join(alle_wochen, df_summary, by = group_vars) %>%
    filter(!(Woche < 4 & Jahr <= 2023)) %>%
    filter(!(Woche>lubridate::isoweek(Sys.Date() - 7) & Jahr >= year(Sys.Date() - 7))) %>%
    filter(!Woche==53) #wir hassen KW 53

  # Merge with number of mail sendings per week and year
  df_summary <- df_summary %>%
    left_join(einsendungen_data, by = group_vars)

  # Calculate moving averages (last 4 weeks)
    vars_ma <- c(
    virus_vars, "anzahl", "multi", "einsendungen",
    paste0(virus_vars, "_crude")
  )

    df_summary <- df_summary %>%
      arrange(Jahr, Woche, .by_group = FALSE) %>%
      group_by(across(all_of(setdiff(group_vars, "Woche")))) %>%
      mutate(
        across(
          all_of(vars_ma),
          ~ . + lag(., 1) + lag(., 2) + lag(., 3),
          .names = "{.col}_ma"
        )
      ) %>%
      ungroup()

    # Calculate combined influenza variable
    df_summary <- df_summary %>%
      mutate(
        influenza = coalesce(InfluA, 0) + coalesce(InfluB, 0),
        influenza_crude = coalesce(InfluA_crude, 0) + coalesce(InfluB_crude, 0),
        influenza_ma = coalesce(InfluA_ma, 0) + coalesce(InfluB_ma, 0),
        influenza_crude_ma = coalesce(InfluA_crude_ma, 0) + coalesce(InfluB_crude_ma, 0)
      )

    #Set filter for year and week based on the parameters year and week

    current_week <- isoweek(Sys.Date() - 7)
    current_year <- year(Sys.Date() - 7)

    if (length(week) == 1 && week=="current") {
      df_summary <- df_summary %>%
        filter(Woche==current_week)
    } else if (length(week) == 1 && week=="all") {
      df_summary <- df_summary
    } else if (length(week) == 1 && week=="previous52") {
      df_summary <- df_summary %>%
        filter((Jahr == current_year & Woche <= current_week)  |  (Jahr == current_year - 1 & Woche > current_week))
    } else if (is.numeric(week)) {
      df_summary <- df_summary %>%
        filter(Woche %in% week)
    } else {
      message("Für 'week' bitte 'current', 'all', 'previous52' oder ein numerisches Element angeben!")
    }


    if (length(year) == 1 && year=="current") {
      df_summary <- df_summary %>%
        filter(Jahr==current_year)
    } else if (length(year) == 1 && year=="all") {
      df_summary <- df_summary
    } else if (length(year) == 1 && year=="previous2") {
      df_summary <- df_summary %>%
        filter(Jahr==current_year | Jahr==year(Sys.Date() - 372))
    } else if (is.numeric(year)) {
      df_summary <- df_summary %>%
        filter(Jahr %in% year)
    } else{
      message("Für 'year' bitte 'current', 'all', 'previous2' oder ein numerisches Element angeben!")
    }

  return(df_summary)
}






