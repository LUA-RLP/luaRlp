# Libraries


#pacman::p_load(lubridate, stringr, haven, janitor, tidylog, openxlsx, here, glue, readr)

#' einlesen_sure
#'
#' @param path
#'
#' @return Ein datenframe zur Weiterverwendung
#'
#' @import lubridate
#' @import stringr
#'
#' @export
#'
#' @examples einlesen_sure()
einlesen_sure <- function(path = "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/SURE/Daten/SURE_Export.csv") {

# Import data
  data_import <- data.frame(read.csv(path, fileEncoding = "latin1",sep=';',
                                     colClasses = c("Importdatum" = "Date",
                                                    "Eingangsdatum" = "Date",
                                                    "Probenahmedatum" = "Date")))

  # Change main variable names and add diagnostic lab
  data <- data_import %>%
    rename_with(tolower) %>%
    rename(import=importdatum, eingang=eingangsdatum, abstrichdatum=probenahmedatum)%>%
    mutate(Labor = ifelse(laborbereich == 3161, "Koblenz", ifelse(laborbereich == 3261, "Landau", NA)))

  #------------------------------------------------------------------
  # Data corrections
  #------------------------------------------------------------------

  # Delete duplicates
  nrow(data)
  data <- data %>%
    arrange(id,import) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
  nrow(data)

  # Single case-based corrections for abstrichdatum
  corrections <- data.frame(
    id = c("D0FD403AF58E46899FF7E01434DEC403", "5F501726E44E24A34820D83A00D36699",
           "94BDC5854207B54042357F7D7F601AC4", "C3592BC395F01AD9AC7DF2EBD7F53CF9",
           "6611CCFA133BABC8A1B2844477204E95", "688093C811118ACC9EBA6730CFEF17D6",
           "72D22A7A64153754F8FCCACEB6C9A608", "133656C681C7B5082E4893DBACE7F184",
           "DEE2244BD7A526387D48407494282BE2", "FD85493462F79DC3C48B616A4CE62722",
           "21F54F0B58F65D8A5EA9A4AEF1110B33", "ABFDF607457690B09EEF179BA4A51BD5",
           "D26D9EC81FF2A7A2C222D65F4F129A86", "3496D4C29285E066D2EA0DD290256BE2",
           "D3034DD8FF0FAFD22AA24F10A61FE15F", "1A4D2150EDB34D483B1885B599D0C788",
           "F89BF188A568114507FBACBA2C970633", "065426D48BE71533B963480574ABF352",
           "0A695A9D88651621E06EA8EA8750E934", "0FAB3166D51D97863F9722C6C886714D",
           "12DD7B8174AA5216CAAFB11EC0FA6BC2", "134FF4151791C570C4C85AD8956C2A02",
           "178FA0B56A82D4B21AFB94871C6DB7CD", "2B13A8E35FC9904F7AD539471EEFA222",
           "2D7B56A26839177D4C671FA576467D0D", "475A61BF0F5015B828D6273D4328F877",
           "568C609FFDB4E7334E3354FEBD1CF398", "600CBAD4A35D56411486286FD2A7F59C",
           "7695D323D2141A3404DAA3B6051181AB", "8659F9D40B587B2482E99D92A094872E",
           "AD2EF24285FDBF3A7A0C74D9664B9E30", "AE7094F59013B81261110D4311C91746",
           "C4D64D137E4174A74821DBCDE6D90BE3", "DDFD26E8666ED07D6F102D05DE93C4C4",
           "78E26257262F7ABF5C369F1C26899C95"),
    abstrichdatum2 = as.Date(c("2023-01-25", "2023-02-06",
                               "2023-03-14", "2023-03-13",
                               "2023-03-28", "2023-03-20",
                               "2023-02-27", "2023-12-04",
                               "2023-12-04", "2023-12-05",
                               "2024-01-02", "2024-01-22",
                               "2024-02-05", "2025-01-20",
                               "2025-01-27", "2025-03-06",
                               "2025-02-27", "2024-09-03",
                               "2025-02-17", "2023-05-16",
                               "2023-07-11", "2025-01-03",
                               "2023-06-05", "2023-08-28",
                               "2023-09-11", "2023-09-25",
                               "2025-03-10", "2024-12-02",
                               "2023-11-13", "2025-01-03",
                               "2024-06-03", "2023-12-12",
                               "2024-08-05", "2025-01-06",
                               "2024-04-23"))
  )
  data <- data %>%
    left_join(corrections, by = "id") %>%
    mutate(abstrichdatum = case_when(!is.na(abstrichdatum2) ~ abstrichdatum2, TRUE ~ abstrichdatum))

  # Single case-based replacements for eingang
  data <- data %>%
    mutate(eingang = case_when(id=="DCDB089DF64396D8AB609FCCFDE2941A" ~ as.Date("2023-02-06"), TRUE ~ eingang))

  # Single case-based replacements for angaben
  data <- data %>%
    mutate(angaben = ifelse(id == "CE41AA1E685D99383A75406E36D27857",
                            "symptome=1#seit=2#tage=2#wochen=.#beginn=2#schwere=2#test=1#sars=0#influenza=.#rsv=.", angaben),
           angaben = ifelse(id == "540EA179AB3A112A5CCB34B77A5D2C0C",
                            "1#seit=2#tage=6#wochen=.#beginn=2#schwere=2#test=1#sars=0#influenza=.#rsv=.", angaben),
           angaben = ifelse(id == "ED8B48CAAF129F9BD10CF77264A9C66E",
                            "symptome=1#seit=2#tage=3#wochen=.#beginn=1#schwere=2#test=0#sars=.#influenza=.#rsv=.", angaben),
           angaben = ifelse(id == "23BC757555171DCE3178699DCE53DF0D",
                            "symptome=1#seit=2#tage=4#wochen=.#beginn=1#schwere=2#test=1#sars=0#influenza=.#rsv=.", angaben))

  # Replacements for einsender
  data <- data %>%
    mutate(einsender = ifelse(einsender == "xprdughio", "xprdughob", einsender),
           einsender = ifelse(einsender == "xprerco", "xprscbuco", einsender),
           einsender = ifelse(einsender == "xprschmo", "xprgamo", einsender),
           einsender = ifelse(einsender == "xprgrhi", "xprgrge", einsender))

  # Replacements for invalid dates in abstrichdatum
  data <- data %>%
    mutate(abstrichdatum = case_when(abstrichdatum == as_date("1911-01-01") |
                                       abstrichdatum == as_date("1999-01-01")~ NA, TRUE ~ abstrichdatum))

  # Create a problem list with specific cases for later corrections
  plausibility_check <- data %>%
    mutate(lag_eingang = eingang - abstrichdatum, lag_ergebnis = import - eingang) %>%
    filter(((abstrichdatum > Sys.Date() | abstrichdatum < as_date("2023-01-19")) & !is.na(abstrichdatum)) |
             (is.na(abstrichdatum) & (eingang > Sys.Date() | eingang < as_date("2023-01-19")) & !is.na(eingang)) |
             (lag_eingang>30) |
             (lag_ergebnis>30))
  warning_plausibility <- if (nrow(plausibility_check) > 0) {"Achtung: Der Dataframe enthält fehlerhafte Werte!"} else
  {"Alles in Ordnung: Keine fehlerhaften Werte gefunden."}
  message (warning_plausibility)

  # Delete all rows with remaining implausible values
  data <- data %>%
    filter(!(id %in% plausibility_check$id))


  #------------------------------------------------------------------
  # Create main Variables
  #------------------------------------------------------------------

  # Create a main date variable and extract year, month, week, and day of the week
  data <- data %>%
    mutate(base_datum = coalesce(abstrichdatum, eingang),
           Jahr = year(base_datum),
           Monat = month(base_datum),
           Wochentag = wday(base_datum, week_start = 1),
           Woche = isoweek(base_datum),
    )
  #correct year to match with the calendar week
  data$Jahr <- ifelse(data$Monat==12 & data$Woche==1, data$Jahr+1,
                      ifelse(data$Monat==1 & data$Woche>=52, data$Jahr-1,data$Jahr))

  # Generate test result variables and combinations of these
  data <- data %>%
    mutate(across(starts_with("ergebnis"), ~ str_replace_all(.x, " ", ""))) %>%
    mutate(SARS = ifelse(str_detect(ergebnis1, "positiv"), 1, ifelse(str_detect(ergebnis1, "negativ"), 0, NA)),
           InfluA = ifelse(str_detect(ergebnis2, "positiv"), 1, ifelse(str_detect(ergebnis2, "negativ"), 0, NA)),
           InfluB = ifelse(str_detect(ergebnis3, "positiv"), 1, ifelse(str_detect(ergebnis3, "negativ"), 0, NA)),
           influenza = ifelse(InfluA == 1 | InfluB == 1, 1, ifelse(InfluA == 0 & InfluB == 0, 0, NA)),
           RSV = ifelse(str_detect(ergebnis4, "positiv"), 1, ifelse(str_detect(ergebnis4, "negativ"), 0, NA))) %>%
    rowwise() %>%
    mutate(any = ifelse(sum(SARS, InfluA, InfluB, RSV, na.rm = TRUE) >= 1, 1, 0),
           multi = ifelse(sum(SARS, InfluA, InfluB, RSV, na.rm = TRUE) > 1, 1, 0))

  # Age of patient (birthday is set to 15th of respective month)
  data$birthday <- as_date(ifelse(is.na(data$geburtsjahr) | is.na(data$geburtsmonat),
                           NA, paste0(data$geburtsjahr, "-",
                                             data$geburtsmonat, "-15")))

  data <- data %>%
    mutate(Alter = as.numeric(base_datum-birthday)/365.25,
           Altersgruppen3 = cut(Alter, breaks = c(0, 2, 6, 18, 40, 60, 120),
                                labels = c("<2 Jahre", "2-5 Jahre", "6-17 Jahre", "18-39 Jahre", "40-59 Jahre", "60 Jahre und älter"),
                                right = FALSE))

  # add information about the practitioners
  data <- left_join(data, Praxisliste, by = "einsender")

  # Extract additional information summarized in the variable "angaben"
  data <- data %>%
    mutate(
      symptome = ifelse(grepl("symptome=", angaben), sub(".*symptome=(.*?)#seit=.*", "\\1", angaben), NA),
      beginn = ifelse(grepl("beginn=", angaben), sub(".*beginn=(.*?)#schwere=.*", "\\1", angaben), NA),
      schwere = ifelse(grepl("schwere=", angaben), sub(".*schwere=(.*?)#test=.*", "\\1", angaben), NA),
      test = ifelse(grepl("test=", angaben), sub(".*test=(.*?)#sars=.*", "\\1", angaben), NA),
      sarstest = ifelse(grepl("sars=", angaben), sub(".*sars=(.*?)#influenza=.*", "\\1", angaben), NA),
      influenzatest = ifelse(grepl("influenza=", angaben), sub(".*influenza=(.*?)#rsv=.*", "\\1", angaben), NA),
      rsvtest = ifelse(grepl("rsv=", angaben), substr(sub(".*rsv=(.).*", "\\1", angaben), 1, 1), NA),
      seit1 = ifelse(grepl("seit=", angaben), sub(".*seit=(.*?)#tage=.*", "\\1", angaben), NA),
      tage = ifelse(grepl("tage=", angaben), sub(".*tage=(.*?)#wochen=.*", "\\1", angaben), NA),
      wochen = ifelse(grepl("wochen=", angaben), sub(".*wochen=(.*?)#beginn=.*", "\\1", angaben), NA)) %>%
    mutate(across(c(symptome, beginn, schwere, test, sarstest, influenzatest, rsvtest, seit1, tage, wochen),
                  ~ ifelse(. == "" | . == ".", NA, .))) %>%
    mutate(across(c(symptome, beginn, schwere, test, sarstest, influenzatest, rsvtest, seit1, tage, wochen),
                  ~ ifelse(grepl("[A-z]", .), "", gsub("\\.", "", .))))

  # Specific changes for the variable schwere, which will be used in the subsequent analysis
  data %>%
    mutate(schwere=ifelse(schwere==0, NA, schwere)) %>%
    mutate(schwere = na_if(trimws(schwere), ""))
 }



# Aggregate data frame by calendar week and year and calculate additional variables for presentation

#' Title
#'
#' @param sure_data
#'
#' @return Aggregierter Datenframe mit ausgewählten Wochen
#' @export
#'
#' @examples Zusammenfassung_Berichtswoche()
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
    filter(!(Woche>lubridate::isoweek(Sys.Date() - 7) & Jahr >= year(Sys.Date() - 7)))


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

  df_summary %>%
    filter(Woche==week)%>%
    filter(Jahr==year)

}











  #------------------------------------------------------------------
  # Create objects for dates
  #------------------------------------------------------------------

  # # Current and previous weeks (reporting week is always last week)
  # current_date <- Sys.Date()
  # #calendar weeks
  # this_week <- lubridate::isoweek(current_date)
  # current_week <- lubridate::isoweek(current_date - 7)
  # week_2ago <- lubridate::isoweek(current_date - 14)
  # week_3ago <- lubridate::isoweek(current_date - 21)
  # week_4ago <- lubridate::isoweek(current_date - 28)
  # #years
  # current_year <- year(current_date - 7)
  # prev6years <- current_year - 6


# Select observations of reporting week (current week) and add up all values into one row for text elements

#' Title
#'
#' @param sure_data
#'
#' @return Einzeiliger Datenframe mit aktuellen Werten für die Berichte
#' @export
#'
#' @examples Zusammenfassung_Berichtswoche()
Zusammenfassung_Berichtswoche <- function(sure_data) {
  df_summary <- sure_data %>%
    filter(Woche == lubridate::isoweek(Sys.Date() - 7)) %>%
    filter(Jahr == year(Sys.Date() - 7)) %>%
    group_by(Woche, Jahr) %>%
    summarize(
      anzahl = n(),
      SARS = sum(SARS, na.rm = TRUE),
      InfluA = sum(InfluA, na.rm = TRUE),
      InfluB = sum(InfluB, na.rm = TRUE),
      influenza = sum(influenza, na.rm = TRUE),
      RSV = sum(RSV, na.rm = TRUE),
      multi = sum(multi, na.rm = TRUE)
    )

  # Calculate additional numbers
  df_summary %>%
    mutate(pos_real = (SARS + influenza + RSV) - multi,
           prop_real = pos_real / anzahl,
           Numerus_anzahl = ifelse(anzahl == 1, "Probe", "Proben"),
           Numerus_SARS = ifelse(SARS == 1, "Nachweis", "Nachweise"),
           Numerus_influenza = ifelse(influenza == 1, "Nachweis", "Nachweise"),
           Numerus_RSV = ifelse(RSV == 1, "Nachweis", "Nachweise"),
           Numerus_multi = ifelse(multi == 1, "Probe", "Proben")
    )
}


#
# text_df <- sure_summary %>%
#   mutate(anzahl_T = ifelse(anzahl == 1, glue("{df_text$anzahl} Probe"), glue("{df_text$anzahl} Proben")),
#          SARS_T = ifelse(SARS == 1, glue("{df_text$SARS} Nachweis"), glue("{df_text$SARS} Nachweise")),
#          influenza_T = ifelse(influenza == 1, glue("{df_influenza$anzahl} Nachweis"), glue("{df_text$influenza} Nachweise")),
#          RSV_T = ifelse(RSV == 1, glue("{df_text$RSV} Nachweis"), glue("{df_text$RSV} Nachweise")),
#          multi_T = ifelse(multi == 1, glue("{df_text$multi} Probe"), glue("{df_text$multi} Proben"))
#   )
#
#
# Text <- paste0("In Kalenderwoche ",df_text$Woche, " wurden insgesamt ",df_text$anzahl_T,
#                " von den rheinland-pfälzischen Arztpraxen eingesandt.  Davon wurde bei ",df_text$pos_real,
#                " (",round(df_text$prop_real * 100, 0),
#                "%) mindestens eine der folgenden Infektionen nachgewiesen: SARS-CoV-2 (", df_text$SARS_T,"); Influenza (",
#                df_text$influenza_T,"); RSV (",df_text$RSV_T,"). Insgesamt wurde in ",
#                df_text$multi_T, " mehr als ein Erreger nachgewiesen.")
#
# cat(Text)
#
# }
