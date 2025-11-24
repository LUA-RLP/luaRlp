

#' SurvNet_Abfrage
#'
#' @title Funktion zum Aufbau eines Abfragetexts (SQL-Language) für die SurvNet-Abfrage
#' @description Es können spezielle vorgefertigte Abfragen erstellt werden.//
#'              Zusätzlich kann die Periode der Abfrage unabhängig davon festgelegt werden.
#'              Das Thema Tageskontrolle wird gewöhnlich mit der Periode last2weeks kombiniert
#'              Das Thmea IMS_RIDOM wird gewöhnlich mit der Periode IMS_since_2023 kombiniert.
#' @param Thema Auswahl zwischen alle, Tageskontrolle, VHF, WBK, Arboviren, ZoonotischeINV, SerovarSAL, Influenza, COVID19, RSV, IMS_RIDOM
#' @param Periode Auswahl zwischen alle, last2weeks, thisyear, last2years, last5years, last10years, IMS_since_2023
#'
#' @return Ausgabe wird in der Funktion import_SurvNet verwendet.
#'
#' @export
#'
#' @examples
#' query <- build_query(Thema="Tageskontrolle")
#'
build_query <- function(Thema=NULL, Periode=c("alle","last2weeks","thisyear","last2years","last5years","last10years","IMS_since_2023")) {

  select_part <- "
    SELECT DISTINCT
      [Data].[Disease71].[IdVersion],
      [Data].[Version].[Token] AS 'Aktenzeichen',
      [Data].[Disease71].[ReportingCounty] AS 'LK_ID',
      [Data].[Version].[IdType],
      [Data].[Disease71].[ReferenceDefComputed] AS 'Referenzdefinition',
      [Data].[Version].[CodeRecordOwner] AS 'Eigentuemer',
      [Data].[Version].[IdRecord] AS 'IdRecord',
      [Data].[Disease71].[Sex] AS 'Geschlecht',
      [Data].[Disease71].[ReportingDate] AS 'Meldedatum',
      [Data].[Disease71].[AgeComputed],
      [Data].[Disease71].[StatusHospitalization] AS 'HospitalisierungStatus',
      [Data].[Disease71].[StatusDeceased] AS 'VerstorbenStatus',
      [Data].[Disease71].[OnsetOfDisease] AS 'Erkrankungsbeginn',
      DT1001.Week,
      DT1001.WeekYear"

  from_part <- "
    FROM [Data].[Version]
    INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion]
    LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT)"

  where_part <- "
    WHERE (GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil]) AND
    [Data].[Version].[IsActive] = 1 AND
    [Data].[Version].[IdRecordType] = 1 AND
    [Data].[Disease71].[ReportingState] = 13000007"

  # Thema-spezifische Erweiterungen -----------------------------------------
  if (!is.null(Thema)) {
    message("Thema ist: ", Thema)

    filtered <- themes_list %>%
      filter(Thema == !!Thema)

    if (nrow(filtered) == 0) {
      stop(paste0("❌ Das angegebene Thema '", Thema, "' wurde in themes_list nicht gefunden.\n",
                  "Verfügbare Themen sind: ",
                  paste(unique(themes_list$Thema), collapse = ", "), "."))
    }

    # SQL-Bestandteile zusammenbauen
    select2 <- filtered %>% pull(Select) %>% paste(collapse = " ") %>% gsub("\n", "", .)
    from2   <- filtered %>% pull(From)   %>% paste(collapse = " ") %>% gsub("\n", "", .)
    where2  <- filtered %>% pull(Where)  %>% paste(collapse = " ") %>% gsub("\n", "", .)

    if (nchar(select2) > 0) select_part <- paste0(select_part, ", ", select2)
    if (nchar(from2)   > 0) from_part   <- paste0(from_part, " ", from2)
    if (nchar(where2)  > 0) where_part  <- paste0(where_part, " AND ", where2)

  } else {
    message("Kein Thema angegeben. Standard-Abfrage")
    if (exists("merge_format", envir = .GlobalEnv)) rm(merge_format, envir = .GlobalEnv)
  }

  # Periode-spezifische Erweiterungen -----------------------------------------

  Periode <- match.arg(Periode)
  message("Periode ist ",Periode)

  thisweek <- isoweek(Sys.Date())
  lastweek <- isoweek(Sys.Date()-7)
  thisyear <- isoyear(Sys.Date())
  lastweekyear <- isoyear(Sys.Date()-7)
  lastyear <- thisyear-1
  prev5years <- thisyear-4
  prev10years <- thisyear-9

  if (Periode=="last2weeks") {
    where3 <- paste0(" AND ((Week=", thisweek, " AND Year=", thisyear, ") OR (Week=", lastweek, " AND Year=", lastweekyear, "))")
    where_part <- paste0(where_part,where3)
  }

  if (Periode=="thisyear") {
    where3 <- paste0(" AND Year=", thisyear)
    where_part <- paste0(where_part,where3)
  }

  if (Periode=="last2years") {
    where3 <- paste0(" AND (Year BETWEEN ", lastyear, " AND ", thisyear, ")")
    where_part <- paste0(where_part,where3)
  }

  if (Periode=="last5years") {
    where3 <- paste0(" AND (Year BETWEEN ", prev5years, " AND ", thisyear, ")")
    where_part <- paste0(where_part,where3)
  }

  if (Periode=="last10years") {
    where3 <- paste0(" AND (Year BETWEEN ", prev10years, " AND ", thisyear, ")")
    where_part <- paste0(where_part,where3)
  }

  if (Periode=="IMS_since_2023") {
    where3 <- " AND Year>=2023"
    where_part <- paste0(where_part,where3)
  }

  # Finaler Query -----------------------------------------------------------
  query <- paste0(select_part, from_part, where_part)
  query <- gsub("[\r\n]", " ", query)
  query <- gsub("\\s+", " ", query)

  return(query)

}




#' import_SurvNet
#'
#' @return A data frame specified columns (hardcoded for now)
#'
#' @export
#'
#' @import odbc
#' @examples
#' import_SurvNet(build_query())
#'
#'
import_SurvNet <- function(query){
  dsn <- odbcListDataSources()$name
  if (length(dsn) != 1) {
    if (length(dsn) > 1) {
      stop(paste("Multiple potential SurvNet data sources detected.",
                 "Please select one of:", paste(dsn, collapse = ", ")))
    } else {
      stop("No data source detected. Please configure ODBC.")
    }
  }
  # Connect if exactly one DSN is found
  myconn <- dbConnect(odbc(), dsn = dsn)
  # Try running the query and ensure the connection is closed
  data <- tryCatch({
    dbGetQuery(myconn, query)
  },
  error = function(e) {
    message("Error: Failed to execute the SQL query. Check your syntax and connection.")
    message("Details: ", e$message)
    return(NULL)  # Return NULL if an error occurs
  },
  finally = {
    dbDisconnect(myconn)
  })

  return(data)
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

  # Merge immer
  df <- merge(df, geo_standards, all.x=TRUE, by = "LK_ID")
  df <- merge(df, IdType_Datensatzkategorie, all.x=TRUE, by = "IdType")

  # Merge nur, wenn Region_ID existiert
  if ("Region_ID" %in% names(df)) {
    df <- merge(df, Regionen, all.x=TRUE, by = "Region_ID")
    df$Region_ID <- NULL
    # Erkennen, welche IdRecords mehrfach vorkommen
    dups <- duplicated(df$IdRecord) | duplicated(df$IdRecord, fromLast = TRUE)
    # Für diese IDs den Wert ersetzen
    df$Angabe_zum_Expositionsort[df$IdRecord %in% df$IdRecord[dups]] <- "mehrere Expositionsorte"
    # Danach nur erste Zeile pro ID behalten
    df <- df[!duplicated(df$IdRecord), ]
  }

  # Merge nur, wenn PathogenABV existiert
  if ("PathogenABV_ID" %in% names(df)) {
    df <- merge(df, PathogenABV, all.x=TRUE, by = "PathogenABV_ID")
    #df$PathogenABV_ID <- NULL
  }

  #Unnötige oder doppelte Spalten entfernen
  spalten_entfernen <- c("Eigentuemer", "AtGACreated", "AtLSCreated", "GültigAb_DMYhs", "IdType", "LK_ID", "Kürzel", "PlaceOfInf_")
  df <- df[, !(names(df) %in% spalten_entfernen)]

  return(df)


}



