

#' SurvNet_Abfrage
#'
#' @title Funktion zum Aufbau eines Abfragetexts (SQL-Language) für die SurvNet-Abfrage
#' @description Es können spezielle vorgefertigte Abfragen erstellt werden.//
#'              Zusätzlich kann die Periode der Abfrage unabhängig davon festgelegt werden.
#'              Das Thema Tageskontrolle wird gewöhnlich mit der Periode last2weeks kombiniert
#'              Das Thema IMS_RIDOM wird gewöhnlich mit der Periode IMS_since_2023 kombiniert.
#' @param Thema Auswahl zwischen alle, Tageskontrolle, VHF, WBK, Arboviren, ZoonotischeINV, SerovarSAL,
#'              Influenza, COVID19, RSV, IMS, EBC und LEG
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
      [Data].[Version].[CodeRecordOwner] AS 'Eigentuemer_ID',
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
#' @title Funktion zur Abfrage von Daten aus der SurvNet-Datenbank
#' @description Das Hauptargument für diese Funktion "query" entsteht durch die Funktion build_query().
#'              Zusätzlich dazu muss die Datenquelle "dsn" festgelegt sein

#' @param query Dieser Wert wird mit der Funktion build_query() erzeugt. Alternativ könnte hier ein beliebiger Abfragecode
#'              im SQL-Format übergeben werden.
#' @param dsn Hiermit wird die SQL-Datenquelle festgelegt. Als Standard ist bereits "SurvNet_datenbank" festgelegt.
#'            Sollte die Datenquelle auf Deinem Rechner einen anderen Namen tragen, muss er an dieser Stelle
#'            für den Parameter dsn übergeben werden.
#'
#' @return  Die Ausgabe ist ein aus der SurvNet-Datenbank ausgelesener Datensatz, auf den bspw. im nächsten Schritt
#'          die Funktion add_values() angewendet werden kann, um die Daten lesbar/interpretierbar zu machen.
#'
#' @import odbc
#'
#' @seealso
#' \code{\link{ get_survnet_dsn}} zum Abfragen der aktuellen SurvNet-dsn Konfiguration. \cr
#' \code{\link{autodetect_survnet_dsn}} zum automatischen Finden und Setzen eines passenden DSN.
#'
#' @export
#'
#' @examples
#' SurvNet_data <- import_SurvNet(build_query(Thema="Tageskontrolle"))
#'
import_SurvNet <- function(query,  dsn = get_survnet_dsn()){
  if (is.null(dsn)) {
    dsn <- odbc::odbcListDataSources()$name
  }
  message("SurvNet Datenbank ist: ", dsn, "\n")
  if (length(dsn) == 0) {
    stop("Keine Datenquelle gefunden. Bitte konfiguriere ODBC.")
  }
  if (length(dsn) > 1) {
    stop(paste("Es wurden mehrere mögliche Datenbanken",
               "detektiert, WÄHLEN SIE eine der Quellen",
               paste(dsn, collapse = ", "),
               "und gebe sie diese als dsn = \"Option\" an.")
    )
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






