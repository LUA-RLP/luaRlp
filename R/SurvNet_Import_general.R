

#' SurvNet_Abfrage
#'
#' @title Funktion zum Aufbau eines Abfragetexts (SQL-Language) für die SurvNet-Abfrage
#' @description Es können spezielle vorgefertigte Abfragen erstellt werden.//
#'              Zusätzlich kann die Periode der Abfrage unabhängig davon festgelegt werden.
#' @param Thema Auswahl zwischen alle, Tageskontrolle, VHF, WBK, Arboviren, ZoonotischeINV, SerovarSAL, Influenza, COVID19, RSV
#' @param Periode
#'
#' @return Ausgabe wird in der Funktion import_SurvNet_general verwendet.
#'
#' @export
#'
#' @examples
#' query <- build_query(Thema="Tageskontrolle")
#'
build_query <- function(Thema=NULL, Periode=c("alle","last2weeks","thisyear","last2years","last5years","last10years")) {

  select_part <- "
    SELECT DISTINCT
      [Data].[Disease71].[IdVersion],
      [Data].[Version].[Token] AS 'Aktenzeichen',
      (SELECT I.ItemName FROM Meta.Catalogue2Item AS C2I INNER JOIN Meta.Item AS I ON C2I.IdItem = I.IdItem
              WHERE C2I.IdCatalogue = 1010 AND I.IdIndex = [Data].[Disease71].[ReportingCounty]) AS 'Meldelandkreis',
      [Data].[Version].[IdType],
      [Data].[Disease71].[ReferenceDefComputed] AS 'Referenzdefinition',
      [Data].[Version].[CodeRecordOwner] AS 'Eigentuemer',
      [Data].[Version].[IdRecord] AS 'IdRecord',
      [Data].[Disease71].[Sex] AS 'Geschlecht',
      [Data].[Disease71].[ReportingDate] AS 'Meldedatum',
      [Data].[Disease71].[AgeComputed],
      (CASE [Data].[Disease71].[StatusHospitalization] WHEN 0 THEN '-nicht erhoben-' WHEN 10 THEN 'Nein' WHEN 20 THEN 'Ja' ELSE 'u' END)
             AS 'HospitalisierungStatus',
      (CASE [Data].[Disease71].[StatusDeceased] WHEN 10 THEN 'Nein' WHEN 20 THEN 'Ja' ELSE 'u' END) AS 'VerstorbenStatus',
      [Data].[Disease71].[OnsetOfDisease] AS 'Erkrankungsbeginn',
      DT1001.Week,
      DT1001.Year,
      DT1001.WeekYear"

  from_part <- "
    FROM [Data].[Version]
    INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion]
    LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT)"

  where_part <- "
    WHERE (GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil]) AND
    [Data].[Version].[IsActive] = 1 AND
    [Data].[Version].[IdRecordType] = 1 AND
    [Data].[Disease71].[ReportingState] = 13000007 AND Year=2025"

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
  lastweeksyear <- isoyear(Sys.Date()-7)
  lastyear <- thisyear-1
  prev5years <- thisyear-4
  prev10years <- thisyear-9

  if (Periode=="thisyear") {
    where3 <- paste0(" AND Year=", thisyear)
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
#' import_SurvNet_general(build_query())
#'
#'
import_SurvNet_general <- function(query){
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

