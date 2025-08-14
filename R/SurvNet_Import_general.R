

#' Build query
#'
#' @param TagKon
#'
#' @return a query for survNet
#'
#' @export
#'
#' @examples
#' query <- build_query(TagKon=TRUE)
#'
build_query <- function(TagKon=FALSE) {
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
      DT1001.Year"

  from_part <- "
    FROM [Data].[Version]
    INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion]
    LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT)"

  where_part <- "
    WHERE (GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil]) AND
    [Data].[Version].[IsActive] = 1 AND
    [Data].[Version].[IdRecordType] = 1 AND
    [Data].[Disease71].[ReportingState] = 13000007"

  if (TagKon) {
    S_TagKon <- ",
      [Data].[Disease71].[DeceasedDate],
      [Data].[Disease71].[CaseDefCategoryComputed] AS 'Falldefkategorie',
      [Data].[Disease71].[P112Relevant] AS 'P112Relevant_',
      [Data].[Disease71].[StatusPlaceOfInf] AS 'StatusPlaceOfInf_' ,
      (SELECT COUNT(*) FROM [Data].[PlaceOfInfection] WITH (FORCESEEK, INDEX(IX_IdVersion))
            WHERE [Data].[PlaceOfInfection].IdVersion = [Data].[Version].[IdVersion]) AS 'PlaceOfInfections_COUNT',
      [Data].[PlaceOfInfection].[Region] AS 'ID',
      [Data].[Disease71].[StatusPatientSetting] AS 'StatusPatientSetting_',
      ExTransTime.AtGACreated,
      ExTransTime.AtLSCreated,
      [Data].[Version].[ValidFrom] AS 'GültigAb_DMYhs'"

    select_part <- paste0(select_part,S_TagKon)

    F_TagKon <- "
      LEFT OUTER JOIN [Data].[PlaceOfInfection] ON [Data].[Version].[IdVersion] = [Data].[PlaceOfInfection].[IdVersion]
      Outer Apply Data.ExpandWithTrackTimes([Data].[Version].[GuidRecord]) ExTransTime"

    from_part <- paste0(from_part,F_TagKon)

    W_TagKon <- " AND
      [Data].[Version].[IdType] IN (135, 121, 124, 174, 106, 168, 116, 112, 118, 141, 146, 142, 145, 148, 102, 144, 172,
            175, 151, 113, 111, 152, 162, 131, 138, 134, 126, 165, 122, 170, 176, 132, 104, 110, 153, 139, 140, 201, 199,
            157, 115, 200, 156, 155, 147, 159, 179, 114, 123, 204, 205, 207, 208, 211, 212, 213, 210, 206, 80)"

    where_part <- paste0(where_part,W_TagKon)
  }



  query <- paste0(select_part, from_part, where_part)
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
#' import_SurvNet(build_query)
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



















