#' import_SurvNet
#'
#' @return A data frame specified columns (hardcoded for now)
#'
#' @export
#'
#' @examples
#' import_SurvNet()
#'
#'
import_SurvNet <- function(){
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
    dbGetQuery(myconn,
               "SELECT DISTINCT [Data].[Version].[Token] AS 'Aktenzeichen' ,(SELECT I.ItemName FROM Meta.Catalogue2Item AS C2I INNER JOIN Meta.Item AS I ON C2I.IdItem = I.IdItem WHERE C2I.IdCatalogue = 1010 AND I.IdIndex = [Data].[Disease71].[ReportingCounty]) AS 'Meldelandkreis' ,[Data].[Disease71].[IdVersion], [Data].[Version].[IdType], [Data].[Version].[CodeRecordOwner] AS 'Eigentuemer', [Data].[Version].[IdRecord] AS 'IdRecord', [Data].[Disease71].[Sex] 'Geschlecht' , [Data].[Disease71].[ReportingDate] AS 'Meldedatum' , [Data].[Disease71].[MunicipalityKey] , [Data].[Disease71].[AgeComputed] , [Data].[Disease71].[StatusHospitalization] AS 'HospitalisierungStatus' , [Data].[Disease71].[StatusDeceased] AS 'VerstorbenStatus' ,ExPOI2.ExpKont1, ExPOI2.ExpSubKont1, ExPOI2.ExpLand1, ExPOI2.ExpBL1, ExPOI2.ExpLK1, ExPOI2.ExpKont2, ExOutInfo.AusbruchInfo_InterneRef FROM [Data].[Version] INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion] LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT) LEFT OUTER JOIN [Meta].[DayTable] DT1110 ON DT1110.IdDaySQL = CAST(CAST([Data].[Disease71].[OnsetOfDisease] AS FLOAT) AS INT) Outer Apply Data.ExpandWithPlaceOfInfections2(Data.Version.IdVersion) ExPOI2 Outer Apply Data.ExpandWithOutbreakInfo ([Data].[Version].[IdVersion]) ExOutInfo WHERE (GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil]) AND ([Data].[Version].[IsActive] = 1) AND ([Data].[Version].[IdRecordType] = 1) AND (([Data].[Version].[IdType] IN (121,157,179,140,138))) AND (((DT1001.WeekYear>=2023)))")
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

