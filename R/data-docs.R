#' Gesundheitsaemter and their IDs: Eigentuemer
#'
#' Gesundheitsaemter "own" (German: "sind die Eigetuemer") of the data in
#' SurvNet, this dataset contains information on how we use them assigning
#' IDs to and from them.
#' @docType data
#' @usage data(Eigentuemer)
#'
#' @format A tibble with 24 rows and 3 variables:
#' \describe{
#'   \item{Eigentuemer}{ID for the Gesunheitsamt}
#'   \item{Gesundheitsam}{Name (Ortsname) of the Gesundheitsamt}
#'   \item{Kuerzel}{Short abbreviation for the Gesundheitsamt derived from
#'   location identifier used on car license plates}
#' }
#' @source Compiled by Anja Schoeps
#' @examples
#' data(Eigentuemer)
#' head(Eigentuemer)
"Eigentuemer"


#' Landkreise in Rheinland-Pfalz: Centroids of Latitude and Longitude
#'
#' Centroids (thier latitude and longitude) of the Landkreise in Rheinland-Pfalz
#' We might remove this dataset at some point and include code to generate the
#' centroids from geographic shapes. We should do so when we use the shapes for
#' other purposes.
#'
#' @docType data
#' @usage data(LKLatLong)
#'
#' @format A tibble with 36 rows and 2 variables:
#' \describe{
#'   \item{Meldekreis}{Name of the Landkreis (starting LK for Landkreis)}
#'   \item{Lat/Long of Isolation}{The latitude and Longitude of the Landkreis'
#'   centroid separated by a comma}
#'   }
#' @source Compiled by Thomas Stelzer and Anja Schoeps
#' @examples
#' data(LKLatLong)
#' head(LKLatLong)
"LKLatLong"
