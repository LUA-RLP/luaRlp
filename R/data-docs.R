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




#' Landkreise und Gemeinden in Rheinland-Pfalz
#'
#' The shapes and names of Kreise and Gemeinden in Rheinland Pfalz
#'
#' @docType data
#' @usage data(RLP_geo)
#'
#' @format A list with two geo-objects (Sf - Simple feature collections):
#' \describe{
#'   \item{Kreise}{Landkreise in Rheinland-Pfalz}
#'   \item{Gemeinden}{Gemeinden in Rheinland-Pfalz}
#'   }
#' @source Compiled by Thomas Stelzer and Anja Schoeps
#' @examples
#' data(RLP_geo)
#' head(RLP_geo[["Kreise"]])
#' head(RLP_geo[["Gemeinden"]])
"RLP_geo"


#' Praxisliste
#'
#' Foo
#'
#' @docType data
#' @usage data(Praxislist)
#'
#' @format A list with two geo-objects (Sf - Simple feature collections):
#' \describe{
#'   \item{einsender}{foo}
#'   \item{praxisname}{foo}
#'   }
#' @source Compiled by Anja Schoeps
#' @examples
#' data(Praxisliste)
"Praxisliste"

