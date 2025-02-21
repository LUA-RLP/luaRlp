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
