#' Paketoptionen von luaRlp
#'
#' Das Paket \pkg{luaRlp} verwendet die folgenden globalen Optionen:
#'
#' \describe{
#'   \item{\code{survnet.dsn}}{
#'     Name der ODBC-Datenquelle (DSN) für die SurvNet-Datenbank.
#'     Der Standardwert \code{"SurvNet_datenbank"} wird beim Laden des Pakets
#'     gesetzt, sofern die Option noch nicht existiert.
#'   }
#' }
#'
#' Die Optionen können vom Benutzer jederzeit mit
#' \code{\link[base]{options}} oder \code{\link{autodetect_survnet_dsn}}
#' zum automatischen Finden und Setzen eines passenden DSN überschrieben werden.
#'
#' @name luaRlp-package-options
#' @keywords internal
NULL
