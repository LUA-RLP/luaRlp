


## Setzen von Optionen-Variablen
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("survnet.dsn"))) {
    options(survnet.dsn = "SurvNet_datenbank")
    message("survnet.dsn ist gesetzt auf: ", get_survnet_dsn(), "\n")
  }
}
