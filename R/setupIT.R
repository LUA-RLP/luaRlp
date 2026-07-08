# Hier sammeln wir Funktionen, die für die Interaktion mit unserer IT und für
# das Setup unserer R-installation am LUA RLP wichtig sind.
#
# Viele dieser Funktionen werden beim laden des LUA RLP Paketes automatisch
# ausgeführt
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Ermittelt den konfigurierten SurvNet-ODBC-DSN
#'
#' Gibt den Namen der ODBC-Datenquelle (DSN) zurück, die für den Zugriff auf die
#' SurvNet-Datenbank verwendet wird. Der DSN wird über die globale R-Option
#' \code{"survnet.dsn"} konfiguriert.
#'
#' Falls die Option nicht gesetzt ist, bricht die Funktion mit einer
#' aussagekräftigen Fehlermeldung ab.
#'
#' @details
#' Beim Laden des Pakets \pkg{luaRlp} wird – sofern die Option noch nicht gesetzt
#' ist – ein Standardwert (\code{"SurvNet_datenbank"}) gesetzt. Dieser Standard
#' kann jederzeit durch den Benutzer überschrieben werden, z.\,B. für Test- oder
#' Produktivumgebungen.
#'
#' @return
#' Ein Zeichenstring der Länge 1 mit dem Namen des ODBC-DSN.
#'
#' @examples
#' \dontrun{
#' # Verwendung des Standard-DSN (falls auf dem System vorhanden)
#' get_survnet_dsn()
#'
#' # Explizites Überschreiben des DSN
#' options(survnet.dsn = "SurvNetDB_Test")
#' get_survnet_dsn()
#' }
#'
#' @seealso
#' \code{\link{autodetect_survnet_dsn}} zum automatischen Finden und Setzen eines passenden DSN. \cr
#' \code{\link[base]{options}}
#'
#' @export
get_survnet_dsn <- function() {
  dsn <- getOption("survnet.dsn")
  if (is.null(dsn)) {
    stop(
      "Option 'survnet.dsn' ist nicht gesetzt.\n",
      "Bitte setze sie mit options(survnet.dsn = \"<DSN>\").",
      call. = FALSE
    )
  }
  dsn
}



#' Automatisches Erkennen und Setzen des SurvNet-ODBC-DSN
#'
#' Sucht in den auf dem System verfügbaren ODBC-Datenquellen (DSNs) nach einem
#' Eintrag, der zu SurvNet passt, und setzt anschließend die globale R-Option
#' \code{"survnet.dsn"} auf den gefundenen DSN-Namen.
#'
#' Standardmäßig wird per regulärem Ausdruck (case-insensitive) nach
#' \code{"surv.*net"} gesucht.
#'
#' @details
#' Das automatische Erkennen ist bewusst als **explizite** Hilfsfunktion
#' implementiert (kein stilles „Guessing“ beim Laden des Pakets), damit das
#' Verhalten nachvollziehbar bleibt – insbesondere wenn mehrere DSNs in Frage
#' kommen (z.\,B. Test/Prod).
#'
#' Verhalten:
#' \itemize{
#'   \item \strong{0 Treffer}: Gibt \code{NULL} zurück und gibt eine Meldung aus.
#'   \item \strong{1 Treffer}: Setzt \code{options(survnet.dsn = "<Treffer>")} und gibt den DSN zurück.
#'   \item \strong{>1 Treffer}: Bricht mit Fehler ab und listet die Treffer, damit man manuell wählen kann.
#' }
#'
#' @param pattern Regulärer Ausdruck zur Suche in den DSN-Namen.
#'
#' @return
#' Bei genau einem Treffer: Zeichenstring (DSN-Name). Bei 0 Treffern:
#' \code{NULL} (invisibly). Bei mehreren Treffern wird ein Fehler geworfen.
#'
#' @examples
#' \dontrun{
#' # automatische Erkennung mit Default-Pattern
#' autodetect_survnet_dsn()
#'
#' # eigenes Pattern (z.B. abweichende Namenskonvention)
#' autodetect_survnet_dsn(pattern = "^SurvNet")
#'
#' # danach kann der DSN z.B. so abgefragt werden:
#' get_survnet_dsn()
#' }
#'
#' @seealso
#' \code{\link{get_survnet_dsn}} zur Abfrage des aktuell konfigurierten DSN. \cr
#' \code{\link[odbc]{odbcListDataSources}} für die zugrundeliegende DSN-Liste. \cr
#' \code{\link[base]{options}} zum manuellen Setzen der Option.
#'
#' @export
autodetect_survnet_dsn <- function(pattern = "surv.*net") {
  dsns <- odbc::odbcListDataSources()$name
  hits <- dsns[grepl(pattern, dsns, ignore.case = TRUE)]

  if (length(hits) == 0) {
    message("❌ Kein passender SurvNet-DSN gefunden (Pattern: ", pattern, ").")
    return(invisible(NULL))
  }

  if (length(hits) > 1) {
    message("⚠️ Mehrere mögliche DSNs gefunden:")
    message(paste(" -", hits))
    stop("Bitte DSN manuell auswählen und mit options(survnet.dsn = \"<DSN>\") setzen.", call. = FALSE)
  }

  options(survnet.dsn = hits[[1]])
  message("✅ SurvNet-DSN gesetzt auf: ", hits[[1]])

  invisible(hits[[1]])
}




#' lua_is_online Überprüfe ob R online-Zugang hat
#'
#' Die Funktion checkt ob innerhalb einer gewissen Latenzzeit (timeout) eine
#' Website erreicht werden kann. Dies ist besonders nützlich um bei
#' Fehlermeldugnen während der Paketinstallation zu prüfen, ob die Proxy-
#' Einstellungen in R korrekt sind und eine Netzwerkverbindung zulassen
#'
#' @param timeout Latenzzeit
#'
#' @return logical indicating whether you're online
#'
#' @export
#'
#' @examples lua_is_online()
#'
lua_is_online <- function(timeout = 15, url = "http://www.google.com"){
  tryCatch({
    setTimeLimit(elapsed = timeout, transient = TRUE)  # Enforce timeout
    con <- url(url, "rb")
    close(con)
    message("\033[32m ✅",  "Netzwerkverbindung vorhanden!")
    TRUE
  },
  error = function(e) FALSE, finally = setTimeLimit())
}


# .onLoad <- function(libname, pkgname) {
#   # Code to execute when the package is loaded
#   packageStartupMessage(
#     "\033[36m⚙️",
#     "Laden der R Kofiguration f\u00fcr das LUA RLP mittels Paket ",
#     pkgname, "! \033[36m⚙️")
#   if (lua_is_online()) {
#     packageStartupMessage("\033[32m ✅",  "Netzwerkverbindung vorhanden!")
#     } else {
#       packageStartupMessage(
#         "\033[31m❌ ",
#         "Netzwerkverbindung konnte (mit manuellen proxy ",
#         "setings) nicht hergestellt werden",
#         "R nur eingeschränkt nutzbar!")
#     }
#   if(R.Version()$version.string == "R version 4.4.0 (2024-04-24 ucrt)") {
#     packageStartupMessage(
#       "\033[32m ✅",
#       "Wir nutzen ", R.Version()$version.string, ", ",
#       "\"",R.Version()$nickname, "\", ",
#       "unser Standard am LUA RLP!")
#   } else{
#     packageStartupMessage(
#       "\033[31m❌ ",
#       "Diese Installation nutzt ",
#       R.Version()$version.string, ", ",
#       "\" ", R.Version()$nickname,  "\", ",
#       "NON-STANDARD am LUA RLP!")
#   }
#   packageStartupMessage("\033[31m💉\033[34m📊 ",
#                         "Viel Spaß bei wichtigen Amtsaufgaben in R!",
#                         " \033[35m🦠\033[32m🏥")
# }




#' lua_network_speedtest()
#'
#' @param net_path Path on the Network you want to test. Defaults to a path on O:
#' @param loc_path Local path. Defaults to a path on your user's desktop
#'
#' @returns writes speed estimates to console
#' @export
#'
#' @examples lua_network_speedtest()
lua_network_speedtest <- function(net_path="O:/Abteilung Humanmedizin (AHM)/Referat 32/R_scratch/R_IO_test_network.txt",
                                  loc_path=file.path(Sys.getenv("USERPROFILE"), "Desktop", "R_IO_test_local.txt")
) {

  ## Pfade festlegen
  local_path <- loc_path
  network_path <- net_path

  # Testdaten erzeugen (10 MB Text)
  test_data <- paste(rep("ABCDEFGHIKLMNOPQRSTUVWXYZ0123456789", 300000), collapse = "\n")

  # Schreiben testen
  cat("### WRITE SPEED ###\n")

  local_write <- system.time(
    writeLines(test_data, local_path)
  )
  cat("Local write time:   ", local_write["elapsed"], "sec on", local_path, "\n")

  network_write <- system.time(
    writeLines(test_data, network_path)
  )
  cat("Network write time: ", network_write["elapsed"], "sec on",
      network_path, "\n\n")

  # Lesen testen
  cat("### READ SPEED ###\n")

  local_read <- system.time(
    readLines(local_path, warn = FALSE)
  )
  cat("Local read time:    ", local_read["elapsed"], "sec on", local_path, "\n")

  network_read <- system.time(
    readLines(network_path, warn = FALSE)
  )
  cat("Network read time:  ", network_read["elapsed"], "sec on",
      network_path, "\n\n")

}


#' check_r_storage_paths
#'
#' @returns writes R path configuration to console
#' @export
#'
#' @examples check_r_storage_paths()
check_r_storage_paths <- function() {
  cat("==== R STORAGE PATH DIAGNOSTICS ====\n\n")

  # 1. R Library paths
  cat("📦 .libPaths():\n")
  print(.libPaths())
  cat("\n")

  # Check for network paths
  network_libs <- .libPaths()[grepl("^O:|^\\\\\\\\", .libPaths(), ignore.case = TRUE)]
  if(length(network_libs) > 0) {
    cat("⚠️  WARNUNG: Einige Libraries liegen auf dem Netzlaufwerk:\n")
    print(network_libs)
    cat("→ Pakete dort laden/compilieren ist *massiv* langsam!\n\n")
  } else cat("✅ Keine Library-Pfade auf einem Netzlaufwerk gefunden.\n\n")

  # 2. R Temp directory
  cat("🧪 Temp directory (tempdir()):\n")
  print(tempdir())
  cat("\n")

  if(grepl("^O:|^\\\\\\\\", tempdir(), ignore.case = TRUE)) {
    cat("⚠️  WARNUNG: tempdir() liegt auf dem Netzlaufwerk!\n",
        "→ Das verlangsamt *jedes* Paket (inkl. devtools::load_all()).\n\n")
  } else cat("✅ tempdir() ist lokal.\n\n")


  # 3. R HOME
  cat("🏠 R_HOME:\n")
  print(R.home())
  cat("\n")

  # 4. User / system config files
  cat("⚙️  Nutzer-Konfigurationen:\n")
  user_profile <- path.expand("~/.Rprofile")
  user_renviron <- path.expand("~/.Renviron")
  site_profile <- file.path(R.home("etc"), "Rprofile.site")
  site_renviron <- file.path(R.home("etc"), "Renviron.site")

  paths <- data.frame(
    file = c("User .Rprofile", "User .Renviron", "Site Rprofile", "Site Renviron"),
    exists = file.exists(c(user_profile, user_renviron, site_profile, site_renviron)),
    location = c(user_profile, user_renviron, site_profile, site_renviron)
  )
  print(paths)
  cat("\n")

  # 5. devtools project root
  if (requireNamespace("devtools", quietly = TRUE)) {
    cat("🧱 devtools::as.package() Projektpfad (falls in Projekt ausgeführt):\n")
    try(print(devtools::as.package(".")$path), silent = TRUE)
    cat("\n")
  }

  cat("==== DONE ====\n")
}



