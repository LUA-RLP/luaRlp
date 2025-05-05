# Hier sammeln wir Funktionen, die für die Interaktion mit unserer IT und für
# das Setup unserer R-installation am LUA RLP wichtig sind.
#
# Viele dieser Funktionen werden beim laden des LUA RLP Paketes automatisch
# ausgeführt
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'





#' lua_is_online Überprüfe ob R online-Zugang hat
#'
#' Die Funktion checkt ob innerhalb einer gewissen Latenzzeit (timeout) eine
#' Website erreicht werden kann. Dies ist besonders nützlich um bei
#' Fehlermeldugnen während der Paketinstallation zu prüfen, ob die Proxy-
#' Einstellungen in R korrekt sind und eine Netzwerkverbindung zulassen
#'
#' @param timeout
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
