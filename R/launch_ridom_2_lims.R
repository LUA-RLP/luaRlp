#' Launch the Ridom 2 LIMS Shiny App
#'
#' This function starts the Shiny app for preparing RIDOM outputs for LIMS
#' import.
#'
#' @importFrom shiny runApp
#' @export
launch_ridom_2_lims <- function() {
  appDir <- system.file("shiny-apps", "ridom_2_lims", package = "luaRlp")

  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.",
         call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE)
}
