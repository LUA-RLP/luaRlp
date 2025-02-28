#' Launch the qPCR Evaluator Shiny App
#'
#' This function starts the Shiny app for Evaluating qPCR results.
#'
#' @importFrom shiny runApp
#' @export
launch_qPCR_evaluator <- function() {
  appDir <- system.file("shiny-apps", "qPCR_evaluator", package = "luaRlp")

  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.",
         call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE)
}
