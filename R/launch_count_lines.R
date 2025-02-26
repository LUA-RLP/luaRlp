#' Launch the Line Counter Shiny App
#'
#' This function starts the Shiny app for counting lines in a file.
#'
#' @importFrom shiny runApp
#' @export
launch_count_lines <- function() {
  appDir <- system.file("shiny-apps", "count_lines", package = "luaRlp")

  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.",
         call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE)
}
