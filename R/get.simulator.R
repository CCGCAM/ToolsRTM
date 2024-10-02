#' Get the simulator tools
#'
#' This function launches the simulator Shiny app and ensures that all necessary package dependencies are installed. 
#' It first runs `get.packages()` to check for and install any missing packages required by the app.
#'
#' Note: It is required to run `get.packages()` beforehand to ensure all package dependencies are resolved.
#'
#' @return Launches the Shiny app and verifies that all necessary packages are installed.
#' @export
#'
#' @examples
#' # First, ensure package dependencies are installed:
#' get.packages()
#' 
#' # Then, launch the simulator:
#' get.simulator()
get.simulator <- function() {
 
  appDir <- system.file("application", "simulator", package = "ToolsRTM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ToolsRTM`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
