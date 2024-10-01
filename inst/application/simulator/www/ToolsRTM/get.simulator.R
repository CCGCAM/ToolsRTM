#' Get the simulator tools
#'
#' This function launches the simulator app and checks for package dependencies using `get.packages()`.
#'
#' @return Launches the Shiny app and checks for package dependencies.
#' @export
#'
#' @examples
#' get.simulator()
get.simulator <- function() {
  # Call get.packages to check for required dependencies
  packages <- get.packages()

  # Print the list of dependencies (optional, for debugging purposes)
  message("The following packages are required for the simulator:")
  print(packages)

  appDir <- system.file("application", "simulator", package = "ToolsRTM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ToolsRTM`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
