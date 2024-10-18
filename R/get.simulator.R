#' Get the simulator tools
#'
#' This function launches the simulator Shiny app and ensures that all necessary package dependencies are installed. 
#' It first runs `get.packages()` to check for and install any missing packages required by the app.
#'
#' Note: It is required to run `get.packages()` beforehand to ensure all package dependencies are resolved.
#'
#' @param app A character string indicating which simulator to launch. 
#'            Acceptable values are:
#'            - `"prosail"`: Launch the PROSAIL simulator.
#'            - `"prosail-brf"`: Launch the PROSAIL-BRF simulator.
#'            - `"marmit"`: Launch the MARMIT simulator.
#'            - `"default"`: Launch the default simulator (general).
#' @return Launches the Shiny app and verifies that all necessary packages are installed.
#' @export
#'
#' @examples
#' # First, ensure package dependencies are installed:
#' get.packages()
#' 
#' # Then, launch the simulator:
#' get.simulator()
#' 
#' # Launch specific simulators:
#' get.simulator("prosail")
#' get.simulator("prosail-brf")
#' get.simulator("marmit")

get.simulator <- function(app = "default") {
  
  
  # Determine the appropriate app directory based on the simulator parameter
  if (app == "prosail") {
    appDir <- system.file("application", "PROSAIL", package = "ToolsRTM")
  } else if (app == "prosail-brf") {
    appDir <- system.file("application", "PROSAIL-BRF", package = "ToolsRTM")
  } else if (app == "marmit") {
    appDir <- system.file("application", "MARMIT", package = "ToolsRTM")
  } else {
    appDir <- system.file("application", "simulator", package = "ToolsRTM")
  }
  
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ToolsRTM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}