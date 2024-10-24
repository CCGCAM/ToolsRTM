#' Get the simulator tools
#'
#' @param app A character string indicating which simulator to launch. 
#'            Acceptable values are:
#'            - `"PROSAIL"`: Launch the PROSAIL simulator.
#'            - `"PROSAIL-BRDF"`: Launch the PROSAIL-BRF simulator.
#'            - `"MARMIT"`: Launch the MARMIT simulator.
#'            - `"SPART"`: Launch the SPART simulator. Need the SCOPEinR package
#'            - `"SCOPE"`: Launch the SCOPE simulator. Need the SCOPEinR package
#'            - `"getLUT"`: Launch the configuration LUT app. 
#'            -`"RTMs"`: Launch the RTM simulator. 
#'            -`"STAC"`: Launch the RTM simulator. 
#'            -`"Inversion"`: Launch the Inversion module for retriving plant traits. 
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
#' get.simulator("PROSAIL")
#' get.simulator("SCOPE")
#' get.simulator("MARMIT")
#' get.simulator("getLUT")
#' get.simulator("STAC")

get.simulator <- function(app = "PROSAIL") {
  
  # Validate the app parameter
  valid_apps <- c("PROSAIL", "PROSAIL-BRDF", "MARMIT", "getLUT",'SPART', 'SCOPE','RTMs','Inversion', 'STAC')
  
  if (!app %in% valid_apps) {
    stop(paste("Invalid app specified. Please choose one of the following options:", 
               paste(valid_apps, collapse = ", ")), call. = FALSE)
  }
  
  # Check for SCOPEinR package if app is "scope" or "spart"
  if (app %in% c("SCOPE", "SPART") && !"SCOPEinR" %in% installed.packages()[,"Package"]) {
    stop("The SCOPEinR package is required for this app. Please install it from the GitLab repository.", call. = FALSE)
  } else {
    message ('ToolRTM and SCOPEinR packages are install on your system.')
  }
  # Determine the appropriate app directory based on the simulator parameter
  if (app == "PROSAIL") {
    appDir <- system.file("applications", "PROSAIL", package = "ToolsRTM")
  } else if (app == "PROSAIL-BRDF") {
    appDir <- system.file("applications", "PROSAIL-BRDF", package = "ToolsRTM")
  } else if (app == "MARMIT") {
    appDir <- system.file("applications", "MARMIT", package = "ToolsRTM")
  } else if (app == "SPART") {
    appDir <- system.file("applications", "SPART", package = "ToolsRTM")
  } else if (app == "SCOPE") {
    appDir <- system.file("applications", "SCOPE", package = "ToolsRTM")
  } else if (app == "getLUT") {
    appDir <- system.file("applications", "LUTs", package = "ToolsRTM")
  } else if (app == "STAC") {
    appDir <- system.file("applications", "STAC", package = "ToolsRTM")
  } else if (app == "Inversion") {
    appDir <- system.file("applications", "Inversion", package = "ToolsRTM")
  } else {
    appDir <- system.file("applications", "RTMs", package = "ToolsRTM")
  }
  
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ToolsRTM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}