
#' Get Simulator  By Carlos Camino
#'
#' @return
#' @export
#'
#' @examples
get.simulator <- function() {
  appDir <- system.file("application", "simulator", package = "ToolsRTM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ToolsRTM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}