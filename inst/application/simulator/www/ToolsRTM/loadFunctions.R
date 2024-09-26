#' loading all function in ToolsRTM and SCOPEinR packages
#'
#' @return
#' @export
#'
#' @examples
loadFunctions <- function() {
  # Load functions from ToolsRTM folder
  sourceDir <- 'www/ToolsRTM'
  sourceFiles <- list.files(sourceDir, pattern = "\\.R$", full.names = TRUE)
  sapply(sourceFiles, source, .GlobalEnv)

  # Load functions from SCOPEinR folder
  sourceDir <- 'www/SCOPEinR'
  sourceFiles <- list.files(sourceDir, pattern = "\\.R$", full.names = TRUE)
  sapply(sourceFiles, source, .GlobalEnv)
}
