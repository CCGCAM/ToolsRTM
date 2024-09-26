#' load rda databases
#'
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
loadRDa <- function(directory) {
  # Get all .rds files in the specified directory
  rdaFiles <- list.files(directory, pattern = "\\.rda$", full.names = TRUE)
  rdaFiles <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)

  # Load each .rds file into the global environment
  for (file in rdaFiles) {
    load(file, .GlobalEnv)
  }

}
