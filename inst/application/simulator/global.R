
rm(list= ls())
##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################
if (!require("leaflet.extras2")) { install.packages("leaflet.extras2"); require("leaflet.extras2") }  ### Google services
if (!require("leaflet.extras")) { install.packages("leaflet.extras"); require("leaflet.extras") }  ### Google services
if (!require("mapedit")) { install.packages("mapedit"); require("mapedit") }

if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }  ###

if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }  ###
if (!require("tidyverse")) { install.packages("tidyverse"); require("tidyverse") }

if (!require("shinydashboard")) { install.packages("shinydashboard"); require("shinydashboard") }  ###
if (!require("shinyWidgets")) { install.packages("shinyWidgets"); require("shinyWidgets") }  ###
if (!require("shinythemes")) { install.packages("shinythemes"); require("shinythemes") }
if (!require("shinyWidgets")) { install.packages("shinyWidgets"); require("shinyWidgets") }
if (!require("shinybusy")) { install.packages("shinybusy"); require("shinybusy") } ## loading bar progress

if (!require("foreach")) { install.packages("foreach"); require("foreach") }
if (!require("parallel")) { install.packages("parallel"); require("parallel") }
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }


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

# Call this function to load all your functions
loadFunctions()

loadRDa <- function(directory) {
  # Get all .rds files in the specified directory
  rdaFiles <- list.files(directory, pattern = "\\.rda$", full.names = TRUE)

  # Load each .rds file into the global environment
  for (file in rdaFiles) {
    load(file, .GlobalEnv)
  }

}

if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### Paralell foreach and caret
if (!require("SCOPEinR")) { install.packages("SCOPEinR"); require("SCOPEinR") }  ### Paralell foreach and caret

# Call this function by providing the path to the directory containing .rds files
#loadRDa("www/data/ToolsRTM")
#loadRDa("www/data/SCOPEinR")
#loadRDa("www/data")


