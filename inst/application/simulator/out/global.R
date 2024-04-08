

##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################
# remotes::install_github("r-spatial/rgee")
#library(shiny)


install_and_load_packages <- function() {
  required_packages <- c("shiny", "ggplot2", "dplyr")  # Add your required packages here
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }
  
  sapply(required_packages, require, character.only = TRUE)
}


if (!require("leaflet.extras2")) { install.packages("leaflet.extras2"); require("leaflet.extras2") }  ### Google services
if (!require("leaflet.extras")) { install.packages("leaflet.extras"); require("leaflet.extras") }  ### Google services
if (!require("geojsonio")) { install.packages("geojsonio"); require("geojsonio") }  ### Google services


if (!require("stars")) { install.packages("stars"); require("stars") }  ###
if (!require("shinydashboard")) { install.packages("shinydashboard"); require("shinydashboard") }  ###
if (!require("shinyWidgets")) { install.packages("shinyWidgets"); require("shinyWidgets") }  ###
if (!require("shinythemes")) { install.packages("shinythemes"); require("shinythemes") }  ###

#if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") } ###
#if (!require("SCOPEinR")) { install.packages("SCOPEinR"); require("SCOPEinR") }  ###

if (!require("foreach")) { install.packages("foreach"); require("foreach") }
if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }  ###
if (!require("tidyverse")) { install.packages("tidyverse"); require("tidyverse") }  ###
if (!require("mapedit")) { install.packages("mapedit"); require("mapedit") }  ###

# Load and install packages
install_and_load_packages()

