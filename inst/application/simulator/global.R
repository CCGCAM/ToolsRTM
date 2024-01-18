

##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################
# remotes::install_github("r-spatial/rgee")
#library(shiny)
if (!require("leaflet.extras2")) { install.packages("leaflet.extras2"); require("leaflet.extras2") }  ### Google services
if (!require("leaflet.extras")) { install.packages("leaflet.extras"); require("leaflet.extras") }  ### Google services

if (!require("geojsonio")) { install.packages("geojsonio"); require("geojsonio") }  ### Google services


#if (!require("raster")) { install.packages("raster"); require("raster") }  ### G
#if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }  ###
#if (!require("sf")) { install.packages("sf"); require("sf") }  ###
if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }  ###

#if (!require("maps")) { install.packages("maps"); require("maps") }  ###

if (!require("stars")) { install.packages("stars"); require("stars") }  ###
if (!require("shinydashboard")) { install.packages("shinydashboard"); require("shinydashboard") }  ###
if (!require("shinyWidgets")) { install.packages("shinyWidgets"); require("shinyWidgets") }  ###

#if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") } ###
if (!require("SCOPEinR")) { install.packages("SCOPEinR"); require("SCOPEinR") }  ###

if (!require("foreach")) { install.packages("foreach"); require("foreach") }

library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(mapedit)


