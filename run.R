## global.R ##

# CARREGAMENTO DE PACOTES
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(tmap)
#devtools::install_github("rpradosiqueira/brazilmaps"
library(brazilmaps)
library(viridisLite)
library(units)
library(sf)
library(leaflet)
library(leafsync)
library(lwgeom)
library(leafem)
library(raster)
library(stars)
library(tmaptools)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
