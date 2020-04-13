library(shiny)
library(shinythemes)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(viridisLite)
library(leaflet)
library(DT) 
library(spdep)
library(knitr)
library(kableExtra)
library(viridis)
library(plotly)
# library(crosstalk)

source("Data/cancer_df.R")

source("Data/ui_cancer.R")

source("Data/server_cancer.R")

shinyApp(ui = ui, server = server)