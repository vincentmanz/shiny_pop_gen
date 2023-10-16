# app.R #

library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(tidyr)
library(shinydashboard)
library(dplyr)
library(shinyalert)
library(shinydashboardPlus)
library(hierfstat)
library(kableExtra)
library(adegenet)
library(tidyverse)


# Source server and UI files
source("module/server_import_data.R")
source("module/server_genetic_drift.R")
source("module/server_general_stats.R")

source("module/ui_import_data.R")
source("module/ui_genetic_drift.R")
source("module/ui_general_stats.R")

source("www/helper.R")

source("module/welcome.R")

shiny.react::enableReactDebugMode()

ui <- dashboardPage(
  skin = "midnight",
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  # Combine server functions from other source files
  server_import_data(input, output, session)
  general_stats_server(input, output, session)
  server_genetic_drift(input, output, session)
}


shinyApp(ui = ui, server = server)
