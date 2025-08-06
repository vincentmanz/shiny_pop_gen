# app.R
suppressPackageStartupMessages({
  # Interface
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyalert)
  library(waiter)

  # data manipulation
  library(tidyverse)
  library(tidyr)
  library(dplyr)
  library(broom)
  library(reshape2)

  # population genomics
  library(hierfstat)
  library(adegenet)
  library(pegas)
  library(poppr)
  library(boot)

  # themes and graphics
  library(hrbrthemes)
  library(leaflet)
  library(plotly)
  library(kableExtra)
  library(ggplot2)
  
  # processs
  library(parallel)
  library(foreach)
  library(doParallel)
})

# Source server and UI files
source("module/server_import_data.R")
source("module/server_genetic_drift.R")
source("module/server_general_stats.R")
source("module/server_LD.R")

source("module/ui_import_data.R")
source("module/ui_genetic_drift.R")
source("module/ui_general_stats.R")
source("module/ui_LD.R")

source("www/helper.R")

source("module/welcome.R")

shiny.react::enableReactDebugMode()

ui <- dashboardPage(
  skin = "black-light",
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  server_import_data(input, output, session)
  server_general_stats(input, output, session)
  server_LD(input, output, session)
  server_genetic_drift(input, output, session)
}

pgacmdr_app <- shinyApp(
  ui = ui, 
  server = server
)

runApp(pgacmdr_app)

