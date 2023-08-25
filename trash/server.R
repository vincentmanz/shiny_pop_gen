library(shiny)
library(plotly)
library(dplyr)
library(shinyalert)


source("server_import_data.R")
source("server_general_stats.R")
#source("server_map.R")
source("server_genetic_drift.R")

server <- function(input, output, session) {
  # Combine server functions from other source files
  server_import_data(input, output, session)
  server_genetic_drift(input, output, session)
}
