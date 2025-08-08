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
  
  # process
  library(parallel)
  library(foreach)
  library(doParallel)
})

# ----- Source module files -----
source("module/welcome.R")              # header & sidebar only
source("module/ui_welcome.R")            # welcome_ui()
source("module/ui_import_data.R")        # import_data_ui()
source("module/ui_general_stats.R")      # general_stats_ui()
source("module/ui_LD.R")                 # ld_ui()
# source("module/ui_genetic_drift.R")      # genetic_drift_ui()

source("module/server_welcome.R")        # welcome_server()
source("module/server_import_data.R")    # server_import_data()
source("module/server_general_stats.R")  # server_general_stats()
source("module/server_LD.R")             # server_LD()
# source("module/server_genetic_drift.R")  # server_genetic_drift()

# ---------- UI ----------
ui <- dashboardPage(
  skin = "midnight",
  header = header,
  sidebar = sidebar,
  body = dashboardBody(
    waiter::useWaiter(),
    shinyalert::useShinyalert(),
    tabItems(
      tabItem("welcome", welcome_ui("welcome")),
      tabItem("data", import_data_ui("import")),
      tabItem("general_stats", general_stats_ui("gs")),
      tabItem("linkage_desequilibrium", linkage_desequilibrium_UI("ld"))
      # tabItem("drift", genetic_drift_ui("gd"))
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  welcome_server("welcome")
  server_import_data("import")
  server_general_stats("gs")
  server_LD("ld")
  # server_genetic_drift("gd")
}

# ---------- Run ----------
shinyApp(ui = ui, server = server)