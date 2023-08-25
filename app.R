# app.R 

library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(tidyr)
library(shinydashboard)
library(dplyr)
library(shinyalert)
library(shinydashboardPlus)

# Source server and UI files
source("server_import_data.R")
source("server_genetic_drift.R")
source("ui_import_data.R")
source("ui_genetic_drift.R")

shiny.react::enableReactDebugMode()

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Create the result_data reactiveValues object
result_data <- reactiveValues(
  number_pop = 0,
  number_indv = 0,
  number_marker = 0,
  number_missing = 0,
  number_missing_per = 0
)


## dashboard version
header <- dashboardHeader(title = "GenoPop")
sidebar <- dashboardSidebar(
  sidebarMenu(id="sidebar",  
              menuItem("Welcome to GenoPop", tabName = "welcome", icon = icon("home"),  selected=TRUE),
              menuItem("Data import and filtering", tabName = "data", icon = icon("dashboard")),
              menuItem("Genetic drift", tabName = "drift", icon = icon("dashboard"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "welcome",
      h2("Welcome to GenoPop"),
      icon = icon("home"),
      HTML(
        paste(
          "This interactive Shiny app serves as a resource for both novice participants in the Empirical Population Genetics training course and researchers seeking to analyze ALFP or RAPD data. The app features multiple tabs designed to facilitate data processing and enhance understanding of the calculations.",
          "For more comprehensive insights and course details, please visit Thierry de Meuus' website:",
          "<a href='https://www.t-de-meeus.fr/EnseignMeeus.html' target='_blank'>https://www.t-de-meeus.fr/EnseignMeeus.html</a>",
          sep = "<br/>"
        )
      ),
      linebreaks(10),
      HTML(
        "If you have any suggestions or encounter bugs, please don't hesitate to contact Vincent Manzanilla through the GitHub page:<br/><a href='https://github.com/vincentmanz/shiny_pop_gen' target='_blank'>https://github.com/vincentmanz/shiny_pop_gen</a>"
      )
    ),
    tabItem(
      tabName = "data",
      generateImportDataUI(result_data),  # Pass result_data to the function
      icon = icon("cog", lib = "glyphicon")
    ),
    tabItem(
      tabName = "drift",
      generateGeneticDriftUI(),
      icon = icon("cog", lib = "glyphicon")
    )
  )
)

ui <- dashboardPage(
  skin = "midnight",
  header = header,
  sidebar = sidebar,
  body = body(result_data)  # Pass result_data to the body function
)

server <- function(input, output, session) {
  # Combine server functions from other source files
  server_import_data(input, output, session, result_data)
  server_genetic_drift(input, output, session)
}

shinyApp(ui = ui, server = server)