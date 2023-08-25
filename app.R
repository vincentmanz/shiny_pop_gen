library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(tidyr)
library(shinydashboard)

source("ui_import_data.R")
source("ui_genetic_drift.R")

linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- navbarPage(
  "GenoPop",
  theme = shinytheme("flatly"),
  tabPanel("Home", 
           h2("Welcome to GenoPop"), icon = icon("home"),
           HTML(paste("This interactive Shiny app serves as a resource for both novice participants in the Empirical Population Genetics training course and researchers seeking to analyze ALFP or RAPD data. The app features multiple tabs designed to facilitate data processing and enhance understanding of the calculations.",
                      "For more comprehensive insights and course details, please visit Thierry de Meuus' website:", "<a href='https://www.t-de-meeus.fr/EnseignMeeus.html' target='_blank'>https://www.t-de-meeus.fr/EnseignMeeus.html</a>", sep="<br/>")),
           linebreaks(10),
           HTML("If you have any suggestions or encounter bugs, please don't hesitate to contact Vincent Manzanilla through the GitHub page:<br/><a href='https://github.com/vincentmanz/shiny_pop_gen' target='_blank'>https://github.com/vincentmanz/shiny_pop_gen</a>")
  ),
  tabPanel("Data", generateImportDataUI(), icon = icon("cog", lib = "glyphicon")),
  tabPanel("Genetic drift", generateGeneticDriftUI(), icon = icon("cog", lib = "glyphicon")),
  tabPanel("LD", h2("Hierarchical tab"), icon = icon("cog", lib = "glyphicon")),
  tabPanel("Classic statistics", h2("Classic statistics tab"), icon = icon("cog", lib = "glyphicon")),
  tabPanel("Amplification problems", h2("Amplification problems tab"), icon = icon("cog", lib = "glyphicon")),
  tabPanel("Distances", h2("Distances tab"), icon = icon("cog", lib = "glyphicon"))
)

library(shiny)
library(plotly)
library(dplyr)
library(shinyalert)

source("server_import_data.R")
source("server_genetic_drift.R")


server <- function(input, output, session) {
  # Combine server functions from other source files
  server_import_data(input, output, session)
  server_genetic_drift(input, output, session)
}


shinyApp(ui = ui, server = server)
