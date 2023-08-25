library(shiny)
library(shinythemes)
library(leaflet) 
library(plotly)
library(tidyr)
library(shinydashboard)

source("ui_import_data.R")
source("ui_general_stats.R")
source("ui_map.R")
source("ui_genetic_drift.R")


linebreaks <- function(n){HTML(strrep(br(), n))}


ui = navbarPage("GenoPop", theme = shinytheme("flatly"),
                tabPanel("Home", 
                         h2("Welcome to GenoPop"), icon = icon("home"),
                         HTML(paste("This interactive Shiny app serves as a resource for both novice participants in the Empirical Population Genetics training course and researchers seeking to analyze ALFP or RAPD data. The app features multiple tabs designed to facilitate data processing and enhance understanding of the calculations.",
                                  "For more comprehensive insights and course details, please visit Thierry de Meuus' website:", "<a href='https://www.t-de-meeus.fr/EnseignMeeus.html' target='_blank'>https://www.t-de-meeus.fr/EnseignMeeus.html</a>", sep="<br/>")),
                         linebreaks(10),
                         HTML("If you have any suggestions or encounter bugs, please don't hesitate to contact Vincent Manzanilla through the GitHub page:<br/><a href='https://github.com/vincentmanz/shiny_pop_gen' target='_blank'>https://github.com/vincentmanz/shiny_pop_gen</a>")
#                         img(src = "https://www.google.com/url?sa=i&url=https%3A%2F%2Fosg-htc.org%2Fspotlights%2FJohri.html&psig=AOvVaw0rqZ0PoLVpSouxccl8fU_7&ust=1692874522389000&source=images&cd=vfe&opi=89978449&ved=0CBAQjRxqFwoTCIDrrJfP8oADFQAAAAAdAAAAABAH")
                         
                      ),
                tabPanel("Data", generateImportDataUI(), icon = icon("cog", lib = "glyphicon")),
#                tabPanel("General stats", generateGeneralStatsUI(), icon = icon("cog", lib = "glyphicon")),
#                tabPanel("Map", mapUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Genetic drift", generateGeneticDriftUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("LD", h2("Hierarchical tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Classic statistics", h2("Classic statistics tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Amplification problems", h2("Amplification problems tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Distances", h2("Distances tab"), icon = icon("cog", lib = "glyphicon")))
