library(shiny)
library(shinythemes)
library(leaflet) 
library(plotly)
library(tidyr)

source("ui_import_data.R")
source("ui_general_stats.R")
source("ui_map.R")
source("ui_genetic_drift.R")

ui = navbarPage("GenoPop", theme = shinytheme("flatly"),
                tabPanel("Home", h2("Home tab"), icon = icon("home")),
                tabPanel("Data", generateImportDataUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("General stats", generateGeneralStatsUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Map", mapUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Genetic drift", generateGeneticDriftUI(), icon = icon("cog", lib = "glyphicon")),
                tabPanel("LD", h2("Hierarchical tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Classic statistics", h2("Classic statistics tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Amplification problems", h2("Amplification problems tab"), icon = icon("cog", lib = "glyphicon")),
                tabPanel("Distances", h2("Distances tab"), icon = icon("cog", lib = "glyphicon")))
