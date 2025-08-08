# ui_import_data.R
import_data_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        width = 2,
        title = "Data Import and Structure",
        status = "primary",
        solidHeader = TRUE,
        h3("1. Import Data"),
        fileInput(ns("file1"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput(ns("header"), "Header", TRUE),
        radioButtons(ns("sep"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = "\t"),
        actionButton(ns("load_user_data"), "Load Data", icon = icon("rocket")),        
        br(), br(),
        
        actionButton(ns("load_default_data"), "Load Default Data"),
        tableOutput(ns("preview")),
        br(), br(), tags$hr(), br(),
        h3("2. Filtering data"),
        class = "fixed-filtering-data",
        textInput(ns("exclude_cols"), "Exclude columns (comma-separated)", ""),
        textInput(ns("exclude_rows"), "Exclude rows (comma-separated or range)", ""),
        actionButton(ns("run_filter"), "Filter data", icon = icon("rocket")),
        br(), br(), tags$hr(), br(),
        h3("3. Data structure"),
        class = "fixed-filtering-data",
        position = "right",
        selectInput(ns("pop_data"), "Population*", choices = NULL),
        selectInput(ns("latitude_data"), "Latitude", choices = NULL),
        selectInput(ns("longitude_data"), "Longitude", choices = NULL),
        selectInput(ns("Level1"), "Level 1", choices = NULL),
        selectInput(ns("Level2"), "Level 2", choices = NULL),
        selectInput(ns("Level3"), "Level 3", choices = NULL),
        textInput(ns("col_ranges_data"), "Select allele columns* (format: 1-4 or 5:10)"),
        numericInput(ns("ploidy"), "Ploidy", 2, min = 2, max = 8, step = 2, width = NULL),
        radioButtons(ns("file_format"), "File format*",
                     choices = c("Microsatellite 1 column per allele" = 1,
                                 "Microsatellite 1 column for all the alleles" = 2),
                     selected = 1),
        textInput(ns("missing_code"), "Code for missing data", value = 0),
        actionButton(ns("run_assign"), "Assign metadata", icon = icon("rocket")),
        footer = "* mandatory fields"
      ),
      mainPanel(
        infoBoxOutput(ns("box_population"), width = 3),
        infoBoxOutput(ns("box_individuals"), width = 3),
        infoBoxOutput(ns("box_marker"), width = 3),
        infoBoxOutput(ns("box_number_missing_per"), width = 3)
      ),
      box(
        title = "Data",
        status = "primary",
        solidHeader = TRUE,
        height = "2000",
        width = 10,  
        tagList(
          div(style = "height:500px; overflow-y: scroll; overflow-x: scroll;",
              tableOutput(ns("contents"))
          ),
          downloadButton(ns("download_csv"), ""),
          tableOutput(ns("populationsLL_uniq_table"))
        )
      ),
      box(
        width = 10,
        title = "Map",
        status = "primary",
        solidHeader = TRUE,
        leafletOutput(ns("map"), height = "800px")
      )
    )
  )
}
