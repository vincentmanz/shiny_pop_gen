# ui_import_data.R

generateImportDataUI <- function() {
  fluidPage(
    fluidRow(
      box(
        width = 3,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "Data Import and Structure"),
        solidHeader = TRUE,
        h3("1. Import Data"),
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput("header", "Header", TRUE),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = "\t"),
        actionButton("load_user_data", "Load Data", icon = icon("rocket")),        
        br(), br(),
        actionButton("load_default_data", "Load Default Data"),
        tableOutput("preview"),
        br(), br(), tags$hr(), br(),
        h3("2. Filtering data"),
        class = "fixed-filtering-data",
        textInput("exclude_cols", "Exclude columns (comma-separated)", ""),
        textInput("exclude_rows", "Exclude rows (comma-separated or range)", ""),
        actionButton("run_filter", "Filter data", icon = icon("rocket")),
        br(), br(), tags$hr(), br(),
        h3("3. Data structure"),
        class = "fixed-filtering-data",
        position = "right",
        selectInput("pop_data", "Population*", choices = NULL),
        selectInput("latitude_data", "Latitude", choices = NULL),
        selectInput("longitude_data", "Longitude", choices = NULL),
        selectInput("Level1", "Level 1", choices = NULL),
        selectInput("Level2", "Level 2", choices = NULL),
        selectInput("Level3", "Level 3", choices = NULL),
        textInput("col_ranges_data", "Select allele columns* (format: 1-4 or 5:10)"),
        numericInput("ploidy", "Ploidy", 2, min = 2, max = 8, step = 2, width = NULL),
        radioButtons("file_format", "File format*",
                     choices = c("Microsatellite 1 column per allele" = 1,
                                 "Microsatellite 1 column for all the alleles" = 2),
                     selected = 1),
        textInput("missing_code", "Code for missing data", value = 0),
        actionButton("run_assign", "Assign metadata", icon = icon("rocket")),
        footer = "* mandatory fields"
      ),
      mainPanel(
        infoBoxOutput("box_population", width = 6),
        infoBoxOutput("box_individuals", width = 6),
        infoBoxOutput("box_marker", width = 6),
        infoBoxOutput("box_number_missing_per", width = 6)
      ),
      box(
        title = "Data",
        solidHeader = TRUE,
        height = "2000",
        width = 9,  
        tagList(
          div(style = "height:500px; overflow-y: scroll; overflow-x: scroll;",
              tableOutput("contents")
          ),
          br(),
          downloadButton("download_csv", "Download", class = "btn btn-primary"),
          tableOutput("populationsLL_uniq_table")
        )
      ),
      box(
        width = 9,
        title = "Map",
        solidHeader = TRUE,
        tagList(
          div(style = "overflow-y: scroll; overflow-x: scroll;",
              leafletOutput("map")
          ),
          br(),
          downloadButton("download_map", "Download")
        )
      )
    )
  )
}
