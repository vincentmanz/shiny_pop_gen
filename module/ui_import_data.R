# ui_import_data.R

#customDownloadbutton <- function(outputId, label = ""){
#  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = paste("data-", Sys.Date(), ".csv", sep = "\t"), 
#         target = "contents", download = NA, icon("download"), label)
#}

generateImportDataUI <- function() {
  fluidPage(
    fluidRow(
      box(
        width = 2,
        title = "Data Import and Structure",
        status = "primary",
        solidHeader = TRUE,
        h4("1. Import Data"),
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
        actionButton("load_default_data", "Load Default Data"),
        br(), br(), tags$hr(), br(),
        h4("2. Filtering data"),
        class = "fixed-filtering-data",
        textInput("exclude_cols", "Exclude columns (comma-separated)", ""),
        textInput("exclude_rows", "Exclude rows (comma-separated or range)", ""),
        actionButton("run_filter", "Run"),
        br(), br(), tags$hr(), br(),
        h4("3. Data structure"),
        class = "fixed-filtering-data",
        position = "right",
        selectInput("pop_data", "Population*", choices = NULL),
        selectInput("latitude_data", "Latitude", choices = NULL),
        selectInput("longitude_data", "Longitude", choices = NULL),
        textInput("col_ranges_data", "Select allele columns* (format: 1-4 or 5:10)"),
        numericInput("ploidy", "Ploidy", 2, min = 2, max = 8, step = 2, width = NULL),
        radioButtons("file_format", "File format*",
                     choices = c("Microsatellite 1 column per allele" = 1,
                                 "Microsatellite 1 column for all the alleles" = 2),
                     selected = 1),
        textInput("missing_code", "Code for missing data", value = 0),
        actionButton("run_assign", "Run Assign Data"),
        footer = "* mandatory fields"
      ),
      mainPanel(
        infoBoxOutput("box_population", width = 3),
        infoBoxOutput("box_individuals", width = 3),
        infoBoxOutput("box_marker", width = 3),
        infoBoxOutput("box_number_missing_per", width = 3)
      ),
      box(
        title = "Data",
        status = "primary",
        solidHeader = TRUE,
        height = "2000",
        width = 10,  
        tagList(
          # Wrap the table in a div with scrollable styles
          div(style = "height:500px; overflow-y: scroll; overflow-x: scroll;",
              tableOutput("contents")
          ),
          
          ## DOWNLOAD
          downloadButton("download_csv", ""),
          tableOutput("populationsLL_uniq_table")
        )
      ),
      box(
        width = 10,
        title = "Map",
        status = "primary",
        solidHeader = TRUE,
        leafletOutput("map",
                      height = "800px")
        ## DOWNLOAD
#        downloadButton("download_map", "Download"),
      )
    )
  )
}
