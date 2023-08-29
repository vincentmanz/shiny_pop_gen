# ui_import_data.R

generateImportDataUI <- function() {
  fluidPage(
    # Box title
    fluidRow(
      column(4,
             # Sidebar panel for uploading files
             sidebarPanel(
               width = 12,
               h3("1. Import Data"),
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               
               checkboxInput("header", "Header", TRUE),
               
               radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = "\t"),
               
               tags$hr(),
               
               actionButton("load_default_data", "Load Default Data")
             )
      ),  
      column(4,
             # Sidebar panel for filtering data
             sidebarPanel(
               width = 12,
               h3("2. Filtering data"),
               class = "fixed-filtering-data",
               
               textInput("exclude_cols", "Exclude columns (comma-separated)", ""),
               
               textInput("exclude_rows", "Exclude rows (comma-separated or range)", ""),
               
               actionButton("run_filter", "Run")
               
             )
      ),
      column(4,
             # Sidebar panel for assigning data
             sidebarPanel(
               width = 12,
               h3("3. Data structure"),
               class = "fixed-filtering-data",
               position = "right",
               
               selectInput("pop_data", "Population*", choices = NULL),
               
               selectInput("latitude_data", "Latitude", choices = NULL),
               
               selectInput("longitude_data", "Longitude", choices = NULL),
               
               textInput("col_ranges_data", "Select allelle columns* (format: 1-4 or 5:10)"),
               
               numericInput( "ploidy", "Ploidy", 2, min = 2, max = 8, step = 2, width = NULL),
               
               radioButtons("file_format", "File format",
                            choices = c("Microsatelite 1 colomn per allele" = 1,
                                        "Microsatelite 1 colomn for the all the alleles" = 2 
                            ),
                            selected = "Microsatelite 1 colomn per allele"),
               textInput("missing_code", "Code for missing data", value=0),
               
               actionButton("run_assign", "Run Assign Data"),
              
               h6("* mandatory fields")
               
             )
      )
    ),
    
    fluidRow(
      column(12,
             # Main panel for displaying outputs
             mainPanel(
               tableOutput("contents"),
               tableOutput("populationsLL_uniq_table"),
               infoBoxOutput("box_population", width=5),
               infoBoxOutput("box_individuals", width=5),
               infoBoxOutput("box_marker", width=5),
               infoBoxOutput("box_number_missing_per", width=5),
               leafletOutput("map") 
             )
      )
    )
  )
}
