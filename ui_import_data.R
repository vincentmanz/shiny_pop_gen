# ui_import_data.R

generateImportDataUI <- function(result_data) {
  fluidPage(
    # Box title
    titlePanel("Import Data"),
    
    fluidRow(
      column(4,
             # Sidebar panel for uploading files
             sidebarPanel(
               width = 12,
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
               
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = ''),
               
               tags$hr(),
               
               actionButton("load_default_data", "Load Default Data")
             )
      ),
      column(4,
             # Sidebar panel for filtering data
             sidebarPanel(
               width = 12,
               h3("Filtering data"),
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
               h3("Assign data"),
               class = "fixed-filtering-data",
               position = "right",
               
               selectInput("pop_data", "Population", choices = NULL),
               
               selectInput("latitude_data", "Latitude", choices = NULL),
               
               selectInput("longitude_data", "Longitude", choices = NULL),
               
               textInput("col_ranges_data", "Select Genotypes (e.g., 1-4, 5:10)"),
               
               actionButton("run_assign", "Run Assign Data")
             )
             )
      ),
    
    fluidRow(
      column(12,
             # Main panel for displaying outputs
             mainPanel(
               # Display the infoBoxes for the calculated values
               infoBox("Number of Population:", result_data$number_pop),
               infoBox("Number of individuals:", result_data$number_indv),
               infoBox("Number of marker:", result_data$number_marker),
               infoBox("Number of missing data:", result_data$number_missing),
               infoBox("Percentage of missing data:", result_data$number_missing_per),
               
               tableOutput("results_table"),
               tableOutput("contents"),  # Display df
               tableOutput("df_local"),   # Display df_local
               
               tableOutput("populationsLL_uniq_table"),
               leafletOutput("map")
             )
      )
    )
  )
}
