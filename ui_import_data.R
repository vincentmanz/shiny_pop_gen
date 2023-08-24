# ui_import_data.R

tags$head(
  tags$style(
    HTML(
      ".fixed-filtering-data {
         position: fixed;
         top: 100px; /* Adjust this value as needed */
         right: 20px; /* Adjust this value as needed */
         width: 300px; /* Adjust this value as needed */
         background-color: white;
         border: 1px solid #ccc;
         padding: 10px;
      }"
    )
  )
)

generateImportDataUI <- function() {
  fluidPage(
    # Box title
    titlePanel("Uploading Files"),
    
    # Sidebar layout
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = "\t"),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = ''),
        
        # Horizontal line ----
        tags$hr(),
        
        # Button to load default data
        actionButton("load_default_data", "Load Default Data")
      ),
      # Main panel for displaying outputs
      mainPanel(
        # Output: Data file
        tableOutput("contents"),
        tableOutput("results_table"),
        tableOutput("populationsLL_uniq_table"),
        leafletOutput("map") 
        
      )
    ),
    
    # Additional sidebar panel for filtering data
    sidebarPanel(
      h3("Filtering data"),
      class = "fixed-filtering-data",
      
      # Input: Exclude columns
      textInput("exclude_cols", "Exclude columns (comma-separated)", ""),
      
      # Input: Exclude rows
      textInput("exclude_rows", "Exclude rows (comma-separated or range)", ""),
      
      # Run button
      actionButton("run_filter", "Run")
    ),
    
    # Additional sidebar panel for assigning data
    sidebarPanel(
      h3("Assign data"),
      class = "fixed-filtering-data",
      
      # Input: Population
      selectInput("pop_data", "Population", choices = NULL),
      
      # Input: Latitude (Dropdown Menu)
      selectInput("latitude_data", "Latitude", choices = NULL),
      
      # Input: Longitude (Dropdown Menu)
      selectInput("longitude_data", "Longitude", choices = NULL),
      
      # Input: Markers (range)
      textInput("col_ranges_data", "Select Genotypes (e.g., 1-4, 5:10)"),
      
      # Run button for Assign data
      actionButton("run_assign", "Run Assign Data")
    
      )
  )
}
