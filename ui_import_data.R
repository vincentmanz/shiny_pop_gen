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
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),
     
        # Button to load default data
        actionButton("load_default_data", "Load Default Data")
      ),
      # Main panel for displaying outputs
      mainPanel(
        # Output: Data file
        tableOutput("contents")
      )
    ),
    
    # Additional sidebar panel for filtering data
    sidebarPanel(
      h3("Filtering data"),
      
      # Input: Exclude columns
      textInput("exclude_cols", "Exclude columns (comma-separated)", ""),
      
      # Input: Exclude rows
      textInput("exclude_rows", "Exclude rows (comma-separated or range)", ""),
      
      # Run button
      actionButton("run_filter", "Run")
    )
  )
}
