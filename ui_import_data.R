################################## Input Data tab ################################

generateImportDataUI <- function() {
  fluidPage(
    # Box title
    titlePanel("Uploading Files"),
    # Sidebar panel for file input
    sidebarLayout(
      sidebarPanel(
        # Input: Load default data button
        actionButton("load_default_data", "Load Default Data"),
        
        # Horizontal line
        tags$hr(),
        
        # Input: Select a file
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Horizontal line
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
        
        # Input: Display mode radio buttons
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head")
      ),
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        tableOutput("contents")
        
      )
    )
  )
}

