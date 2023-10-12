# data.R #

# ui

generateImportDataUI <- function() {
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
               tableOutput("contents"),
               tableOutput("results_table"),
               tableOutput("populationsLL_uniq_table"),
               leafletOutput("map") 
             )
      )
    )
  )
}


# server

default_df <- readr::read_tsv("https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt")

server_import_data <- function(input, output, session) {
  
  # Define the reactive expression to hold the data frame
  df <- reactiveVal(default_df)
  
  # Load default data when the button is clicked
  observeEvent(input$load_default_data, {
    df(read.csv("https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt",
                header = input$header,
                sep = input$sep,
                quote = input$quote))
  })
  
  # Handle file upload
  observeEvent(input$file1, {
    req(input$file1)
    df(read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote))
  })
  
  # Handle filtering of data
  observeEvent(input$run_filter, {
    req(df())
    
    # Excluding columns
    excluded_cols <- unlist(strsplit(input$exclude_cols, ","))
    
    if (!is.null(excluded_cols) && length(excluded_cols) > 0 && any(excluded_cols != "")) {
      df_filtered <- df() %>%
        select(-any_of(excluded_cols))
      df(df_filtered)
    }
    
    # Excluding rows
    excluded_rows <- unlist(strsplit(input$exclude_rows, "[, ]+"))
    excluded_rows <- as.integer(excluded_rows[excluded_rows != ""])
    
    if (length(excluded_rows) > 0) {
      df_filtered <- df() %>%
        slice(-excluded_rows)
      df(df_filtered)
    }
  })
  
  # Display the first 15 rows of the dataframe
  output$contents <- renderTable({
    req(df())
    head(df(), n = 15)
  })
  
  # Update the select input choices
  observe({
    req(df())
    df_local <- df()
    updateSelectInput(session, 'pop_data', choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'latitude_data', choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'longitude_data', choices = c("select" = "", colnames(df_local)))
  })
  
  # Create a reactiveValues object to store the results
  result_data <- reactiveValues(
    number_pop = 0,
    number_indv = 0,
    number_marker = 0,
    number_missing = 0,
    number_missing_per = 0
  )
  
  # Create the table and the map
  observeEvent(input$run_assign, {
    req(input$pop_data, input$latitude_data, input$longitude_data, input$col_ranges_data, df())
    df_local <- df()
    
    # Check if Population has data
    if (input$pop_data == "") {
      shinyalert(title = "Error", text = "You need to select populations.", type = "error")
      return()  # Exit the event handler
    } 
    
    # Check if Marker Range has data
    if (input$col_ranges_data == "") {
      shinyalert(title = "Error", text = "You need to select a marker range.", type = "error")
      return()  # Exit the event handler
    }
    
    # Check if latitude is not numeric
    if (is.numeric(input$latitude_data)) {
      shinyalert(title = "Error", text = "Latitude should be numerical, select another column or check your data.", type = "error")
      return()  # Exit the event handler
    }
    if (is.numeric(input$longitude_data)) {
      shinyalert(title = "Error", text = "Longitude should be numerical, select another column or check your data.", type = "error")
      return()  # Exit the event handler
    }
    # Check if the range is valid
    range_values <- unlist(strsplit(input$col_ranges_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    if (any(is.na(range_values)) || range_values[1] < 1 || range_values[2] > length(colnames(df_local))) {
      shinyalert(title = "Error", text = "Try again, your range is out of bounds.", type = "error")
      return()  # Exit the event handler
    }
    
    # Convert marker range to numeric
    range_data <- input$col_ranges_data
    range_values <- unlist(strsplit(range_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    # Extract the range of column headers
    column_range_name <- colnames(df_local)[range_values[1]:range_values[2]]
    
    # Filter columns to keep in the new data frame
    cols_to_keep <- c(input$pop_data, input$latitude_data, input$longitude_data, column_range_name)
    new_df <- df_local[, cols_to_keep]
    
    # Rename columns
    col_names <- colnames(new_df)
    col_names[2] <- "Latitude"
    col_names[3] <- "Longitude"
    col_names[4:(3 + length(column_range_name))] <- column_range_name
    colnames(new_df) <- col_names
    
    # Convert selected columns to numeric
    cols_to_convert <- col_names[which(col_names %in% c("Latitude", "Longitude", col_names[4:(3 + length(range_values))]))]
    new_df[, cols_to_convert] <- apply(new_df[, cols_to_convert], 2, as.numeric)
    
    # Update df_assigned
    df(new_df)
    
    ### General stats###
    range_cols <- colnames(new_df)[4:(3 + length(range_values))]
    
    # Calculate the number of rows and assign it to a variable 'number_indv'
    number_indv <- nrow(new_df)
    
    # Count missing data in the range col_ranges_data
    number_missing <- sum(
      is.na(new_df[, 4:(3 + length(range_values))]) |
        new_df[, 4:(3 + length(range_values))] == 0 |
        tolower(new_df[, 4:(3 + length(range_values))]) == "na" |
        tolower(new_df[, 4:(3 + length(range_values))]) == 000 |
        tolower(new_df[, 4:(3 + length(range_values))]) == ""
    )
    
    number_missing_per <- (number_missing / length(range_cols)) * 100
    
    # Count the number of unique values in pop_data
    number_pop <- length(unique(new_df[[input$pop_data]]))
    
    # Count the number of selected columns in col_ranges_data
    number_marker <- length(range_values)
    
    # Print the results
    print(paste("Number of Population:", number_pop))
    print(paste("Number of individuals:", number_indv))
    print(paste("Number of marker:", number_marker))
    print(paste("Number of missing data:", number_missing))
    print(paste("Percentage of missing data:", number_missing_per))
    
    # Create a data frame for the missing info
    results_table <- data.frame(
      "Number of Population:" = number_pop,
      "Number of individuals:" = number_indv,
      "Number of marker:" = number_marker,
      "Number of missing data:" = number_missing,
      "Percentage of missing data:" = number_missing_per
    )
    
    # Update result_data with the calculated values
    result_data$number_pop <- number_pop
    result_data$number_indv <- number_indv
    result_data$number_marker <- number_marker
    result_data$number_missing <- number_missing
    result_data$number_missing_per <- number_missing_per
    
    # Render the results table using renderUI
    output$results_table_ui <- renderUI({
      tableOutput("results_table")
    })
    
    # Render the actual results table
    output$results_table <- renderTable({
      results_table
    })
    
    
    ##### MAP ####
    
    # Create the populationsLL data frame
    populationsLL <- new_df[,1:3]
    print(populationsLL)
    
    # Group by Locality, Latitude, and Longitude, and calculate Population Size
    populationsLL_grouped <- populationsLL %>%
      group_by_all()%>%count()
    colnames(populationsLL_grouped) <- c('Population', 'Longitude', 'Latitude', 'Population size')
    
    # Render the populationsLL_uniq data frame as a table
    output$populationsLL_uniq_table <- renderTable({
      req(input$run_map)  # Show the table after clicking "Run Map" button
      populationsLL_grouped
    })
    # Render the map
    output$map <- renderLeaflet({
      leaflet(populationsLL_grouped) %>%
        addTiles() %>%
        addCircles(lng = populationsLL_grouped$Latitude, lat = populationsLL_grouped$Longitude, 
                   popup=paste("Location:", populationsLL_grouped$Population, "<br>","Population size:", populationsLL_grouped$`Population size`), 
                   radius = populationsLL_grouped$`Population size` * 50,
                   stroke = FALSE, fillOpacity = 0.5)
    })
  })
  
  
}
