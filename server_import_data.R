# server_import_data.R

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
  # Assign data logic
  observeEvent(input$run_assign, {
    req(input$pop_data, input$latitude_data, input$longitude_data, input$col_ranges_data)
    
    if (input$pop_data == "") {
      print("You need to select populations")
    } else if (input$col_ranges_data == "") {
      print("You need to select a marker range")
    } else {
      latitude_data <- tryCatch(as.numeric(input$latitude_data), error = function(e) NA)
      longitude_data <- tryCatch(as.numeric(input$longitude_data), error = function(e) NA)
      
      if (is.na(latitude_data) || is.na(longitude_data)) {
        print("Latitude and longitude should be numerical")
      } else {}
    }
  })
}
