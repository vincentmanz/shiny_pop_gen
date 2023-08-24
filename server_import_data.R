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
    req(input$pop_data, input$latitude_data, input$longitude_data, input$col_ranges_data, df())
    df_local <- df()
    if (input$pop_data == "") {
      print("You need to select populations")
    } else if (input$col_ranges_data == "") {
      print("You need to select a marker range")
    } else {
      latitude_data <- tryCatch(as.numeric(input$latitude_data), error = function(e) NA)
      longitude_data <- tryCatch(as.numeric(input$longitude_data), error = function(e) NA)
      # Remove spaces from latitude and longitude
      latitude_data <- gsub(" ", "", input$latitude_data)
      longitude_data <- gsub(" ", "", input$longitude_data)
      
      # Check if latitude and longitude are not characters
      if (!is.na(as.numeric(latitude_data)) && !is.na(as.numeric(longitude_data))) {
        print("Latitude and longitude should be numerical, select another column or check your data")
        
        
        
        
      } else {
        
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
        
        
        ## chatGPT:  here, 
        
        ## 1. Convert latitude and longitude to numeric if possible and also if there is a , replace it by a .
        ## 2. Convert the Polation column in Character type not numerical. 
        
        # Convert latitude and longitude to numeric if possible
        if (!is.na(latitude_data)) {
          latitude_data <- as.numeric(gsub(",", ".", gsub(" ", "", input$latitude_data)))
        }
        if (!is.na(longitude_data)) {
          longitude_data <- as.numeric(gsub(",", ".", gsub(" ", "", input$longitude_data)))
        }
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
        range_missing <- sum(
          is.na(new_df[, 4:(3 + length(range_values))]) |
            new_df[, 4:(3 + length(range_values))] == 0 |
            tolower(new_df[, 4:(3 + length(range_values))]) == "na" |
            tolower(new_df[, 4:(3 + length(range_values))]) == 000 |
            tolower(new_df[, 4:(3 + length(range_values))]) == ""
        )
        
        number_missing_per <- (range_missing / length(range_cols)) * 100
        
        print(paste("Percentage of missing data:", number_missing_per))
        
        # Count the number of unique values in pop_data
        number_pop <- length(unique(new_df[[input$pop_data]]))
        print("1")
        
        # Count the number of selected columns in col_ranges_data
        number_marker <- length(range_values)
        print("2")
        
        # Print the results
        print(paste("Number of Population:", number_pop))
        print(paste("Number of individuals:", number_indv))
        print(paste("Number of marker:", number_marker))
        print(paste("Number of missing data:", range_missing))
        print(paste("Percentage of missing data:", number_missing_per))
      }
    }
  })
  
}
