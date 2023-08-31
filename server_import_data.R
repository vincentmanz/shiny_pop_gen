# server_import_data.R


# Define a function for concatenate the alleles in columns

concat_identical_cols <- function(df, ploidy) {
  col_names <- colnames(df)
  col_names <-
    sub("\\.\\d+$", "", col_names)  # Remove .1 or .2 from column names
  
  # Initialize result with one row from temp_df
  col1 <- col_names[1]
  temp_df <- data.frame(col1 = df[[col1]])
  colnames(temp_df) <- col1
  result <- temp_df
  i <- 1
  while (i <= ncol(df)) {
    col1 <- col_names[i]
    if (i + 1 <= ncol(df)) {
      col2 <- col_names[i + 1]
    } else {
      result <- rbind(result, df[[col1]])
      break
    }
    if (identical(col1, col2)) {
      concatenated <-
        ifelse(is.na(df[, i + 1]), as.character(df[, i]), paste(df[, i], df[, i + 1], sep = "/"))
      temp_df <- data.frame(col1 = concatenated)
      colnames(temp_df) <- col1
      result <- cbind(result, temp_df)
      i <- i + ploidy
    } else {
      result <- cbind(result, df[[col1]], df[[col2]])
      i <- i + ploidy
    }
  }
  result <- result[,-1]
  
  return(result)
}

# Define a function for rendering info boxes
renderInfoBoxUI <- function(title, value, icon_name, color) {
  infoBox(
    title,
    value,
    icon = icon(icon_name),
    color = color,
    fill = TRUE
  )
}


server_import_data <- function(input, output, session) {
  # Define the reactive expression to hold the data frame
  df <- reactiveVal()
  
  # Load default data when the button is clicked
  observeEvent(input$load_default_data, {
    df(
      read.csv(
        "https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt",
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    )
  })
  
  # Handle file upload
  observeEvent(input$file1, {
    req(input$file1)
    df(
      read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    )
  })
  # Handle filtering of data
  observeEvent(input$run_filter, {
    req(df())
    missing_code <- as.numeric(input$missing_code)
    
    # Excluding columns
    excluded_cols <- unlist(strsplit(input$exclude_cols, ","))
    
    if (!is.null(excluded_cols) &&
        length(excluded_cols) > 0 && any(excluded_cols != "")) {
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
    updateSelectInput(session,
                      'latitude_data',
                      choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session,
                      'longitude_data',
                      choices = c("select" = "", colnames(df_local)))
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
    req(
      input$pop_data,
      input$latitude_data,
      input$longitude_data,
      input$col_ranges_data,
      df()
    )
    missing_code <- as.numeric(input$missing_code)
    df_local <- df()
    # Check if Population has data
    if (input$pop_data == "") {
      shinyalert(title = "Error",
                 text = "You need to select populations.",
                 type = "error")
      return()  # Exit the event handler
    }
    # Check if Marker Range has data
    if (input$col_ranges_data == "") {
      shinyalert(title = "Error",
                 text = "You need to select a marker range.",
                 type = "error")
      return()  # Exit the event handler
    }
    # Check if the range is valid
    range_values <- unlist(strsplit(input$col_ranges_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    if (any(is.na(range_values)) ||
        range_values[1] < 1 ||
        range_values[2] > length(colnames(df_local))) {
      shinyalert(title = "Error",
                 text = "Try again, your range is out of bounds.",
                 type = "error")
      return()  # Exit the event handler
    }
    # Convert marker range to numeric
    range_data <- input$col_ranges_data
    range_values <- unlist(strsplit(range_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    # Extract the range of column headers
    column_range_name <-
      colnames(df_local)[range_values[1]:range_values[2]]

    # Determine if Latitude and Longitude are empty
    latitude_empty <- input$latitude_data == ""
    longitude_empty <- input$longitude_data == ""
    
    
    
    ### Option map ###
    
    if (latitude_empty && longitude_empty) {
      print("\n\n\n(latitude_empty && longitude_empty)\n\n\n")
      
      # Filter columns to keep in the new data frame
      cols_to_keep <-
        c(
          input$pop_data,
          input$latitude_data,
          input$longitude_data,
          column_range_name
        )
      new_df <- df_local[, cols_to_keep]
      
      # Rename columns
      col_names <- colnames(new_df)
      col_names[2] <- "Latitude"
      col_names[3] <- "Longitude"
      col_names[4:(3 + length(column_range_name))] <-
        column_range_name
      colnames(new_df) <- col_names
      # Convert selected columns to numeric
      cols_to_convert <-
        col_names[which(col_names %in% c("Latitude", "Longitude", col_names[4:(3 + length(range_values))]))]
      new_df[, cols_to_convert] <-
        apply(new_df[, cols_to_convert], 2, as.numeric)
      
      
      
      #### data formating       ####
      if (input$file_format == 1) {
        # Check if "Microsatellite 1 column per allele" is selected
        locus <-
          new_df[, 4:(3 + length(seq(range_values[1], range_values[2])))]
        concatenated_data <-
          concat_identical_cols(locus, input$ploidy)
        df_formated <-
          cbind(new_df[, 1:3], concatenated_data, stringsAsFactors = FALSE)
        # Update df_assigned
        df(df_formated)
        
        ### General stats###
        df_range_cols_markers <- df_formated %>% select( 4:ncol(.) )
        range_cols <-   colnames(df_range_cols_markers)
        # Calculate the number of rows and assign it to a variable 'number_indv'
        number_indv <- nrow(df_formated)
        # Count missing data in the range col_ranges_data
        miss <- paste(as.character(missing_code), "/", as.character(missing_code), sep = "")
        number_missing <- sum(df_range_cols_markers == miss)
        number_missing_per <- (number_missing / (nrow(df_range_cols_markers) * ncol(df_range_cols_markers))) * 100
        formatted_number_missing_per <- sprintf("%.2f%%", number_missing_per)
        
        # Count the number of unique values in pop_data
        number_pop <- nrow(unique(df_formated[1]))
        # Count the number of selected columns in col_ranges_data
        number_marker <- length(df_range_cols_markers)
        
        # Render the results table using renderUI                                # check necessity?
        output$results_table_ui <- renderUI({
          tableOutput("results_table")
        })
        # Render the actual results table
        output$results_table <- renderTable({
          results_table
        })
        # Render info boxes
        output$box_population <-
          renderInfoBox({
            renderInfoBoxUI("Population",
                            number_pop,
                            "map-location-dot",
                            "purple")
          })
        output$box_individuals <-
          renderInfoBox({
            renderInfoBoxUI("Individuals", number_indv, "people-group", "green")
          })
        output$box_marker <-
          renderInfoBox({
            renderInfoBoxUI("Marker", number_marker, "dna", "blue")
          })
        output$box_number_missing_per <-
          renderInfoBox({
            renderInfoBoxUI("Percentage of<br>missing data",
                            formatted_number_missing_per,
                            "database",
                            "yellow")
          })
        
      } else if (input$file_format == 2) {
        # Check if "Microsatellite 1 column for all the alleles" is selected
        # Update df_assigned with new_df
        df(new_df)
      }
      
    } else {
      # Check if latitude is not numeric
      if (is.numeric(input$latitude_data)) {
        shinyalert(title = "Error",
                   text = "Latitude should be numerical, select another column or check your data.",
                   type = "error")
        return()  # Exit the event handler
      }
      if (is.numeric(input$longitude_data)) {
        shinyalert(title = "Error",
                   text = "Longitude should be numerical, select another column or check your data.",
                   type = "error")
        return()  # Exit the event handler
      }
      # Filter columns to keep in the new data frame
      cols_to_keep <-
        c(
          input$pop_data,
          input$latitude_data,
          input$longitude_data,
          column_range_name
        )
      new_df <- df_local[, cols_to_keep]
      
      # Rename columns
      col_names <- colnames(new_df)
      col_names[2] <- "Latitude"
      col_names[3] <- "Longitude"
      col_names[4:(3 + length(column_range_name))] <-
        column_range_name
      colnames(new_df) <- col_names
      # Convert selected columns to numeric
      cols_to_convert <-
        col_names[which(col_names %in% c("Latitude", "Longitude", col_names[4:(3 + length(range_values))]))]
      new_df[, cols_to_convert] <-
        apply(new_df[, cols_to_convert], 2, as.numeric)

      
      
      #### data formating       ####
      if (input$file_format == 1) {
        # Check if "Microsatellite 1 column per allele" is selected
        locus <-
          new_df[, 4:(3 + length(seq(range_values[1], range_values[2])))]
        concatenated_data <-
          concat_identical_cols(locus, input$ploidy)
        df_formated <-
          cbind(new_df[, 1:3], concatenated_data, stringsAsFactors = FALSE)
        # Update df_assigned
        df(df_formated)
        
        ### General stats###
        df_range_cols_markers <- df_formated %>% select( 4:ncol(.) )
        range_cols <-   colnames(df_range_cols_markers)
        # Calculate the number of rows and assign it to a variable 'number_indv'
        number_indv <- nrow(df_formated)
        # Count missing data in the range col_ranges_data
        miss <- paste(as.character(missing_code), "/", as.character(missing_code), sep = "")
        number_missing <- sum(df_range_cols_markers == miss)
        number_missing_per <- (number_missing / (nrow(df_range_cols_markers) * ncol(df_range_cols_markers))) * 100
        formatted_number_missing_per <- sprintf("%.2f%%", number_missing_per)
        
        # Count the number of unique values in pop_data
        number_pop <- nrow(unique(df_formated[1]))
        # Count the number of selected columns in col_ranges_data
        number_marker <- length(df_range_cols_markers)
        
        # Render the results table using renderUI                                # check necessity?
        output$results_table_ui <- renderUI({
          tableOutput("results_table")
        })
        # Render the actual results table
        output$results_table <- renderTable({
          results_table
        })
        # Render info boxes
        output$box_population <-
          renderInfoBox({
            renderInfoBoxUI("Population",
                            number_pop,
                            "map-location-dot",
                            "purple")
          })
        output$box_individuals <-
          renderInfoBox({
            renderInfoBoxUI("Individuals", number_indv, "people-group", "green")
          })
        output$box_marker <-
          renderInfoBox({
            renderInfoBoxUI("Marker", number_marker, "dna", "blue")
          })
        output$box_number_missing_per <-
          renderInfoBox({
            renderInfoBoxUI("Percentage of<br>missing data",
                            formatted_number_missing_per,
                            "database",
                            "yellow")
          })
        
      } else if (input$file_format == 2) {
        # Check if "Microsatellite 1 column for all the alleles" is selected
        # Update df_assigned with new_df
        df(new_df)
      }
      
      ##### MAP ####
      # Create the populationsLL data frame
      populationsLL <- new_df[, 1:3]
      # Group by Locality, Latitude, and Longitude, and calculate Population Size
      populationsLL_grouped <- populationsLL %>%
        group_by_all() %>% count()
      colnames(populationsLL_grouped) <-
        c('Population', 'Longitude', 'Latitude', 'Population size')
      # Render the populationsLL_uniq data frame as a table
      output$populationsLL_uniq_table <- renderTable({
        req(input$run_map)  # Show the table after clicking "Run Map" button
        populationsLL_grouped
      })
      # Render the map
      output$map <- renderLeaflet({
        leaflet(populationsLL_grouped) %>%
          addTiles() %>%
          addCircles(
            lng = populationsLL_grouped$Latitude,
            lat = populationsLL_grouped$Longitude,
            popup = paste(
              "Location:",
              populationsLL_grouped$Population,
              "<br>",
              "Population size:",
              populationsLL_grouped$`Population size`
            ),
            radius = populationsLL_grouped$`Population size` * 50,
            stroke = FALSE,
            fillOpacity = 0.5
          )
      })
      write.csv(new_df, file = "data/filtered_data.csv")
    }
  })
}
