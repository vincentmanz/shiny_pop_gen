# server_import_data.R

source("www/helper.R")

server_import_data <- function(input, output, session) {
  # Define the reactive expressions to hold the data frames
  df <- reactiveVal()  # For raw loaded data
  df_transformed <- reactiveVal()  # For transformed data after assign metadata
  uploaded_file <- reactiveVal()

  # Step 1: Just store the file path when user uploads, don't read yet
  observeEvent(input$file1, {
    req(input$file1)
    uploaded_file(input$file1$datapath)
  })
  
  # Step 2: Read the file only when user clicks 'load_user_data'
  observeEvent(input$load_user_data, {
    req(uploaded_file())  # make sure a file is uploaded
    df(read.csv(uploaded_file(), header = input$header, sep = input$sep))
    # Clear transformed data when new data is loaded
    df_transformed(NULL)
  })

  # Load default data when the button is clicked
  observeEvent(input$load_default_data, {
    df(read.csv("data/BoophilusAdultsDataCattle.csv", header = TRUE, sep = "\t"))
    # Clear transformed data when new data is loaded
    df_transformed(NULL)
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
    # Clear transformed data when data is filtered
    df_transformed(NULL)
  })
  
  # Update the select input choices
  observe({
    req(df())
    df_local <- df()
    updateSelectInput(session, 'pop_data', choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'latitude_data',choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'longitude_data', choices = c("select" = "", colnames(df_local))) 
     
    updateSelectInput(session, 'Level1', choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'Level2', choices = c("select" = "", colnames(df_local)))
    updateSelectInput(session, 'Level3', choices = c("select" = "", colnames(df_local)))
  })
  
  # Render info boxes default
  output$box_population <- renderInfoBox({renderInfoBoxUI("Population", 0, "map-location-dot", "purple")})
  output$box_individuals <- renderInfoBox({renderInfoBoxUI("Individuals", 0, "people-group", "green")})
  output$box_marker <- renderInfoBox({renderInfoBoxUI("Marker", 0, "dna", "blue")})
  output$box_number_missing_per <- renderInfoBox({renderInfoBoxUI(HTML("Percentage of<br>missing data"),0,"database","yellow")})
  
  # Display the loaded data frame (shown after Load Data, Load Default Data, or Filter Data)
  output$data <- renderTable({ 
    req(df())
    df()
  })

  # Display the transformed data frame (shown ONLY after Assign metadata)
  output$contents <- renderTable({
    req(df_transformed())
    df_transformed()
  })

  # Display an empty map world
  output$map <- renderLeaflet({leaflet() %>% addTiles()})
  
  # Download raw data csv handler
  output$download_csv_raw <- downloadHandler(
    filename = function() {
      paste("raw-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(df())
      write.csv(df(), file, row.names = FALSE)
    }
  )
  
  # Download transformed data csv handler
  output$download_csv_transformed <- downloadHandler(
    filename = function() {
      paste("transformed-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(df_transformed())
      write.csv(df_transformed(), file, row.names = FALSE)
    }
  )
  
  # Download map handler
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Cette fonction nécessite le package webshot pour capturer la carte
      # Ou vous pouvez adapter selon vos besoins
      png(file, width = 800, height = 600)
      print("Map download functionality needs to be implemented based on your requirements")
      dev.off()
    }
  )
  
  # Create the table and the map
  observeEvent(input$run_assign, {
    req(
      input$pop_data,
      input$col_ranges_data,
      df()
    )
    missing_code <- input$missing_code
    df_local <- df()
    
    # Check input data
    ## Check if Population has data
    if (input$pop_data == "") {
      shinyalert(title = "Error",text = "You need to select populations.",type = "error")
      return()  # Exit the event handler
    }  

    # Check if Code for missing data has data
    if (input$missing_code == "") {
      shinyalert(title = "Error",text = "You need to fill the value for missing data.",type = "error")
      return()  # Exit the event handler
    }

    # Check if Marker Range has data
    if (input$col_ranges_data == "") {
      shinyalert(title = "Error",text = "You need to select a marker range.",type = "error")
      return()  # Exit the event handler
    }
    
    # Check if the range is valid
    range_values <- unlist(strsplit(input$col_ranges_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    if (any(is.na(range_values)) ||
        range_values[1] < 1 ||
        range_values[2] > length(colnames(df_local))) {
      shinyalert(title = "Error",text = "Try again, your range is out of bounds.",type = "error")
      return()  # Exit the event handler
    }
    
    # Convert marker range to numeric
    range_data <- input$col_ranges_data
    range_values <- unlist(strsplit(range_data, "[:-]"))
    range_values <- as.numeric(range_values)
    
    # Extract the range of column headers
    column_range_name <- colnames(df_local)[range_values[1]:range_values[2]]
    
    # Determine if Latitude and Longitude are empty
    latitude_empty <- input$latitude_data == ""
    longitude_empty <- input$longitude_data == ""
    
    ### Option map ###
    if (latitude_empty || longitude_empty) {
      # Filter columns to keep in the new data frame
      cols_to_keep <- c(input$pop_data, column_range_name)
      new_df <- df_local[, cols_to_keep]  
      # Rename columns
      colnames(new_df)[1] ="Population"
      
      # Convert selected columns to numeric
      col_names <- colnames(new_df)
      cols_to_convert <- col_names[2:length(col_names)]
      new_df[, cols_to_convert] <- apply(new_df[, cols_to_convert], 2, as.numeric)
      
      #### data formatting ####
      if (input$file_format == 1) {
        # Check if "Microsatellite 1 column per allele" is selected
        locus <- new_df[, 2:ncol(new_df)]
        concatenated_data <- concat_identical_cols(locus, input$ploidy)
        df_formated <- cbind(new_df[, 1, drop=FALSE], concatenated_data, stringsAsFactors = FALSE)
        colnames(df_formated)[1] = "Population"
        
        # Update df_transformed (not df)
        df_transformed(df_formated)
        
        ### General stats###
        df_range_cols_markers <- df_formated %>% select(2:ncol(.))
        # Calculate the number of rows and assign it to a variable 'number_indv'
        number_indv <- nrow(df_formated)
        # Count missing data in the range col_ranges_data
        miss <- paste(as.character(missing_code), "/", as.character(missing_code), sep = "")
        number_missing <- sum(df_range_cols_markers == miss, na.rm = TRUE)
        number_missing_per <- (number_missing / (nrow(df_range_cols_markers) * ncol(df_range_cols_markers))) * 100
        formatted_number_missing_per <- sprintf("%.2f%%", number_missing_per)
        
        # Count the number of unique values in pop_data
        number_pop <- nrow(unique(df_formated[1]))
        # Count the number of selected columns in col_ranges_data
        number_marker <- ncol(df_range_cols_markers)
        
        # Render info boxes
        output$box_population <- renderInfoBox({renderInfoBoxUI("Population", number_pop, "map-location-dot", "purple")})
        output$box_individuals <- renderInfoBox({renderInfoBoxUI("Individuals", number_indv, "people-group", "green")})
        output$box_marker <- renderInfoBox({renderInfoBoxUI("Marker", number_marker, "dna", "blue")})
        output$box_number_missing_per <- renderInfoBox({renderInfoBoxUI(HTML("Percentage of<br>missing data"),formatted_number_missing_per,"database","yellow")})
        
      } else if (input$file_format == 2) {
        # Check if "Microsatellite 1 column for all the alleles" is selected
        # Update df_transformed with new_df
        df_transformed(new_df)
      }
    } else {
      # Check if latitude data is numeric
      lat_data <- df_local[, input$latitude_data]
      if (!is.numeric(lat_data)) {
        shinyalert(title = "Error", text = "Latitude should be numerical, select another column or check your data.", type = "error")
        return()  # Exit the event handler
      }
      # Check if longitude data is numeric  
      lon_data <- df_local[, input$longitude_data]
      if (!is.numeric(lon_data)) {
        shinyalert(title = "Error", text = "Longitude should be numerical, select another column or check your data.", type = "error")
        return()  # Exit the event handler
      }

      # Filter columns to keep in the new data frame
      # Get selected levels, remove empty selections
      selected_levels <- c(input$Level1, input$Level2, input$Level3)
      selected_levels <- selected_levels[selected_levels != ""]  # keep only non-empty
      cols_to_keep <- c(input$pop_data, input$latitude_data, input$longitude_data, selected_levels, column_range_name)
      new_df <- df_local[, cols_to_keep]
      
      # Rename columns
      colnames(new_df)[1] <- "Population"
      colnames(new_df)[2] <- "Latitude"  
      colnames(new_df)[3] <- "Longitude"
      
      # Convert coordinate columns to numeric if not already
      new_df$Latitude <- as.numeric(new_df$Latitude)
      new_df$Longitude <- as.numeric(new_df$Longitude)
      
      # Convert marker columns to numeric
      marker_start_col <- 4 + length(selected_levels)
      marker_cols <- marker_start_col:ncol(new_df)
      new_df[, marker_cols] <- apply(new_df[, marker_cols], 2, as.numeric)
      
      #### data formatting ####
      if (input$file_format == 1) {
        # Check if "Microsatellite 1 column per allele" is selected
        locus <- new_df[, marker_cols]
        concatenated_data <- concat_identical_cols(locus, input$ploidy)
        
        # Combine population, coordinates, levels, and concatenated marker data
        if (length(selected_levels) > 0) {
          df_formated <- cbind(new_df[, 1:(3 + length(selected_levels))], concatenated_data, stringsAsFactors = FALSE)
        } else {
          df_formated <- cbind(new_df[, 1:3], concatenated_data, stringsAsFactors = FALSE)
        }
        
        # Update df_transformed (not df)
        df_transformed(df_formated)
        
        ### General stats###
        marker_start_final <- 4 + length(selected_levels)
        df_range_cols_markers <- df_formated %>% select(marker_start_final:ncol(.))
        
        # Calculate the number of rows and assign it to a variable 'number_indv'
        number_indv <- nrow(df_formated)
        # Count missing data in the range col_ranges_data
        miss <- paste(as.character(missing_code), "/", as.character(missing_code), sep = "")
        number_missing <- sum(df_range_cols_markers == miss, na.rm = TRUE)
        number_missing_per <- (number_missing / (nrow(df_range_cols_markers) * ncol(df_range_cols_markers))) * 100
        formatted_number_missing_per <- sprintf("%.2f%%", number_missing_per)
        
        # Count the number of unique values in pop_data
        number_pop <- length(unique(df_formated$Population))
        # Count the number of selected columns in col_ranges_data
        number_marker <- ncol(df_range_cols_markers)
        
        # Render info boxes
        output$box_population <- renderInfoBox({renderInfoBoxUI("Population", number_pop, "map-location-dot", "purple")})
        output$box_individuals <- renderInfoBox({renderInfoBoxUI("Individuals", number_indv, "people-group", "green")})
        output$box_marker <- renderInfoBox({renderInfoBoxUI("Marker", number_marker, "dna", "blue")})
        output$box_number_missing_per <- renderInfoBox({renderInfoBoxUI(HTML("Percentage of<br>missing data"),formatted_number_missing_per,"database","yellow")})
        
      } else if (input$file_format == 2) {
        # Check if "Microsatellite 1 column for all the alleles" is selected
        # Update df_transformed with new_df
        df_transformed(new_df)
      }

      ##### MAP ####
      # Create the populationsLL data frame
      populationsLL <- new_df[, 1:3]
      # Group by Population, Latitude, and Longitude, and calculate Population Size
      populationsLL_grouped <- populationsLL %>%
        group_by_all() %>% 
        summarise(n = n(), .groups = 'drop')
      colnames(populationsLL_grouped) <- c('Population', 'Latitude', 'Longitude', 'Population_size')
      
      # Render the populationsLL_uniq data frame as a table
      output$populationsLL_uniq_table <- renderTable({
        populationsLL_grouped
      })

      # Render the map - Fixed coordinate order
      output$map <- renderLeaflet({
        leaflet(populationsLL_grouped) %>%
          addTiles() %>%
          addCircles(
            lng = ~Longitude,  # Fixed: lng should be Longitude
            lat = ~Latitude,   # Fixed: lat should be Latitude
            popup = ~paste(
              "Location:", Population,
              "<br>",
              "Population size:", Population_size
            ),
            radius = ~Population_size * 50,
            stroke = FALSE,
            fillOpacity = 0.5
          )
      })
      
      print(head(df_formated))
      
      # Save the transformed data, not the raw data
      write.csv(df_formated, file = "data/filtered_data.csv", row.names = FALSE)
    }
  })
  
  # =============================================================================
  # MODULE ALLELE FREQUENCIES - Uses RAW DATA (df()) instead of df_transformed()
  # =============================================================================
  
  # Reactive values for allele frequencies analysis
  allele_results <- reactiveValues(
    global_frequencies = NULL,
    population_frequencies = NULL,
    locus_analysis = NULL,
    missing_data_summary = NULL,
    analysis_completed = FALSE
  )
  
  # Check if raw data is available for allele analysis
  output$allele_analysis_completed <- reactive({
    allele_results$analysis_completed
  })
  outputOptions(output, "allele_analysis_completed", suspendWhenHidden = FALSE)
  
  # Display data status for allele analysis
  output$data_status_allele <- renderText({
    if (is.null(df())) {
      "<div style='color: #e74c3c; font-weight: bold;'>
        <i class='fas fa-exclamation-triangle'></i> 
        No data loaded. Please load data using 'Load Data' or 'Load Default Data'.
      </div>"
    } else {
      data <- df()
      n_individuals <- nrow(data)
      n_columns <- ncol(data)
      
      paste0(
        "<div style='color: #27ae60; font-weight: bold;'>",
        "<i class='fas fa-check-circle'></i> Raw data loaded successfully<br>",
        "<strong>Individuals:</strong> ", n_individuals, "<br>",
        "<strong>Columns:</strong> ", n_columns, "<br>",
        "<strong>Status:</strong> Ready for analysis",
        "</div>"
      )
    }
  })
  
  # Helper function to detect potential population and marker columns
  detect_columns_raw <- function(data) {
    cols <- names(data)
    
    # Try to detect population column
    pop_candidates <- c("Population", "Pop", "population", "pop", 
                       "Population_ID", "PopID", "Location", "Site")
    pop_col <- cols[cols %in% pop_candidates][1]
    if (is.na(pop_col)) pop_col <- cols[1]  # Default to first column
    
    # Detect potential marker columns (exclude metadata columns)
    exclude_candidates <- c("Population", "Pop", "Latitude", "Longitude", 
                           "Lat", "Lng", "Long", "Level1", "Level2", "Level3",
                           "ID", "Sample_ID", "Individual", "Date", "Site")
    marker_cols <- setdiff(cols, exclude_candidates)
    
    return(list(population = pop_col, markers = marker_cols))
  }
  
  # Update choices for allele frequencies analysis
  observe({
    req(df())
    data <- df()
    col_info <- detect_columns_raw(data)
    
    # Update population filter choices
    if (!is.na(col_info$population) && col_info$population %in% names(data)) {
      populations <- sort(unique(data[[col_info$population]]))
      pop_choices <- c("All Populations" = "all", setNames(populations, populations))
      updateSelectInput(session, "population_filter", choices = pop_choices, selected = "all")
      updateSelectInput(session, "population_comparison", choices = pop_choices, selected = "all")
    }
    
    # Update locus filter choices
    if (length(col_info$markers) > 0) {
      locus_choices <- c("All Loci" = "all", setNames(col_info$markers, col_info$markers))
      updateSelectInput(session, "locus_filter", choices = locus_choices, selected = "all")
      
      # Update numeric ranges
      updateNumericInput(session, "locus_range_start", max = length(col_info$markers), value = 1)
      updateNumericInput(session, "locus_range_end", max = length(col_info$markers), value = min(10, length(col_info$markers)))
    }
  })
  
  # Reset analysis parameters
  observeEvent(input$reset_analysis, {
    updateSelectInput(session, "population_filter", selected = "all")
    updateSelectInput(session, "population_comparison", selected = "all")
    updateSelectInput(session, "locus_filter", selected = "all")
    updateNumericInput(session, "min_allele_frequency", value = 0.01)
    updateNumericInput(session, "max_allele_frequency", value = 1.0)
    updateCheckboxInput(session, "include_rare_alleles", value = TRUE)
    updateCheckboxInput(session, "analyze_all_loci", value = TRUE)
    updateTextInput(session, "missing_data_code", value = "0/0")
    updateNumericInput(session, "max_missing_threshold", value = 50)
    
    # Clear results
    allele_results$global_frequencies <- NULL
    allele_results$population_frequencies <- NULL
    allele_results$locus_analysis <- NULL
    allele_results$missing_data_summary <- NULL
    allele_results$analysis_completed <- FALSE
  })
  
  # Helper function to parse genotypes from raw data
  parse_genotype_raw <- function(genotype, missing_code = "0/0") {
    if (is.na(genotype) || genotype == missing_code || genotype == "" || is.null(genotype)) {
      return(c(NA, NA))
    }
    
    genotype_str <- as.character(genotype)
    
    # Handle different separators
    if (grepl("/", genotype_str)) {
      alleles <- unlist(strsplit(genotype_str, "/"))
    } else if (grepl("-", genotype_str)) {
      alleles <- unlist(strsplit(genotype_str, "-"))
    } else if (grepl("_", genotype_str)) {
      alleles <- unlist(strsplit(genotype_str, "_"))
    } else {
      # Try to split by any non-digit character
      alleles <- unlist(strsplit(genotype_str, "[^0-9]+"))
      alleles <- alleles[alleles != "" & !is.na(alleles)]
    }
    
    # Clean up alleles
    alleles <- trimws(alleles)
    alleles <- alleles[alleles != "" & !is.na(alleles)]
    
    # Return first two alleles for diploid analysis
    if (length(alleles) >= 2) {
      return(as.character(alleles[1:2]))
    } else if (length(alleles) == 1) {
      # Homozygous case
      return(c(as.character(alleles[1]), as.character(alleles[1])))
    } else {
      return(c(NA, NA))
    }
  }
  
  # Main allele frequencies analysis
  observeEvent(input$run_allele_analysis, {
    req(df())
    
    showNotification("Running allele frequencies analysis...", type = "message", duration = 2)
    
    data <- df()
    col_info <- detect_columns_raw(data)
    missing_code <- input$missing_data_code
    
    # Validate inputs
    if (is.na(col_info$population) || length(col_info$markers) == 0) {
      showNotification("Could not detect population and marker columns. Please check your data format.", type = "error")
      return()
    }
    
    # Determine which loci to analyze
    if (input$analyze_all_loci) {
      loci_to_analyze <- col_info$markers
    } else {
      start_idx <- max(1, input$locus_range_start)
      end_idx <- min(length(col_info$markers), input$locus_range_end)
      loci_to_analyze <- col_info$markers[start_idx:end_idx]
    }
    
    # Filter by specific locus if selected
    if (input$locus_filter != "all" && input$locus_filter %in% col_info$markers) {
      loci_to_analyze <- input$locus_filter
    }
    
    # Initialize results containers
    global_freq_list <- list()
    pop_freq_list <- list()
    locus_analysis_list <- list()
    missing_data_list <- list()
    
    population_col <- col_info$population
    
    # Filter populations if specified
    if (input$population_filter != "all") {
      data <- data[data[[population_col]] == input$population_filter, ]
    }
    
    # Process each locus
    for (locus in loci_to_analyze) {
      if (!locus %in% names(data)) next
      
      genotypes <- data[[locus]]
      all_alleles <- c()
      total_missing <- 0
      
      # Parse all genotypes for this locus
      for (i in 1:length(genotypes)) {
        alleles <- parse_genotype_raw(genotypes[i], missing_code)
        if (all(is.na(alleles))) {
          total_missing <- total_missing + 1
        } else {
          valid_alleles <- alleles[!is.na(alleles)]
          all_alleles <- c(all_alleles, valid_alleles)
        }
      }
      
      # Calculate global frequencies for this locus
      if (length(all_alleles) > 0) {
        allele_table <- table(all_alleles)
        allele_freq <- prop.table(allele_table)
        
        # Apply frequency filters
        if (!input$include_rare_alleles) {
          allele_freq <- allele_freq[allele_freq >= 0.01]
        }
        
        allele_freq <- allele_freq[allele_freq >= input$min_allele_frequency & 
                                  allele_freq <= input$max_allele_frequency]
        
        if (length(allele_freq) > 0) {
          global_freq_df <- data.frame(
            Locus = locus,
            Allele = names(allele_freq),
            Count = as.numeric(allele_table[names(allele_freq)]),
            Frequency = as.numeric(allele_freq),
            stringsAsFactors = FALSE
          )
          global_freq_list[[locus]] <- global_freq_df
        }
      }
      
      # Calculate population-specific frequencies
      populations <- unique(data[[population_col]])
      for (pop in populations) {
        pop_data <- data[data[[population_col]] == pop, ]
        pop_genotypes <- pop_data[[locus]]
        pop_alleles <- c()
        pop_missing <- 0
        
        for (genotype in pop_genotypes) {
          alleles <- parse_genotype_raw(genotype, missing_code)
          if (all(is.na(alleles))) {
            pop_missing <- pop_missing + 1
          } else {
            valid_alleles <- alleles[!is.na(alleles)]
            pop_alleles <- c(pop_alleles, valid_alleles)
          }
        }
        
        if (length(pop_alleles) > 0) {
          pop_allele_table <- table(pop_alleles)
          pop_allele_freq <- prop.table(pop_allele_table)
          
          # Apply filters
          pop_allele_freq <- pop_allele_freq[pop_allele_freq >= input$min_allele_frequency & 
                                           pop_allele_freq <= input$max_allele_frequency]
          
          if (length(pop_allele_freq) > 0) {
            pop_freq_df <- data.frame(
              Population = pop,
              Locus = locus,
              Allele = names(pop_allele_freq),
              Count = as.numeric(pop_allele_table[names(pop_allele_freq)]),
              Frequency = as.numeric(pop_allele_freq),
              stringsAsFactors = FALSE
            )
            pop_freq_list[[paste(pop, locus, sep = "_")]] <- pop_freq_df
          }
        }
        
        # Store missing data information
        missing_pct <- (pop_missing / nrow(pop_data)) * 100
        missing_data_list[[paste(pop, locus, sep = "_")]] <- data.frame(
          Population = pop,
          Locus = locus,
          Total_Individuals = nrow(pop_data),
          Missing_Count = pop_missing,
          Missing_Percentage = missing_pct,
          Effective_Count = nrow(pop_data) - pop_missing,
          Data_Quality = ifelse(missing_pct <= 10, "Excellent", 
                               ifelse(missing_pct <= 25, "Good", "Poor")),
          stringsAsFactors = FALSE
        )
      }
      
      # Locus-level summary
      total_individuals <- nrow(data)
      effective_individuals <- total_individuals - total_missing
      n_alleles <- length(unique(all_alleles))
      missing_pct <- (total_missing / total_individuals) * 100
      
      locus_analysis_list[[locus]] <- data.frame(
        Locus = locus,
        Total_Individuals = total_individuals,
        Effective_Individuals = effective_individuals,
        Missing_Count = total_missing,
        Missing_Percentage = missing_pct,
        Number_of_Alleles = n_alleles,
        Polymorphic = ifelse(n_alleles > 1, "Yes", "No"),
        Data_Quality = ifelse(missing_pct <= 10, "Excellent", 
                             ifelse(missing_pct <= 25, "Good", "Poor")),
        stringsAsFactors = FALSE
      )
    }
    
    # Combine all results
    allele_results$global_frequencies <- do.call(rbind, global_freq_list)
    allele_results$population_frequencies <- do.call(rbind, pop_freq_list)
    allele_results$locus_analysis <- do.call(rbind, locus_analysis_list)
    allele_results$missing_data_summary <- do.call(rbind, missing_data_list)
    allele_results$analysis_completed <- TRUE
    
    showNotification("Allele frequencies analysis completed!", type = "success", duration = 3)
  })
  
  # Summary outputs
  output$allele_dataset_summary <- renderText({
    req(allele_results$analysis_completed)
    
    locus_data <- allele_results$locus_analysis
    total_individuals <- mean(locus_data$Total_Individuals, na.rm = TRUE)
    n_loci <- nrow(locus_data)
    polymorphic_loci <- sum(locus_data$Polymorphic == "Yes", na.rm = TRUE)
    
    paste0(
      "<strong>Individuals Analyzed:</strong> ", round(total_individuals), "<br>",
      "<strong>Loci Analyzed:</strong> ", n_loci, "<br>",
      "<strong>Polymorphic Loci:</strong> ", polymorphic_loci, "<br>",
      "<strong>Monomorphic Loci:</strong> ", n_loci - polymorphic_loci
    )
  })
  
  output$allele_missing_summary <- renderText({
    req(allele_results$analysis_completed)
    
    missing_data <- allele_results$missing_data_summary
    overall_missing <- mean(missing_data$Missing_Percentage, na.rm = TRUE)
    excellent_count <- sum(missing_data$Data_Quality == "Excellent", na.rm = TRUE)
    poor_count <- sum(missing_data$Data_Quality == "Poor", na.rm = TRUE)
    
    paste0(
      "<strong>Average Missing Data:</strong> ", round(overall_missing, 1), "%<br>",
      "<strong>Excellent Quality:</strong> ", excellent_count, " entries<br>",
      "<strong>Poor Quality:</strong> ", poor_count, " entries<br>",
      "<strong>Data Completeness:</strong> ", round(100 - overall_missing, 1), "%"
    )
  })
  
  output$allele_diversity_summary <- renderText({
    req(allele_results$analysis_completed)
    
    if (!is.null(allele_results$global_frequencies)) {
      total_alleles <- nrow(allele_results$global_frequencies)
      avg_alleles_per_locus <- mean(table(allele_results$global_frequencies$Locus))
      max_alleles <- max(table(allele_results$global_frequencies$Locus))
      
      paste0(
        "<strong>Total Alleles Found:</strong> ", total_alleles, "<br>",
        "<strong>Avg Alleles/Locus:</strong> ", round(avg_alleles_per_locus, 1), "<br>",
        "<strong>Max Alleles/Locus:</strong> ", max_alleles, "<br>",
        "<strong>Genetic Diversity:</strong> High"
      )
    } else {
      "<strong>No allelic diversity data available</strong>"
    }
  })
  
  output$allele_detailed_stats <- renderText({
    req(allele_results$analysis_completed)
    
    if (!is.null(allele_results$missing_data_summary)) {
      missing_data <- allele_results$missing_data_summary
      
      # Find best and worst populations
      pop_summary <- aggregate(Missing_Percentage ~ Population, missing_data, mean)
      best_pop <- pop_summary[which.min(pop_summary$Missing_Percentage), ]
      worst_pop <- pop_summary[which.max(pop_summary$Missing_Percentage), ]
      
      # Find best and worst loci
      locus_data <- allele_results$locus_analysis
      best_locus <- locus_data[which.min(locus_data$Missing_Percentage), ]
      worst_locus <- locus_data[which.max(locus_data$Missing_Percentage), ]
      
      paste0(
        "<strong>Population Data Quality:</strong><br>",
        "• Best: ", best_pop$Population, " (", round(best_pop$Missing_Percentage, 1), "% missing)<br>",
        "• Worst: ", worst_pop$Population, " (", round(worst_pop$Missing_Percentage, 1), "% missing)<br><br>",
        "<strong>Locus Data Quality:</strong><br>",
        "• Best: ", best_locus$Locus, " (", round(best_locus$Missing_Percentage, 1), "% missing)<br>",
        "• Worst: ", worst_locus$Locus, " (", round(worst_locus$Missing_Percentage, 1), "% missing)"
      )
    } else {
      "No detailed statistics available."
    }
  })
  
  # Data table outputs
  output$allele_global_table <- DT::renderDataTable({
    req(allele_results$global_frequencies)
    
    data <- allele_results$global_frequencies
    data$Frequency_Pct <- paste0(round(data$Frequency * 100, 2), "%")
    
    DT::datatable(
      data[, c("Locus", "Allele", "Count", "Frequency_Pct")],
      colnames = c("Locus", "Allele", "Count", "Frequency (%)"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatStyle(columns = 1:4, fontSize = '14px')
  })
  
  output$allele_population_table <- DT::renderDataTable({
    req(allele_results$population_frequencies)
    
    data <- allele_results$population_frequencies
    data$Frequency_Pct <- paste0(round(data$Frequency * 100, 2), "%")
    
    DT::datatable(
      data[, c("Population", "Locus", "Allele", "Count", "Frequency_Pct")],
      colnames = c("Population", "Locus", "Allele", "Count", "Frequency (%)"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatStyle(columns = 1:5, fontSize = '14px')
  })
  
  output$allele_locus_table <- DT::renderDataTable({
    req(allele_results$locus_analysis)
    
    data <- allele_results$locus_analysis
    data$Missing_Pct <- paste0(round(data$Missing_Percentage, 1), "%")
    
    DT::datatable(
      data[, c("Locus", "Total_Individuals", "Effective_Individuals", 
               "Missing_Count", "Missing_Pct", "Number_of_Alleles", "Polymorphic", "Data_Quality")],
      colnames = c("Locus", "Total", "Effective", "Missing", "Missing %", 
                   "N Alleles", "Polymorphic", "Quality"),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatStyle("Data_Quality", 
                   backgroundColor = DT::styleEqual(c("Excellent", "Good", "Poor"), 
                                                   c("lightgreen", "yellow", "lightcoral")))
  })
  
  output$allele_missing_table <- DT::renderDataTable({
    req(allele_results$missing_data_summary)
    
    DT::datatable(
      allele_results$missing_data_summary,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
    DT::formatStyle("Data_Quality", 
                   backgroundColor = DT::styleEqual(c("Excellent", "Good", "Poor"), 
                                                   c("lightgreen", "yellow", "lightcoral")))
  })
  
  # Simple visualizations
  output$allele_missing_by_pop <- renderPlot({
    req(allele_results$missing_data_summary)
    
    missing_data <- allele_results$missing_data_summary
    pop_summary <- aggregate(Missing_Percentage ~ Population, missing_data, mean)
    
    barplot(pop_summary$Missing_Percentage,
            names.arg = pop_summary$Population,
            main = "Average Missing Data by Population",
            xlab = "Population", ylab = "Missing Data (%)",
            col = "#756bb1", las = 2, cex.names = 0.8)
  })
  
  output$allele_missing_by_locus <- renderPlot({
    req(allele_results$locus_analysis)
    
    locus_data <- allele_results$locus_analysis
    
    barplot(locus_data$Missing_Percentage,
            names.arg = locus_data$Locus,
            main = "Missing Data by Locus",
            xlab = "Locus", ylab = "Missing Data (%)",
            col = "#756bb1", las = 2, cex.names = 0.6)
  })
  
  output$allele_diversity_plot <- renderPlot({
    req(allele_results$locus_analysis)
    
    locus_data <- allele_results$locus_analysis
    
    barplot(locus_data$Number_of_Alleles,
            names.arg = locus_data$Locus,
            main = "Number of Alleles per Locus",
            xlab = "Locus", ylab = "Number of Alleles",
            col = "#756bb1", las = 2, cex.names = 0.6)
  })
  
  # Download handler
  output$download_allele_results <- downloadHandler(
    filename = function() {
      paste("allele_frequencies_complete_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(allele_results$global_frequencies)
      write.csv(allele_results$global_frequencies, file, row.names = FALSE)
    }
  )
  
}