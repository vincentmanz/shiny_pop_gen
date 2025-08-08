source("www/helper.R")

server_import_data <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <- reactiveVal(NULL)
    uploaded_file <- reactiveVal(NULL)
    
    # Step 1: File upload
    observeEvent(input$file1, {
      req(input$file1)
      uploaded_file(input$file1$datapath)
    })
    
    # Step 2: Read file
    observeEvent(input$load_user_data, {
      req(uploaded_file())
      df(read.csv(uploaded_file(), header = input$header, sep = input$sep))
    })
    
    # Load default dataset
    observeEvent(input$load_default_data, {
      df(read.csv(
        "https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt",
        header = TRUE, sep = "\t"
      ))
    })
    
    # Filter data
    observeEvent(input$run_filter, {
      req(df())
      if (nzchar(input$exclude_cols)) {
        excluded_cols <- trimws(unlist(strsplit(input$exclude_cols, ",")))
        df(dplyr::select(df(), -tidyselect::any_of(excluded_cols)))
      }
      if (nzchar(input$exclude_rows)) {
        excluded_rows <- as.integer(unlist(strsplit(input$exclude_rows, "[, ]+")))
        df(dplyr::slice(df(), -excluded_rows))
      }
    })
    
    # Update UI inputs
    observe({
      req(df())
      cols <- colnames(df())
      updateSelectInput(session, "pop_data",       choices = c("select" = "", cols))
      updateSelectInput(session, "latitude_data",  choices = c("select" = "", cols))
      updateSelectInput(session, "longitude_data", choices = c("select" = "", cols))
      updateSelectInput(session, "Level1",         choices = c("select" = "", cols))
      updateSelectInput(session, "Level2",         choices = c("select" = "", cols))
      updateSelectInput(session, "Level3",         choices = c("select" = "", cols))
    })
    
    # Default infoBoxes
    output$box_population <- renderInfoBox({
      renderInfoBoxUI("Population", 0, "map-location-dot", "purple")
    })
    output$box_individuals <- renderInfoBox({
      renderInfoBoxUI("Individuals", 0, "people-group", "green")
    })
    output$box_marker <- renderInfoBox({
      renderInfoBoxUI("Marker", 0, "dna", "blue")
    })
    output$box_number_missing_per <- renderInfoBox({
      renderInfoBoxUI(HTML("Percentage of<br>missing data"), 0, "database", "yellow")
    })
    
    # Display uploaded data  (return the df!)
    output$contents <- renderTable({
      req(df()); df()
    })
    
    # Map placeholder
    output$map <- renderLeaflet({ leaflet() %>% addTiles() })
    
    # Download raw data
    output$download_csv <- downloadHandler(
      filename = function() paste0("data-", Sys.Date(), ".csv"),
      content  = function(file) write.csv(df(), file, row.names = FALSE)
    )
    
    # Run metadata assignment
    observeEvent(input$run_assign, {
      req(input$pop_data, input$col_ranges_data, df())
      df_local <- df()
      
      # VALIDATION
      if (!nzchar(input$pop_data)) {
        shinyalert("Error", "You need to select a population column.", type = "error")
        return()
      }
      if (!nzchar(input$missing_code)) {
        shinyalert("Error", "You need to fill the value for missing data.", type = "error")
        return()
      }
      if (!nzchar(input$col_ranges_data)) {
        shinyalert("Error", "Please select the haplotype columns.", type = "error")
        return()
      }
      
      range_values <- as.numeric(unlist(strsplit(input$col_ranges_data, "[:-]")))
      if (any(is.na(range_values)) || range_values[1] < 1 || range_values[2] > ncol(df_local)) {
        shinyalert("Error", "Try again, your range is out of bounds.", type = "error")
        return()
      }
      
      # Prepare temp df to check coordinates
      temp_df <- df_local
      colnames(temp_df)[colnames(temp_df) == input$pop_data] <- "Population"
      if (nzchar(input$latitude_data))
        colnames(temp_df)[colnames(temp_df) == input$latitude_data] <- "Latitude"
      if (nzchar(input$longitude_data))
        colnames(temp_df)[colnames(temp_df) == input$longitude_data] <- "Longitude"
      
      if (all(c("Latitude", "Longitude") %in% colnames(temp_df))) {
        temp_df$Latitude  <- as.numeric(as.character(temp_df$Latitude))
        temp_df$Longitude <- as.numeric(as.character(temp_df$Longitude))
        if (any(is.na(temp_df$Latitude)) || any(is.na(temp_df$Longitude))) {
          shinyalert("Error", "Some Latitude or Longitude values are not numeric. Please correct the data.", type = "error")
          return()
        }
      }
      
      # Extract columns
      column_range_name <- colnames(df_local)[range_values[1]:range_values[2]]
      gps_cols <- character(0)
      if (nzchar(input$latitude_data))  gps_cols <- c(gps_cols, input$latitude_data)
      if (nzchar(input$longitude_data)) gps_cols <- c(gps_cols, input$longitude_data)
      selected_levels <- c(input$Level1, input$Level2, input$Level3)
      selected_levels <- selected_levels[nzchar(selected_levels)]
      cols_to_keep <- c(input$pop_data, gps_cols, selected_levels, column_range_name)
      new_df <- df_local[, cols_to_keep, drop = FALSE]
      
      # Rename columns
      colnames(new_df)[colnames(new_df) == input$pop_data] <- "Population"
      if (nzchar(input$latitude_data))  colnames(new_df)[colnames(new_df) == input$latitude_data]  <- "Latitude"
      if (nzchar(input$longitude_data)) colnames(new_df)[colnames(new_df) == input$longitude_data] <- "Longitude"
      if (nzchar(input$Level1)) colnames(new_df)[colnames(new_df) == input$Level1] <- "Level1"
      if (nzchar(input$Level2)) colnames(new_df)[colnames(new_df) == input$Level2] <- "Level2"
      if (nzchar(input$Level3)) colnames(new_df)[colnames(new_df) == input$Level3] <- "Level3"
      
      # Convert GPS + marker columns to numeric where appropriate
      numeric_cols <- intersect(c("Latitude", "Longitude", column_range_name), colnames(new_df))
      new_df[, numeric_cols] <- lapply(new_df[, numeric_cols, drop = FALSE], as.numeric)
      
      # Format marker data
      if (input$file_format == 1) {
        marker_start <- which(colnames(new_df) == column_range_name[1])
        marker_end   <- which(colnames(new_df) == column_range_name[length(column_range_name)])
        locus <- new_df[, marker_start:marker_end, drop = FALSE]
        concatenated_data <- concat_identical_cols(locus, input$ploidy)
        df_formated <- cbind(new_df[, setdiff(colnames(new_df), column_range_name), drop = FALSE], concatenated_data)
      } else {
        df_formated <- new_df
      }
      
      df(df_formated)  # update reactive
      print(head(df_formated))
      
      # Map
      if (all(c("Latitude", "Longitude") %in% colnames(df_formated))) {
        populationsLL <- df_formated[, c("Population", "Longitude", "Latitude")]
        populationsLL_grouped <- dplyr::count(populationsLL, dplyr::across(dplyr::everything()))
        colnames(populationsLL_grouped) <- c("Population", "Longitude", "Latitude", "Population size")
        
        output$map <- renderLeaflet({
          leaflet(populationsLL_grouped) %>%
            addTiles() %>%
            addCircles(
              lng = ~Longitude, lat = ~Latitude,
              popup = ~paste0("Location: ", Population,
                              "<br>Population size: ", `Population size`),
              radius = ~`Population size` * 50,
              stroke = FALSE, fillOpacity = 0.5
            )
        })
      }
      
      # Stats box
      marker_cols <- df_formated[, vapply(df_formated[1, ], function(x) grepl("/", x), logical(1L))]
      number_indv <- nrow(df_formated)
      miss_val <- paste(input$missing_code, input$missing_code, sep = "/")
      number_missing <- sum(marker_cols == miss_val)
      number_missing_per <- sprintf("%.2f%%", (number_missing / (nrow(marker_cols) * ncol(marker_cols))) * 100)
      number_pop <- length(unique(df_formated$Population))
      number_marker <- ncol(marker_cols)
      
      output$box_population <- renderInfoBox({ renderInfoBoxUI("Population",  number_pop, "map-location-dot", "purple") })
      output$box_individuals <- renderInfoBox({ renderInfoBoxUI("Individuals", number_indv, "people-group",     "green")  })
      output$box_marker <- renderInfoBox({ renderInfoBoxUI("Marker", number_marker, "dna", "blue") })
      output$box_number_missing_per <- renderInfoBox({
        renderInfoBoxUI(HTML("Percentage of<br>missing data"), number_missing_per, "database", "yellow")
      })
      
      # Save data
      dir.create("data", showWarnings = FALSE)
      write.csv(new_df, file = "data/filtered_data.csv", row.names = FALSE)
      formatted_data <- list(
        Population   = df_formated$Population,
        haplotype    = marker_cols,
        marker       = colnames(marker_cols),
        missing_code = input$missing_code,
        GPS          = if (all(c("Longitude","Latitude") %in% colnames(df_formated))) df_formated[, c("Longitude","Latitude")] else NULL,
        level1       = if ("Level1" %in% colnames(df_formated)) df_formated$Level1 else NULL,
        level2       = if ("Level2" %in% colnames(df_formated)) df_formated$Level2 else NULL,
        level3       = if ("Level3" %in% colnames(df_formated)) df_formated$Level3 else NULL
      )
      formatted_data$GPS <- formatted_data$GPS[, c("Latitude", "Longitude")]
      save(formatted_data, file = "data/formatted_data.RData")
    })
  })
}
