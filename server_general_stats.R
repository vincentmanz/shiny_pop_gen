server_general_stats <- function(input, output, session) {
  
  # Define the reactive expression to hold the data frame
  df_reactive <- reactiveVal(default_df)
  
  # Update the select input choices for "Population" and "Individuals"
  observe({
    req(df_reactive())
    df <- df_reactive()
    
    updateSelectInput(session, 'var1', choices = c("select" = "", colnames(df)))
    updateSelectInput(session, 'var2', choices = c("select" = "", colnames(df)))
  })
  
  # Observer for the 'Run' button for variables
  observeEvent(input$run_var, {
    req(df_reactive(), input$var1, input$var2)
    df <- df_reactive()
    
    pop_col <- input$var1
    ind_col <- input$var2
    
    if (!is.null(pop_col) && !is.null(ind_col)) {
      counts <- df %>%
        group_by_at(vars({{ pop_col }}, {{ ind_col }})) %>%
        summarize(count = n())
      
      output$count_table <- renderTable({
        counts
      })
    }
  })
  
  # Observer for the 'Run' button for column ranges
  observeEvent(input$run_col_ranges, {
    req(df_reactive(), input$col_ranges)
    df <- df_reactive()
    
    col_ranges <- unlist(strsplit(input$col_ranges, ","))
    
    missing_percentages <- numeric(length(col_ranges))
    missing_counts <- integer(length(col_ranges))
    
    for (i in seq_along(col_ranges)) {
      range_str <- trimws(col_ranges[i])
      if (grepl("-", range_str)) {
        range_parts <- as.numeric(strsplit(range_str, "-")[[1]])
        if (length(range_parts) == 2 && all(!is.na(range_parts))) {
          start_col <- range_parts[1]
          end_col <- range_parts[2]
          
          num_zeros_nas <- sum(df[start_col:end_col] == 0 | is.na(df[start_col:end_col]))
          total_cells <- length(df[start_col:end_col])
          missing_percentage <- (num_zeros_nas / total_cells) * 100
          
          missing_percentages[i] <- missing_percentage
          missing_counts[i] <- num_zeros_nas
        }
      } else if (grepl(":", range_str)) {
        range_parts <- as.numeric(strsplit(range_str, ":")[[1]])
        if (length(range_parts) == 2 && all(!is.na(range_parts))) {
          start_col <- range_parts[1]
          end_col <- range_parts[2]
          
          num_zeros_nas <- sum(df[start_col:end_col] == 0 | is.na(df[start_col:end_col]))
          total_cells <- length(df[start_col:end_col])
          missing_percentage <- (num_zeros_nas / total_cells) * 100
          
          missing_percentages[i] <- missing_percentage
          missing_counts[i] <- num_zeros_nas
        }
      }
    }
    
    # Create a data frame for the missing info
    missing_info_df <- data.frame(
      Column_Range = col_ranges,
      Missing_Count = missing_counts,
      Missing_Percentage = missing_percentages
    )
    
    # Render the missing info as HTML code
    output$missing_info <- renderUI({
      HTML(
        paste(
          "<table class='table shiny-table table-spacing-s' style='width:auto;'>",
          "<thead> <tr>",
          "<th style='text-align: left;'> Column_Range </th>",
          "<th style='text-align: right;'> Missing_Count </th>",
          "<th style='text-align: right;'> Missing_Percentage </th>",
          "</tr> </thead>",
          "<tbody>",
          apply(missing_info_df, 1, function(row) {
            paste("<tr>",
                  "<td>", row[1], "</td>",
                  "<td align='right'>", row[2], "</td>",
                  "<td align='right'>", row[3], "</td>",
                  "</tr>", sep = "")
          }),
          "</tbody>",
          "</table>"
        )
      )
    })
    
  })
  
  # outputOptions(output, "missing_info", suspendWhenHidden = FALSE)  # Keep output active
  
}
