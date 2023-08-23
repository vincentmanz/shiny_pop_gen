server_import_data <- function(input, output, session) {
  

  df <- reactiveVal(default_df)
  
  # Input: Button for choosing default data
  observeEvent(input$use_default_data, {
    isolate({
      df(default_df)
      updateFileInput(session, "file1", label = "Choose CSV File", value = "")
    })
  })
  
  # Input: File upload
  observeEvent(input$file1, {
    req(input$file1)
    df(read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote))
  })
  
  observeEvent(input$run_filter, {
    req(df())
    
    # Excluding columns
    excluded_cols <- unlist(strsplit(input$exclude_cols, ","))
    excluded_cols <- trimws(excluded_cols)
    
    if (!is.null(excluded_cols) && length(excluded_cols) > 0 && any(excluded_cols != "")) {
      df_filtered <- df() %>%
        select(-any_of(trimws(excluded_cols)))  # Trim whitespace around column names
      df(df_filtered)
    
    }
  })
  
  
  output$contents <- renderTable({
    req(df())
    
    if (input$disp == "head") {
      return(head(df()))
    } else {
      return(df())
    }
  })
  
}
