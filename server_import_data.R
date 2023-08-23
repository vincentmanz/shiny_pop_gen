default_df <- readr::read_tsv("https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt")

server_import_data <- function(input, output, session) {
  

  df <- reactiveVal(default_df)
  
  ## Input: Button for loading default data
  observeEvent(input$load_default_data, {
    df(read.csv("https://www.t-de-meeus.fr/Enseign/BoophilusAdultsDataCattle.txt",
                header = input$header,
                sep = input$sep,
                quote = input$quote))
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
  
  
  output$contents <- renderTable({
    req(df())
    
    if (input$disp == "head") {
      return(head(df()))
    } else {
      return(df())
    }
  })
  
}
