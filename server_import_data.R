server_import_data <- function(input, output, session) {
  
  
  # Data
  loaded_data <- reactiveVal()
  default_display_mode <- reactiveVal("head")  # Initialize default display mode
  
  # Input: Load default data button
  observeEvent(input$load_default_data, {
    df(default_df)
    default_df_display("head")  # Reset display mode to "Head"
    updateFileInput(session, "file1", label = "Choose CSV File", value = "")  # Reset file input
  })
  
  # Input: File upload
  observeEvent(input$file1, {
    req(input$file1)
    df(read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote))
  })
  
  output$contents <- renderTable({
    req(df())
    
    if (default_df_display() == "head") {
      return(head(df()))
    } else {
      return(df())
    }
  })
}