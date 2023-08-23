server_import_data <- function(input, output, session) {
  
  ##### Data ####
  
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
  
  output$contents <- renderTable({
    req(df())
    
    if (input$disp == "head") {
      return(head(df()))
    } else {
      return(df())
    }
  })

}