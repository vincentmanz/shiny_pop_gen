server_general_stats <- function(input, output, session) {
  
  # Define the reactive expression to hold the data frame
  df_reactive <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })
  
  # Update the select input choices
  observe({
    req(df_reactive())
    df <- df_reactive()
    
    updateSelectInput(session, 'var1', choices = c("select" = "", colnames(df)))
    updateSelectInput(session, 'var2', choices = c("select" = "", colnames(df)))
  })
  
  # Observer for the 'Run' button 
  observeEvent(input$run, {
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
}