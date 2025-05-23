server_map <- function(input, output, session, df_reactive) {
  
  # Define your reactive data frame
  df_reactive <- reactiveVal(default_df)
  
  # Update the select input choices
  observe({
    req(df_reactive())
    df <- df_reactive()
    
    updateSelectInput(session, 'latitude_var', choices = c("select" = "", colnames(df)))
    updateSelectInput(session, 'longitude_var', choices = c("select" = "", colnames(df)))
    updateSelectInput(session, 'population_var', choices = c("select" = "", colnames(df)))
  })
  
  observeEvent(input$run_map, {
    req(input$latitude_var, input$longitude_var, input$population_var)
    
    # Check if Latitude and Longitude are numerical
    if (!is.numeric(df_reactive()[[input$latitude_var]]) ||
        !is.numeric(df_reactive()[[input$longitude_var]])) {
      cat("Latitude and Longitude should be numerical.\n")
      return()
    }
    
    # Create the populationsLL data frame
    populationsLL <- df_reactive() %>%
      select(input$population_var, input$latitude_var, input$longitude_var)
 
       # Group by Locality, Latitude, and Longitude, and calculate Population Size
    populationsLL_grouped <- populationsLL %>%
      group_by_all()%>%count()
    colnames(populationsLL_grouped) <- c('Population', 'Longitude', 'Latitude', 'Population size')

      # Render the populationsLL_uniq data frame as a table
    output$populationsLL_uniq_table <- renderTable({
      req(input$run_map)  # Show the table after clicking "Run Map" button
      populationsLL_grouped
    })
    # Render the map
    output$map <- renderLeaflet({
      leaflet(populationsLL_grouped) %>%
        addTiles() %>%
        addCircles(lng = populationsLL_grouped$Latitude, lat = populationsLL_grouped$Longitude, 
                   popup=paste("Location:", populationsLL_grouped$Population, "<br>","Population size:", populationsLL_grouped$`Population size`), 
                   radius = populationsLL_grouped$`Population size` * 50,
                   stroke = FALSE, fillOpacity = 0.5)
    })
  })
}