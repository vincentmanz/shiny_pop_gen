mapUI <- function() {
  pageWithSidebar(
    headerPanel("Map"),
    sidebarPanel(
      title = "Select Coordinates",
      selectInput('latitude_var', 'Latitude', choices = NULL),
      selectInput('longitude_var', 'Longitude', choices = NULL),
      selectInput('population_var', 'Population', choices = NULL),
      actionButton("run_map", "Run Map")
    ),
    mainPanel(
      h3("Populations, coordinates, and population sizes"),
      tableOutput("populationsLL_uniq_table"),
      leafletOutput("map") 
    )
  )
}
