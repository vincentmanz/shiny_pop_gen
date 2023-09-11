library(shiny)
library(shinydashboard)
library(leaflet)



leafletCRS(
  crsClass = "L.CRS.EPSG3857",
  code = NULL,
  proj4def = NULL,
  projectedBounds = NULL,
  origin = NULL,
  transformation = NULL,
  scales = NULL,
  resolutions = NULL,
  bounds = NULL,
  tileSize = NULL
)

header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody(
  infoBoxOutput("ibox"),
)

ui <- dashboardPage(header, sidebar, body)
server <- function(input, output, session){
  
  val <- reactiveVal(0)
  
  output$ibox <- renderInfoBox({
    infoBox(
      "Number",
      val(),
      icon = icon("credit-card")
    )
  })
  observe({
    invalidateLater(100, session)
    isolate({
      # It will count till 5000 because of this condition
      if(val() < 5000) {
        newVal <- val()+1
        val(newVal)
      }
    })
  })
}
shinyApp(ui, server)
