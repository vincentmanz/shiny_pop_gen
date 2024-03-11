library(shiny)
library(waiter)

ui <- fluidPage(
  useWaiter(),
  triggerWaiter(
    actionButton(
      "generate",
      "Generate Plot"
    ),
    id = "plot" # hide when plot renders
  ),
  plotOutput("plot")
)

server <- function(input, output){

  output$plot <- renderPlot({
    input$generate
    Sys.sleep(5)
    plot(runif(50))
  })

}

shinyApp(ui, server)
runApp(shinyApp(ui = ui, server = server))
