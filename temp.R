library(shiny)

ui <- fluidPage(
  tags$div(
    class = "this", 
    actionButton("go", "go")
  )
)

server <- function(input, output, session) {
  
  observeEvent( input$go , {
    
    x <- reactiveValues(x = list(iris, mtcars, airquality))
    
    lapply(1:3, function(i){
      insertUI(
        ".this", 
        ui =  tagList(
          p(paste("Number", i)),
          renderPlot({
            plot(x$x[[i]])
          })
        ))
    })
  })
  
}

shinyApp(ui, server)