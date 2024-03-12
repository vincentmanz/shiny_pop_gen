library(shiny)
library(waiter)

ui <- fluidPage(
    h1("waiter"),
    use_waiter(),
    use_hostess(),
    actionButton("draw", "redraw"),
    fluidRow(
        column(4, h3("Loading bar (hostess)"), plotOutput("plot1")),
        column(4, h3("Spinner"), plotOutput("plot2")),
        column(4, h3("Hide on drawn"), plotOutput("plot3"))
    )
)

server <- function(input, output) {
    dataset <- reactive({
        input$draw
        rnorm(100)
    })

    observeEvent(input$draw, {
        # show with loading bar (hostess)
        waiter::waiter_show(
            hostess_loader("myHostess", preset = "bubble"),
            id = "plot1", # specify id of plot to overlay
            color = "white"
        )

        # # show with spinner
        # waiter_show(
        #     spin_3(),
        #     id = "plot2"
        # )

        # # show with spinner AND hide_on_drawn
        # waiter_show(
        #     spin_3(),
        #     id = "plot3",
        #     color = "grey", 
        #     hide_on_render = TRUE
        # )

        # increment progress bar
        hostess <- Hostess$new("myHostess", infinite = TRUE, min = 0, max = 100)
                    hostess$print()

        hostess$start()
        for (i in 1:30) {
            Sys.sleep(runif(1))
        }

        # hide waiters
        hostess$close()
        #waiter_hide_on_render(id = "plot1")
        # waiter_hide_on_render(id = "plot2")
    })

    output$plot1 <- renderPlot(plot(hist(dataset())))
    output$plot2 <- renderPlot(plot(dataset(), type = "l"))
    output$plot3 <- renderPlot(plot(dataset()))
}

# Run the application
shinyApp(ui = ui, server = server)

runApp(shinyApp(ui = ui, server = server))
