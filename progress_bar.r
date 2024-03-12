library(shiny)
library(waiter)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useWaiter(),
    useHostess(),
    actionButton("draw", "redraw"),
    plotOutput("plot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # configure the Waiter

    # hot$set_loader(waiter::hostess_loader(svg = "assets/bookdown.svg", progress_type = "fill"))
    # waiter <- Waiter$new("plot", html = host$get_loader(preset = "bubble"))


    observeEvent(input$draw, {
        # Start the waiter and show it
        # waiter$show()
        # host$start() # Show the hostess progress bar
        # print(host)
        url <- "https://www.freecodecamp.org/news/content/images/size/w2000/2020/04/w-qjCHPZbeXCQ-unsplash.jpg"


        loader <- hostess_loader(progress_type = "stroke", stroke_color = hostess_stripe())
        host <- Hostess$new("plot", infinite = TRUE)$set_loader(loader)

        waiter <- Waiter$new("plot", html = host$get_loader())

        #host$start()

        waiter$show()
        # Simulate a long computation
        for (i in 1:10) {
            Sys.sleep(1)
            # host$inc(i * 10)
        }

        # Hide the waiter
        # host$close()
        # waiter_hide()
        # Create random data
        set.seed(123) # Set seed for reproducibility
        n_points <- 2000
        random_data <- data.frame(
            x = rnorm(n_points),
            y = rnorm(n_points)
        )

        # Create a scatter plot
        library(ggplot2)
        random_plot <- ggplot(random_data, aes(x = x, y = y)) +
            geom_point() + # Scatter plot
            labs(title = "Random Plot", x = "X-axis", y = "Y-axis") + # Labels
            theme_minimal() # Minimal theme
        output$plot <- renderPlot({
            random_plot
        })
    })
}



runApp(shinyApp(ui = ui, server = server))
