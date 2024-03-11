library(shiny)
library(waiter)


dir.create("assets")
download.file("https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/bookdown.svg", destfile = "./assets/bookdown.svg")
shiny::addResourcePath("assets", "assets")

ui <- fluidPage(
    useWaiter(),
    useHostess(),
    actionButton("draw", "redraw"),
    plotOutput("plot")
)

server <- function(input, output) {
    observeEvent(input$draw, {
        hot <- Hostess$new(min = 0, max = 100)
        hot$set_loader(hostess_loader(svg = "assets/bookdown.svg", progress_type = "fill"))
        waiter <- Waiter$new("plot")
        waiter$show()

        for (i in 1:10) {
            Sys.sleep(.2)
            library(hostess)

            hot$inc(1)
        }
print("A")
        set.seed(123) # Set seed for reproducibility
        n_points <- 2000
        random_data <- data.frame(
            x = rnorm(n_points),
            y = rnorm(n_points)
        )

        # Create a scatter plot
        library(ggplot2)
        random_plot <- ggplot(random_data, aes(x = random_data$x, y = random_data$y)) +
            geom_point() + # Scatter plot
            labs(title = "Random Plot", x = "X-axis", y = "Y-axis") + # Labels
            theme_minimal() # Minimal theme
        output$plot <- renderPlot({
            random_plot
        })
        waiter$hide()
    })
}



runApp(shinyApp(ui = ui, server = server))
