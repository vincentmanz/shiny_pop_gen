library(shiny)

ui <- fluidPage(
    ## Add a listener for the window height and plot container width
    tags$head(tags$script('
                        var winDims = [0, 0];
                        var plotElt = document;
                        $(document).on("shiny:connected", function(e) {
                            plotElt = document.getElementById("plotContainer");
                            winDims[0] = plotElt.clientWidth;
                            winDims[1] = window.innerHeight;
                            Shiny.onInputChange("winDims", winDims);
                        });
                        $(window).resize(function(e) {
                            winDims[0] = plotElt.clientWidth;
                            winDims[1] = window.innerHeight;
                            Shiny.onInputChange("winDims", winDims);
                        });
                    ')),
    titlePanel("Old Faithful Geyser Data"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("height", label="Height",
                        min=100, max=900, value = 600)
        ),
        mainPanel(
            tags$div(id="plotContainer", ## Add outer container to make JS constant
                     ## Use an "inline" plot, so that width and height can be set server-side
                     plotOutput("distPlot", inline = TRUE))
        )
    )
)

server <- function(input, output) {
    ## reduce the amount of redraws on window resize
    winDims_d <- reactive(input$winDims) %>% debounce(500)
    ## fetch the changed window dimensions
    getWinX <- function(){
        print(input$winDims);
        if(is.null(winDims_d())) { 400 } else {
            return(winDims_d()[1])
        }
    }
    getWinY <- function(){
        if(is.null(winDims_d())) { 600 } else {
            return(winDims_d()[2] - 50)
        }
    }
    output$distPlot <- renderPlot({
        if(is.null(winDims_d())){
            ## Don't plot anything if we don't yet know the size
            return(NULL);
        }
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    }, width = getWinX, height=getWinY)
}

TEST_APP <- shinyApp(ui = ui, server = server)

runApp(TEST_APP)