# ui_genetic_drift.R

## initial values, adjustable via sliders
initial_p <- 0.5
initial_N <- 10
initial_n_pops <- 5

generateGeneticDriftUI <- function() {
  fluidPage(
    div(
      style = "background-color: white; color: black; padding: 20px; text-align: center; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; border-left: 4px solid #756bb1;",
      titlePanel("Allele frequency changes with genetic drift")
    ),
    sidebarLayout(
      sidebarPanel(
        numericInput("p", "Initial frequency of A", min = 0.0, max = 1.0, value = initial_p, step = 0.01),
        helpText("Frequency must be between 0 and 1"),
        numericInput("N",
                     "Population size",
                     min = 1,
                     max = 20,
                     value = initial_N),
        helpText("N must be between 1 and 100. The plot will take several seconds to load when N is high."),
        sliderInput("n_pops",
                    "Number of populations",
                    min = 1,
                    max = 100,
                    value = initial_n_pops),
        actionButton("go", "Simulate Genetic Drift!", icon("refresh")),
        helpText("When you click the button above, the figure to the right will update with new simulations using the parameters you entered above."),
      ),
      
      ## Show a plot of the simulated change in allele frequencies
      mainPanel(
        p("The inputs to the left allow you to select a different starting initial allele frequency, 
        diploid population size (so the number of gametes is 2N, and number of populations. 
        Each line represents the history of allele frequency change in one population. 
        All populations begin with an identical allele frequency. 
        Each time you change one of the inputs, you'll get a new set of simulation results after you hit the \"Simulate Genetic Drift!\" button. 
        If you hit \"Play\" without changing the input, you'll get a duplicate of the plot you just saw."),
        plotlyOutput("allele_frequency_plot"),
        hr(),
        p("This Shiny app was adapted from Kent Holsinger's source code:")
      )
    )
  )
}

