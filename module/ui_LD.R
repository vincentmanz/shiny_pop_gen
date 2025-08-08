# module/ui_LD.R

# Define the UI for the Linkage Disequilibrium tab (module)
linkage_desequilibrium_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    waiter::useWaiter(),
    fluidRow(
      shinydashboard::box(
        width = 4,
        title = "Compute linkage disequilibrium (LD)",
        status = "primary",
        solidHeader = TRUE,
        
        checkboxInput(ns("include_missing"), "Include Missing Data", value = TRUE),
        numericInput(
          ns("n_iterations"),
          "Number of Iterations",
          value = 10000,
          min = 1000,
          step = 1000
        ),
        tags$hr(),
        actionButton(ns("run_LD"), "Run", icon = icon("rocket"))
      ),
      
      shinydashboard::box(
        width = 8,
        title = "Summary output",
        status = "primary",
        solidHeader = TRUE,
        
        downloadButton(ns("download_gstats_csv"), "Download CSV"),
        div(style = "max-height: 600px; overflow-y: auto;",
            tableOutput(ns("summary_output"))
        )
      )
    )
  )
}
