# ui_LD.R

# Define the UI for the Linkage Disequilibrium tab
linkage_desequilibrium_UI <- function() {
  fluidPage(
    useWaiter(),
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "Compute the linkage disequilibrium statistics"),
        checkboxInput("include_missing", "Include Missing Data", value = TRUE),
        numericInput(
          "n_iterations",
          "Number of Iterations",
          value = 10000,
          min = 1000,
          step = 1000
        ),
        tags$hr(),
        actionButton("run_LD", "Run", icon = icon("rocket"))
      ),
      box(
        width = 8,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "Summary output"),
        solidHeader = TRUE,
        tagList(
          div(style = "overflow-y: scroll; overflow-x: scroll;", tableOutput("summary_output")),
          br(),
          downloadButton("download_LD_csv", "Download CSV", class = "btn btn-primary")
        )
      )
    )
  )
}
