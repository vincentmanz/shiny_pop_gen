# ui_LD.R

# Define the UI for the Linkage Disequilibrium tab
linkage_desequilibrium_UI <- function() {
  fluidPage(
    useWaiter(), # Ensure this is correctly placed
    fluidRow(
      box(
        width = 4,
        title = "Compute the linkage disequilibrium statistics",
        status = "primary",
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
        title = "Summary output",
        status = "primary",
        solidHeader = TRUE,
        # DOWNLOAD
        downloadButton("download_gstats_csv", "Download CSV"),
        # Display the table here
        tableOutput("summary_output")
      )
    )
  )
}
