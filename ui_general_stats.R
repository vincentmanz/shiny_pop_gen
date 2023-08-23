generateGeneralStatsUI <- function() {
  pageWithSidebar(
    headerPanel("General Stats"),
    sidebarPanel(
      selectInput('var1', 'Population', choices = NULL),
      selectInput('var2', 'Individuals', choices = NULL),
      actionButton("run_var", "Run"),
      hr(),
      textInput('col_ranges', 'Select Genotypes (e.g., 1-4, 5:10)'),
      actionButton("run_col_ranges", "Run")
    ),
    mainPanel(
      tableOutput("count_table"),
      verbatimTextOutput("missing_info")
      )
  )
}
