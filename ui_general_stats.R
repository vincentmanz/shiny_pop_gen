generateGeneralStatsUI <- function() {
  pageWithSidebar(
    headerPanel("General Stats"),
    sidebarPanel(
      selectInput('var1', 'Population', choices = NULL),
      selectInput('var2', 'Individuals', choices = NULL),
      actionButton("run", "Run")
    ),
    mainPanel(
      tableOutput("count_table")
      )
  )
}
