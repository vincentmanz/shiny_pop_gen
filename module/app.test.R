ui <- dashboardPage(
  dashboardHeader(title = "Dynamic sidebar"),
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("menuitem")
    )
  ),
  dashboardBody()
)

server <- function(input, output) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
}

shinyApp(ui, server)