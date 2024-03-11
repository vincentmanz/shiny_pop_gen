library(shiny)
library(bs4Dash)
library(waiter)

ui <- dashboardPage(
    
    title = "Waiter Issue",
    
    fullscreen = TRUE,
    
    preloader = list(html = spin_1(), color = "#333e48"),
    
    header = bs4DashNavbar(title = "Home",
                           compact = TRUE),
    
    sidebar = bs4DashSidebar(collapsed = FALSE,
                             bs4SidebarMenu(bs4SidebarMenuItem(text = "Waiter Issue",
                                                               tabName = "issue", 
                                                               icon = icon("exclamation")))),
    
    controlbar = dashboardControlbar(),
    
    footer = dashboardFooter(left = "Waiter issue",
                             right = "2021"),
    
    body = bs4DashBody(bs4TabItems(bs4TabItem(tabName = "issue",
                                              actionButton("show",
                                                           label = "First click doesn't work, while the second yes")))))


server <- function(input, output) {

    observeEvent(input$show, {
        
        waiter_show(html = spin_fading_circles())
        
        Sys.sleep(3)
        
        waiter_hide()
    })
}

runApp(shinyApp(ui, server))
