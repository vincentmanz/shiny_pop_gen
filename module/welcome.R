# welcome.R

# Logo
header <- dashboardHeader(title = "GenoPop",
                          
                          tags$li(a(href = 'https://umr-intertryp.cirad.fr/en',
                                    img(src = 'INTERTRYP_logo.png',
                                        title = "InterTryp Home", height = "50px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"))

# line breaks function
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Create a data frame for the one example

data_one_col <- data.frame(
  Population = c("Boulouparis", "Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c("192/194", "200/200", "0/0", "145/145", "0/0"),
  C07 = c("145/192", "179/179", "92/100", "92/92", "92/92")
)

# Create a data frame for the two-column example
data_two_col <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c(192, 200, 0, 145),
  B12_2 = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92),
  C07_2 = c(192, 179, 92, 100)
)

# Create a data frame for the latitude/longitude example
data_gps <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  Latitude = c(-21.86444444, -22.16805556, -22.10111111, -21.64111111),
  Longitude = c(166.0391667, 166.2694444, 166.3030556, 165.8461111),
  B12 = c(192, 200, 0, 145),
  "B12" = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92),
  "C07" = c(192, 179, 92, 100)
)

## dashboard layout
sidebar <- dashboardSidebar(
  sidebarMenu(id="sidebar",  
              menuItem("Welcome to GenoPop", tabName = "welcome", icon = icon("home"),  selected=TRUE),
              menuItem("Data import and Filtering", tabName = "data", icon = icon("dashboard")),
              menuItem("General Statsistics", tabName = "general_stats", icon = icon("dashboard")),
              menuItem("Genetic Drift", tabName = "drift", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "welcome",
      h2("Welcome to GenoPop"),
      icon = icon("home"),
      HTML(
        paste(
          "This interactive Shiny app serves as a resource for both novice participants in the Empirical Population Genetics training course and researchers seeking to analyze ALFP or RAPD data. The app features multiple tabs designed to facilitate data processing and enhance understanding of the calculations.",
          "For more comprehensive insights and course details, please visit Thierry de Meuus' website:",
          "<a href='https://www.t-de-meeus.fr/EnseignMeeus.html' target='_blank'>https://www.t-de-meeus.fr/EnseignMeeus.html</a>",
          sep = "<br/>"
        ),
        paste("To utilize this application, begin by importing your data in the 
              Data Import and Filtering tab. Once your data is loaded, you can proceed to execute any of the analyses available within the application.",
              sep = "<br/>",
              linebreaks(5)
        )
      ),
      HTML(
        "<h3>The data structure should be as follows:</h3>",
        "<p style='margin-left: 20px;'>- One column for the Population identifier.</p>",
        "<p style='margin-left: 20px;'>- The genetic marker can be encoded using the following patterns:</p>",
        "<p style='margin-left: 20px;'>- The separator could be any symbol, here it is separated by  \"<span style='color: orange;'>/</span>\". </p>",
        "<div style='display: flex;'>",
        "<div style='flex: 1; display: flex; flex-direction: column;'>",  
        data_one_col %>%
          kable("html", align = 'l', caption = 'Genotype coded with one column.') %>%
          kable_styling(full_width = F, position = "c") %>%
          column_spec(column = 2, background = "#347893") %>%
          column_spec(column = 3, background = "#34937E"),
        "</div>",
        
        "<div style='flex: 1; display: flex; flex-direction: column;'>",  
        data_two_col %>%
          kable("html", align = 'l', caption = 'Genotype coded with two columns.',
                col.names = c('Population', 'B12', 'B12', 'C07', 'C07')) %>%
          kable_styling(full_width = F, position = "c") %>%
          column_spec(column = 2, background = "#347893") %>%
          column_spec(column = 3, background = "#699CB3") %>%
          column_spec(column = 4, background = "#34937E") %>%
          column_spec(column = 5, background = "#5AB7AB"),
        "</div>",
        
        "</div>",
        "<br>",
        "<p style='margin-left: 20px;'>- Optional two columns for the GPS coordinates.</p>",
        "<div style='margin-left: 40px;'>",
        data_gps %>%
          kable("html", align = 'l', caption = "Data frame with GPS coordinates") %>%
          kable_styling(full_width = F, position = "c") %>%
          column_spec(column = 2, background = "#347893") %>%
          column_spec(column = 3, background = "#699CB3"),
        "</div>",
        linebreaks(5)
      ),
      HTML(
        "<h3>Contact</h4>",
        "If you have any suggestions or encounter bugs, please don't hesitate to contact Vincent Manzanilla through the GitHub page:<br/><a href='https://github.com/vincentmanz/shiny_pop_gen' target='_blank'>https://github.com/vincentmanz/shiny_pop_gen</a>"
      )
    ),
    tabItem(
      tabName = "data",
      generateImportDataUI(),
      icon = icon("cog", lib = "glyphicon")
    ),
    tabItem(
      tabName = "general_stats",
      general_stats_UI(),
      icon = icon("cog", lib = "glyphicon")
    ),
    tabItem(
      tabName = "drift",
      generateGeneticDriftUI(),
      icon = icon("cog", lib = "glyphicon")
    )
  )
)

