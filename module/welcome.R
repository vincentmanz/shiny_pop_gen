# welcome.R

# HEADER CONFIGURATION
# Create an enhanced dashboard header with improved styling
header <- dashboardHeader(
  title = div(
    style = "display: flex; align-items: center; font-weight: bold; color: black;",
    icon("dna", style = "margin-right: 8px; font-size: 20px; color: black;"),
    "PGA-Commander"
  ),
  titleWidth = 270,

  # Enhanced text link section with styling
  tags$li(
    a(
      href = 'https://umr-intertryp.cirad.fr/en',
      target = "_blank",
      style = "color: #3182bd; font-size: 15px; font-weight: bold; padding: 15px 15px; display: inline-block; font-family: sans-serif;",
      "INTERTRYP Home"
    ),
    class = "dropdown",

    # Add CSS for hover effect on text link
    tags$style(HTML("
      .dropdown a:hover {
        color: #756bb1;
        transform: scale(1.05);
        display: inline-block;
      }
    "))
  )
)

# UTILITY FUNCTIONS

# line breaks function
linebreaks <- function(n) {HTML(strrep(br(), n))}

# DATA EXAMPLES FOR DOCUMENTATION

# Example 1: Single column genotype format
# This demonstrates how genotypes can be encoded in a single column
# using allele pairs separated by a delimiter (here: "/")
data_one_col <- data.frame(
  Population = c("Boulouparis", "Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c("192/194", "200/200", "0/0", "145/145", "0/0"),
  C07 = c("145/192", "179/179", "92/100", "92/92", "92/92"),
  stringsAsFactors = FALSE
)

# Example 2: Two-column genotype format
# This shows how the same genetic data can be represented using
# separate columns for each allele of a marker
data_two_col <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c(192, 200, 0, 145),
  B12_2 = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92),
  C07_2 = c(192, 179, 92, 100),
  stringsAsFactors = FALSE
)

# Example 3: Complete dataset with GPS coordinates
# This demonstrates the full data structure including optional
# geographical coordinates for population genetic analysis
data_gps <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  Latitude = c(-21.86444444, -22.16805556, -22.10111111, -21.64111111),
  Longitude = c(166.0391667, 166.2694444, 166.3030556, 165.8461111),
  B12 = c(192, 200, 0, 145), "B12" = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92), "C07" = c(192, 179, 92, 100),
  stringsAsFactors = FALSE
)

# SIDEBAR CONFIGURATION

# Enhanced sidebar with improved icons and styling
sidebar <- dashboardSidebar(
  width = 270,
  
  # Add custom CSS for better sidebar appearance
  tags$head(
    tags$style(HTML("
      .sidebar-menu {
        font-size: 15px;
      }
      .sidebar-menu > li.active > a {
        background-color: #bcbddc !important;
        border-left: 3px solid #ffffff !important;
      }
      .sidebar-menu > li > a:hover {
        background-color: #756bb1 !important;
        transition: background-color 0.3s ease;
      }
      .sidebar-menu > li > a {
        padding: 15px 20px !important;
      }
    "))
  ),
  
  sidebarMenu(
    id = "sidebar",
    # Welcome tab - Main landing page
    menuItem("Welcome to PGA-Commander", 
             tabName = "welcome", 
             icon = icon("home", lib = "font-awesome"),
             selected = TRUE),
    
    # Data import tab - File upload and data processing
    menuItem("Data Import & Filtering", 
             tabName = "data", 
             icon = icon("upload", lib = "font-awesome")),
    
    # General statistics tab - Basic population genetics metrics
    menuItem("General Statistics", 
             tabName = "general_stats", 
             icon = icon("chart-bar", lib = "font-awesome")),
    
    # Linkage disequilibrium tab - LD analysis
    menuItem("Linkage Disequilibrium", 
             tabName = "linkage_desequilibrium", 
             icon = icon("link", lib = "font-awesome")),
    
    # Genetic drift tab - Drift analysis
    menuItem("Genetic Drift", 
             tabName = "drift", 
             icon = icon("random", lib = "font-awesome"))
  )
)

body <- dashboardBody(
  # Style global amélioré
  tags$style(HTML("
    /* Style général */
    body {
      font-family: 'Helvetica Neue', Arial, sans-serif;
      background-color: #f9f9f9;
    }
    
    /* Titres */
    h2 {
      color: #2c3e50;
      font-weight: 600;
      border-bottom: 2px solid #756bb1;
      padding-bottom: 10px;
      margin-top: 20px;
    }
    
    h3 {
      color: #2980b9;
      font-weight: 550;
    }
    
    /* Boîtes */
    .box {
      border-radius: 8px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      margin-bottom: 20px;
      border-left: 4px solid #756bb1;
    }
    
    .box-title {
      color: black;
      font-size: 20px;
      font-weight: 600;
    }
    
    /* Tableaux */
    .kable-table {
      margin: 20px 0;
      border: 1px solid #ffffff;
    }
    
    .kable-table th {
      background-color: #3498db !important;
      color: white !important;
      font-weight: 600;
    }
    
    /* Section contact */
    .contact-box {
      background-color: #2c3e50 !important;
      color: white;
    }
    
    .contact-box a {
      color: #1abc9c !important;
    }
  ")),
  
  tabItems(
    tabItem(
      tabName = "welcome",
      # Section de bienvenue
      fluidRow(
        box(
          width = 12,
          style = "background-color: white; color: black; border-radius: 4px;",
          solidHeader = TRUE,
          title = h2("Welcome to PGA-Commander", icon("dna")),
          HTML(
            paste(
              p("This interactive Shiny application is designed for population genetics analysis, suitable for both training purposes and research work", 
                style = "font-size: 16px; line-height: 1.6;"),
              p("The application provides multiple analytical modules to facilitate data processing and enhance understanding of key population genetics concepts.",
                style = "font-size: 16px; line-height: 1.6;"),
              p(icon("external-link-alt"), "For detailed theoretical background and course materials, please visit:",
                a("Thierry de Meeûs' website", 
                  href='https://www.t-de-meeus.fr/EnseignMeeus.html', 
                  target='_blank',
                  style = "font-weight: bold;"),
                style = "font-size: 16px; line-height: 1.6; margin-top: 15px;"),
              sep = ""
            )
          )
        )
      ),
      
      # Instructions d'utilisation
      fluidRow(
        box(
          width = 12,
          title = "Getting Started",
          solidHeader = TRUE,
          collapsible = TRUE,
          p(icon("mouse-pointer"), "To begin your analysis:", style = "font-size: 16px;"),
          tags$ol(
            style = "font-size: 16px; padding-left: 20px;",
            tags$li("Import your data using the 'Data Import' tab"),
            tags$li("Explore basic statistics in the 'General Stats' section"),
            tags$li("Perform specialized analyses using the other modules")
          )
        )
      ),
      
      # Structure des données
      fluidRow(
        box(
          width = 12,
          title = "Data Format Requirements",
          solidHeader = TRUE,
          collapsible = TRUE,
          HTML(
            "<h4 style='color: #2980b9; font-weight: bold;'>Data structure specifications:</h4>",
            "<ul style='font-size: 16px; line-height: 1.6;'>
              <li><strong>Mandatory:</strong> One column containing population identifiers</li>
              <li><strong>Genetic markers:</strong> Can be encoded in either format shown below</li>
              <li><strong>Separator:</strong> Any character can be used (shown here: <span style='color: #e67e22; font-weight: bold;'>/</span>)</li>
            </ul>"
          ),
          
          # Affichage des tableaux exemples
          fluidRow(
            column(
              width = 6,
              div(
                style = "background: #efedf5; padding: 10px; border-radius: 5px; margin-bottom: 20px;",
                h4("Single-column genotype encoding", style = "color: #3498db; margin-top: 0; font-weight: bold;"),
                HTML(
                  data_one_col %>%
                    knitr::kable("html", align = 'l', caption = "<span style='color: black; font-weight: bold;'>One-column genotype format</span>") %>%
                    kable_styling(
                      full_width = FALSE, 
                      position = "center",
                      bootstrap_options = c("striped", "hover", "condensed")
                    ) %>%
                    column_spec(2) %>%
                    column_spec(3)
                )
              )
            ),
            column(
              width = 6,
              div(
                style = "background: #efedf5; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                h4("Two-column genotype encoding", style = "color: #3498db; margin-top: 0; font-weight: bold;"),
                HTML(
                  data_two_col %>%
                    knitr::kable("html", align = 'l', caption = "<span style='color: black; font-weight: bold;'>Two-column genotype format</span>",
                                col.names = c('Population', 'B12', 'B12', 'C07', 'C07')) %>%
                    kable_styling(
                      full_width = FALSE, 
                      position = "center",
                      bootstrap_options = c("striped", "hover", "condensed")
                    ) %>%
                    column_spec(2) %>%
                    column_spec(3) %>%
                    column_spec(4) %>%
                    column_spec(5)
                )
              )
            )
          ),
          
          # Section pour les coordonnées GPS
          HTML(
            "<h4 style='color: #2980b9; margin-top: 20px; font-weight: bold;'>Optional geographical data:</h4>",
            "<p style='font-size: 16px;'>You may include GPS coordinates for spatial analyses:</p>"
          ),
          div(
            style = "background: #efedf5; padding: 15px; border-radius: 5px; width: 80%; margin: 0 auto 20px;",
            HTML(
              data_gps %>%
                knitr::kable("html", align = 'l', caption = "<span style='color: black; font-weight: bold;'>Dataset with GPS coordinates</span>") %>%
                kable_styling(
                  full_width = FALSE, 
                  position = "center",
                  bootstrap_options = c("striped", "hover", "condensed")
                ) %>%
                column_spec(2) %>%
                column_spec(3)
            )
          )
        )
      ),
      
      # Section contact
      fluidRow(
        box(
          width = 12,
          title = "Contact & Support",
          style = "background-color: white; color: black; border-radius: 4px;",
          solidHeader = TRUE,
          HTML(
            "<p style='font-size: 16px; color: black;'>", 
            "If you have any suggestions or encounter bugs, please don't hesitate to contact Vincent Manzanilla:</p>",
            "<div style='margin-left: 30px; margin-top: 10px;'>
              <a href='https://github.com/vincentmanz/shiny_pop_gen' 
                 target='_blank' 
                 style='color: #3182bd; text-decoration: none; font-weight: 500;'>
                <i class='fab fa-github'></i> GitHub Repository
              </a>
            </div>"
          )
        )
      )
    ),
    
    # Autres onglets (conservés inchangés)
    tabItem(tabName = "data", generateImportDataUI()),
    tabItem(tabName = "general_stats", general_stats_UI()),
    tabItem(tabName = "linkage_desequilibrium", linkage_desequilibrium_UI()),
    tabItem(tabName = "drift", generateGeneticDriftUI())
  )
)