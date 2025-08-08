# module/ui_welcome.R
welcome_ui <- function(id) {
  ns <- NS(id)
  
  # helper
  linebreaks <- function(n) HTML(strrep(br(), n))
  
  # data frames (same as you had) ...
  
  # Build tables as HTML strings
  table_one_col <- data_one_col |>
    kableExtra::kable("html", align = "l", caption = "Genotype coded with one column.") |>
    kableExtra::kable_styling(full_width = FALSE, position = "center",
                              bootstrap_options = c("striped","hover","condensed")) |>
    kableExtra::column_spec(column = 2, background = "#eef6fb") |>
    kableExtra::column_spec(column = 3, background = "#eefaf6") |>
    as.character()
  
  table_two_col <- data_two_col |>
    kableExtra::kable("html", align = "l",
                      caption = "Genotype coded with two columns.",
                      col.names = c("Population","B12","B12","C07","C07")) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center",
                              bootstrap_options = c("striped","hover","condensed")) |>
    kableExtra::column_spec(column = 2, background = "#eef6fb") |>
    kableExtra::column_spec(column = 3, background = "#f3f9fd") |>
    kableExtra::column_spec(column = 4, background = "#eefaf6") |>
    kableExtra::column_spec(column = 5, background = "#f1fcf9") |>
    as.character()
  
  table_gps <- data_gps |>
    kableExtra::kable("html", align = "l", caption = "Data frame with GPS coordinates") |>
    kableExtra::kable_styling(full_width = FALSE, position = "center",
                              bootstrap_options = c("striped","hover","condensed")) |>
    kableExtra::column_spec(column = 2, background = "#eef6fb") |>
    kableExtra::column_spec(column = 3, background = "#f3f9fd") |>
    as.character()
  
  fluidPage(
    tags$head(
      tags$style(HTML("
        .welcome-container { max-width: 1100px; margin: 0 auto; }
        .welcome-lead { font-size: 16px; line-height: 1.6; }
        .welcome-list p { margin: 0 0 6px; }
        .box .box-body p { margin-bottom: 8px; }
      "))
    ),
    
    div(class = "welcome-container",
        fluidRow(
          shinydashboard::box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = tagList(icon("home"), " Welcome to GenoPop"),
            div(class = "welcome-lead",
                HTML("This interactive Shiny app serves as a resource for both novice participants in the Empirical Population Genetics training course and researchers seeking to analyze ALFP or RAPD data. The app features multiple tabs designed to facilitate data processing and enhance understanding of the calculations."),
                br(),
                HTML("For more details, visit Thierry de Meeus' website: <a href='https://www.t-de-meeus.fr/EnseignMeeus.html' target='_blank'>https://www.t-de-meeus.fr/EnseignMeeus.html</a>."),
                linebreaks(2),
                HTML("-> To get started, import your data in the <b>Data import and Filtering</b> tab. Once loaded, you can run any analysis from the other tabs.")
            )
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Expected data structure",
            div(class = "welcome-list",
                p("\u2022 One column for the Population identifier."),
                p("\u2022 Genetic markers can be encoded either as a single column per marker (e.g., '192/194'),"),
                p("  or two columns per marker (one per allele)."),
                p('\u2022 The allele separator can be any character; here we use "/".')
            ),
            linebreaks(1),
            fluidRow(
              column(6, HTML(table_one_col)),   # << wrap in HTML(...)
              column(6, HTML(table_two_col))    # << wrap in HTML(...)
            ),
            linebreaks(1),
            p("\u2022 Optionally include Latitude and Longitude columns:"),
            div(style = "max-width: 800px; margin: 0 auto;", HTML(table_gps))  # << wrap in HTML(...)
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            width = 12, status = "warning", solidHeader = TRUE,
            title = "Contact",
            HTML(
              "If you have suggestions or encounter bugs, please open an issue on the GitHub repo:<br/>",
              "<a href='https://github.com/vincentmanz/shiny_pop_gen' target='_blank'>github.com/vincentmanz/shiny_pop_gen</a>"
            )
          )
        )
    )
  )
}
