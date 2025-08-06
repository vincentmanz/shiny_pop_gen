# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    useWaiter(),
    fluidRow(
      box(
        width = 3,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "Basic diversity and differentiation statistics"),
        selectInput("Level", "Level", choices = NULL),
        h4("Select indices"),
        solidHeader = TRUE,
        checkboxInput("ho_checkbox", "Ho", TRUE),
        checkboxInput("hs_checkbox", "Hs", TRUE),
        checkboxInput("ht_checkbox", "Ht", TRUE),
        checkboxInput("fit_wc_checkbox", "Fit (W&C)", TRUE),
        checkboxInput("fis_wc_checkbox", "Fis (W&C)", TRUE),
        checkboxInput("fst_wc_checkbox", "Fst (W&C)", TRUE),
        checkboxInput("fis_n_checkbox", "Fis (Nei)", TRUE),
        checkboxInput("fst_n_checkbox", "Fst (Nei)", TRUE),
        checkboxInput("fst_max_checkbox", "Fst-max", FALSE),
        checkboxInput("fst_prim_checkbox", "Fst'", FALSE),
        checkboxInput("GST_checkbox", "GST", FALSE),
        checkboxInput("GST_sec_checkbox", "GST''", FALSE),
        tags$hr(),
        actionButton("run_basic_stats", "Run", icon = icon("rocket"))
      ),
      # Ajouter des niveaux
      box(
        width = 9,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "General statistics"),
        solidHeader = TRUE,
        uiOutput("basic_stats_ui"),
        style = "overflow-y: auto;"
      )
    ),
    fluidRow(
      box(
        title = "Heatmap",
        width = 4,
        solidHeader = TRUE,
        actionButton("run_plot_heatmap", "Heatmap")
      ),
      box(
        title = "GST",
        width = 4,
        solidHeader = TRUE,
        actionButton("run_plot_GST", "GST")
      ),
      box(
        title = "FIS",
        width = 4,
        solidHeader = TRUE,
        actionButton("run_plot_FIS", "FIS")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Plot result",
        solidHeader = TRUE,
        plotOutput("plot_output"),
        br(),
        downloadButton("download_plot_png", "Download")
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Panmixia",
        solidHeader = TRUE,
        numericInput("numboot", "Number of bootstrap replicates", value = 1000, max = 10000),
        textInput("level1", "Population unit", value = "Population"),
        actionButton("run_panmixia", "Run", icon = icon("rocket"))
      ),
      box(
        width = 8,
        title = "Panmixia tabulation",
        solidHeader = TRUE,
        tableOutput("panmixia_boot_result"),
        downloadButton("download_panmixia_csv", "Download CSV", class = "btn btn-primary")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Plot Panmixia",
        solidHeader = TRUE,
        plotOutput("panmixia_boot_plot"),
        br(),
        downloadButton("download_panmixia_boot_plot", "Download png")
      )
    )
  )
}
