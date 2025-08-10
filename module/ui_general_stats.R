# ui_general_stats.R
general_stats_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    waiter::useWaiter(),
    
    fluidRow(
      shinydashboard::box(
        width = 2,
        title = "Basic diversity and differentiation statistics",
        status = "primary",
        solidHeader = TRUE,
        
        h4("Select indices"),
        checkboxInput(ns("ho_checkbox"),  "Ho", TRUE),
        checkboxInput(ns("hs_checkbox"),  "Hs", TRUE),
        checkboxInput(ns("ht_checkbox"),  "Ht", TRUE),
        checkboxInput(ns("fit_wc_checkbox"), "Fit (W&C)", TRUE),
        checkboxInput(ns("fis_wc_checkbox"), "Fis (W&C)", TRUE),
        checkboxInput(ns("fst_wc_checkbox"), "Fst (W&C)", TRUE),
        checkboxInput(ns("fis_n_checkbox"),  "Fis (Nei)", TRUE),
        checkboxInput(ns("fst_n_checkbox"),  "Fst (Nei)", TRUE),
        checkboxInput(ns("GST_checkbox"),    "GST", FALSE),
        checkboxInput(ns("GST_sec_checkbox"), "GST''", FALSE),
        tags$hr(),
        
        # ---- Grouping + optional Population level filter ----
        selectInput(
          ns("level1"),
          label   = "Population level:",
          choices = c("All" = ""),        
          selected = ""
        ),
        tags$hr(),
        
        actionButton(ns("run_basic_stats"), "Run", icon = icon("rocket"))
      ),
      
      shinydashboard::box(
        width = 10,
        title = "General statistics",
        status = "primary",
        solidHeader = TRUE,
        downloadButton(ns("download_gstats_csv"), "Download CSV"),
        tableOutput(ns("basic_stats_result"))
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        title = "Heatmap",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        actionButton(ns("run_plot_heatmap"), "Heatmap")
      ),
      shinydashboard::box(
        title = "GST",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        actionButton(ns("run_plot_GST"), "GST")
      ),
      shinydashboard::box(
        title = "FIS",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        actionButton(ns("run_plot_FIS"), "FIS")
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Plot result",
        status = "primary",
        solidHeader = TRUE,
        downloadButton(ns("download_plot_png"), "Download"),
        plotOutput(ns("plot_output"))
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 4,
        title = "Panmixia",
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("numboot"), "Number of bootstrap replicates", value = 1000, max = 10000),
        actionButton(ns("run_panmixia"), "Run", icon = icon("rocket"))
      ),
      shinydashboard::box(
        width = 8,
        title = "Panmixia tabulation",
        status = "primary",
        solidHeader = TRUE,
        downloadButton(ns("download_panmixia_csv"), "Download CSV"),
        tableOutput(ns("panmixia_boot_result"))
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Plot Panmixia",
        status = "primary",
        solidHeader = TRUE,
        downloadButton(ns("download_panmixia_boot_plot"), "Download png"),
        plotOutput(ns("panmixia_boot_plot"))
      )
    )
  )
}
