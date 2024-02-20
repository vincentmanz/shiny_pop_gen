# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    fluidRow(
      box(
        width = 2,
        title = "Basic diversity and differentiation statistics",             
        status = "primary",
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
        checkboxInput("GST_checkbox", "GST", FALSE),
        checkboxInput("GST_sec_checkbox", "GST''", FALSE),
        tags$hr(),
        actionButton("run_basic_stats", "Run")
      ),
      box(
        width = 10,
        title = "General statistics",
        status = "primary",
        solidHeader = TRUE,
        ## DOWNLOAD
        downloadButton("download_gstats_csv", ""),
        # Display the table here
        tableOutput("basic_stats_result")
      )
    ),
    fluidRow(
      box(
        width = 2,
        title = "Plot",
        status = "primary",
        solidHeader = TRUE,
        actionButton("run_plots_heatmap", "Plot")
      ),
      box(
        width = 15,
        title = "Heatmp missing data",
        status = "primary",
        solidHeader = TRUE,
        ## DOWNLOAD
        downloadButton("download_missing_data", ""),
        uiOutput("Heatmap_out")
      )
    )
  )
}