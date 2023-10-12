# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    fluidRow(
      box(
        width = 2,
        title = "Basic diversity and differentiation statistics",             
        status = "primary",
        h4("Select the indices"),
        solidHeader = TRUE,
        
        checkboxInput("ho_checkbox", "Ho: heterozygosity within population (observed heterozygosity)", TRUE),
        checkboxInput("hs_checkbox", "Hs: sub population genetic diversity", TRUE),
        checkboxInput("ht_checkbox", "Ht: overall gene diversity", TRUE),
        checkboxInput("fisw_checkbox", "Fis: Consanguinity of individuals relative to the consanguinity of subpopulations (Weir)", TRUE),
        checkboxInput("fstw_checkbox", "Fst: Population consanguinity relative to total consanguinity (Weir)", TRUE),
        checkboxInput("fisn_checkbox", "Fis: Consanguinity of individuals relative to the consanguinity of subpopulations (Nei)", FALSE),
        checkboxInput("fstn_checkbox", "Fst: Population consanguinity relative to total consanguinity (Nei)", FALSE),
        
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
  )
}
