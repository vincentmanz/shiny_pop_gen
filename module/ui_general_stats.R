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
        checkboxInput("hs_checkbox", "Hs: genetic diversity within population", FALSE),
        checkboxInput("ht_checkbox", "Ht: overall gene diversity", FALSE),
        checkboxInput("htp_checkbox", "Htp: corrected Ht", FALSE),
        checkboxInput("dst_checkbox", "Dst: gene diversity among samples", FALSE),
        checkboxInput("dstp_checkbox", "Dstp: corrected Dst", FALSE),
        checkboxInput("fst_checkbox", "Fst: fixation index", FALSE),
        checkboxInput("fstp_checkbox", "Fstp: corrected Fst", FALSE),
        checkboxInput("fis_checkbox", "Fis: inbreeding coefficient per overall loci", FALSE),
        checkboxInput("dest_checkbox", "Dest: measure of population differentiation", FALSE),
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
