# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    fluidRow(
      column(4,
             # Sidebar panel for uploading files
             sidebarPanel(
               width = 12,
               h5("Basic diversity and differentiation statistics"),
               checkboxInput("ho_checkbox", "Ho", TRUE),
               checkboxInput("hs_checkbox", "Hs", FALSE),
               checkboxInput("ht_checkbox", "Ht", FALSE),
               checkboxInput("dst_checkbox", "Dst", FALSE),
               checkboxInput("htp_checkbox", "Htp", FALSE),
               checkboxInput("dstp_checkbox", "Dstp", FALSE),
               checkboxInput("fst_checkbox", "Fst", FALSE),
               checkboxInput("fstp_checkbox", "Fstp", FALSE),
               checkboxInput("fis_checkbox", "Fis", FALSE),
               checkboxInput("dest_checkbox", "Dest", FALSE),
               tags$hr(),
               actionButton("run_basic_stats", "Run")
             )
      ),
      column(8,
             # Display the table here
             tableOutput("basic_stats_result")
      )
    ),
    # Footer notes
    fluidRow(
      column(12,
             h4("Indices Definitions:"),
             p("Ho: heterozygosity within population (observed heterozygosity), Hs: genetic diversity within population, 
          Ht: overall gene diversity, Dst: gene diversity among samples, Dstp: corrected Dst, Fst: fixation index, Fstp: corrected Fst, 
          Fis: inbreeding coefficient per overall loci, Dest: measure of population differentiation")
      )
    )
  )
}
