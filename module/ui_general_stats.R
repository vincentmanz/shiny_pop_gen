# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    fluidRow(
      column(4,
             # Sidebar panel for uploading files
             sidebarPanel(
               width = 12,
               h6("Basic diversity and differentiation statistics"),             
               checkboxInput("ho_checkbox", "Ho: heterozygosity within population (observed heterozygosity)", TRUE),
               checkboxInput("hs_checkbox", "Hs: genetic diversity within population", FALSE),
               checkboxInput("ht_checkbox", "Ht: overall gene diversity", FALSE),
               checkboxInput("gst_checkbox", "Htp: corrected Ht", FALSE),
               checkboxInput("gst2_checkbox", "Dstp: corrected Dst", FALSE),
               checkboxInput("fst_checkbox", "Fst: fixation index (Nei)", FALSE),
               checkboxInput("fis_checkbox", "Fis: inbreeding coefficient per overall loci (Nei)", FALSE),
               tags$hr(),
               actionButton("run_basic_stats", "Run")
             )
      ),
      column(8,
             # Display the table here
             tableOutput("basic_stats_result")
      )
    ),
  )
}


# weir kokram fst and fis
# find the formula.... 
