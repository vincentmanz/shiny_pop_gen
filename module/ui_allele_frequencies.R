# ui_allele_frequencies.R

ui_allele_frequencies <- function() {
  fluidPage(
    titlePanel("Allele Frequencies and Missing Data"),
    fluidRow(
      column(12,
        wellPanel(
          h4("General Information"),
          fluidRow(
            column(3, 
              valueBoxOutput("total_individuals", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_populations", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_markers", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_missing", width = NULL)
            )
          )
        )
      )
    ),

    fluidRow(
      column(12,
        box(
          title = "Data Summary: Sample Sizes, Missing & Genotyped Data",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          tabsetPanel(
            tabPanel("By Population and Locus",
              br(),
              DT::dataTableOutput("data_summary_by_pop_locus"),
              br(),
              downloadButton("download_summary_pop_locus", ".csv", class = "btn-primary btn-sm"),
              downloadButton("download_summary_pop_locus_txt", ".txt", class = "btn-outline-primary btn-sm")
            ),
            tabPanel("By Locus (Mean across Populations)",
              br(),
              DT::dataTableOutput("data_summary_by_locus_mean"),
              br(),
              downloadButton("download_summary_locus_mean", ".csv", class = "btn-primary btn-sm"),
              downloadButton("download_summary_locus_mean_txt", ".txt", class = "btn-outline-primary btn-sm")
            ),
            tabPanel("By Population (Sum across Loci)",
              br(),
              DT::dataTableOutput("data_summary_by_pop_sum"),
              br(),
              downloadButton("download_summary_pop_sum", ".csv", class = "btn-primary btn-sm"),
              downloadButton("download_summary_pop_sum_txt", ".txt", class = "btn-outline-primary btn-sm")
            ),
            tabPanel("Global (Total across All)",
              br(),
              DT::dataTableOutput("data_summary_global"),
              br(),
              downloadButton("download_summary_global", ".csv", class = "btn-primary btn-sm"),
              downloadButton("download_summary_global_txt", ".txt", class = "btn-outline-primary btn-sm")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(6,
        box(
          title = "Analysis Parameters", 
          status = "primary", 
          solidHeader = TRUE,
          width = NULL, height = "200px",
          selectInput("selected_population", 
                     "Select Population:",
                     choices = c("All Populations" = "all"),
                     multiple = FALSE),
          selectInput("selected_marker",
                     "Select Marker:",
                     choices = NULL,
                     multiple = FALSE),
          actionButton("update_analysis", "Update Analysis", 
                      class = "btn-primary")
        )
      ),
      column(6,
        box(
          title = "Population Summary", 
          status = "warning", 
          solidHeader = TRUE, width = NULL, height = "200px",
          tableOutput("populations_Summary_table")
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "Allele Frequencies", 
          status = "success", 
          solidHeader = TRUE, width = NULL,
          tabsetPanel(
            tabPanel("By Population",
              br(),
              fluidRow(
                column(9,
                  DT::dataTableOutput("allele_freq_by_pop")
                ),
                column(3,
                  h5("Export Options"),
                  downloadButton("download_freq_pop", "Export CSV", 
                               class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  downloadButton("download_freq_pop_excel", "Export Excel", 
                               class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  br(),
                  h5("Quick Statistics"),
                  verbatimTextOutput("quick_stats_pop", placeholder = TRUE)
                )
              )
            ),
            tabPanel("By Locus",
              br(),
              fluidRow(
                column(9,
                  DT::dataTableOutput("allele_freq_global")
                ),
                column(3,
                  h5("Export Options"),
                  downloadButton("download_freq_global", "Export CSV", 
                               class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  downloadButton("download_freq_global_excel", "Export Excel", 
                               class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  br(),
                  h5("Global Statistics"),
                  verbatimTextOutput("quick_stats_global", placeholder = TRUE)
                )
              )
            ),
            tabPanel("Plots",
              br(),
              fluidRow(
                column(6,
                  plotOutput("allele_freq_plot", height = "400px")
                ),
                column(6,
                  plotOutput("missing_data_plot", height = "400px")
                )
              ),
              br(),
              fluidRow(
                column(6,
                  plotOutput("diversity_plot", height = "400px")
                ),
                column(6,
                  plotOutput("allele_richness_plot", height = "400px")
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "Marker Details", 
          status = "info", 
          solidHeader = TRUE, width = NULL,
          DT::dataTableOutput("marker_details")
        )
      )
    )
  )
}