# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    useWaiter(),
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; font-size: 16px; }
        .success-box { border-left: 4px solid #28a745; }
        .warning-box { border-left: 4px solid #ffc107; }
        .info-box { border-left: 4px solid #17a2b8; }
        .value-box { font-size: 24px; font-weight: bold; text-align: center; }
        .shiny-notification { position: fixed; top: 20px; right: 20px; }
        .section-title { 
          color: #756bb1; 
          text-align: center; 
          margin: 20px 0; 
          padding: 10px;
          border-bottom: 2px solid #756bb1;
        }
        .function-box { 
          margin-bottom: 15px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-function { 
          width: 100%; 
          margin-top: 10px;
        }
        h4, h5 {
          font-weight: bold;
        }
        .value-box .small-box h3 {
          font-size: 28px !important;
          font-weight: bold;
        }
        .value-box .small-box p {
          font-size: 14px !important;
          margin-bottom: 0;
        }
        /* Styles pour les progress bars */
        .progress-bar {
          border-radius: 10px;
          height: 20px;
          transition: width 0.5s ease;
        }
        .progress {
          height: 20px;
          margin-bottom: 10px;
        }
        .status-indicator {
          transition: all 0.3s ease;
        }
        .status-indicator:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    # ===== SECTION 1: BASIC STATISTICS =====
    h2("Basic diversity and differentiation statistics", class = "section-title"),
    
    fluidRow(
      box(
        width = 3,
        title = div(style = "background-color: #756bb1; padding: 10px; color: white;", icon("chart-bar"), "Statistics Configuration"),
        h4("Select Genetic Indices"),
        solidHeader = TRUE,
        status = "primary",
        
        # Diversity indices
        h5("Diversity Measures:"),
        checkboxInput("ho_checkbox", "Ho (Observed Heterozygosity)", TRUE),
        checkboxInput("hs_checkbox", "Hs (Expected Heterozygosity within populations)", TRUE),
        checkboxInput("ht_checkbox", "Ht (Total expected heterozygosity)", TRUE),
        
        # F-statistics
        h5("F-statistics (Weir & Cockerham):"),
        checkboxInput("fit_wc_checkbox", "FIT (Total inbreeding coefficient)", TRUE),
        checkboxInput("fis_wc_checkbox", "FIS (Within-population inbreeding)", TRUE),
        checkboxInput("fst_wc_checkbox", "FST (Population differentiation)", TRUE),
        
        # Alternative estimators
        h5("Alternative Estimators:"),
        checkboxInput("fis_n_checkbox", "Fis (Nei's estimator)", FALSE),
        checkboxInput("fst_n_checkbox", "Fst (Nei's estimator)", FALSE),
        
        # Advanced statistics
        h5("Advanced Statistics:"),
        checkboxInput("fst_max_checkbox", "Fst-max (Maximum differentiation)", FALSE),
        checkboxInput("fst_prim_checkbox", "Fst' (Scaled differentiation)", FALSE),
        checkboxInput("GST_checkbox", "GST (Nei's genetic differentiation)", FALSE),
        checkboxInput("GST_sec_checkbox", "GST'' (Hedrick's correction)", FALSE),
        
        tags$hr(),
        actionButton("run_basic_stats", "Compute Statistics", icon = icon("calculator"), class = "btn-primary btn-block")
      ),
      box(
        width = 9,
        title = div(style = "background-color: #756bb1; padding: 10px; color: white;", icon("table"), "General statistics"),
        solidHeader = TRUE,
        status = "primary",
        tabsetPanel(
          tabPanel("Summary Table",
            DTOutput("basic_stats_table"),
            br(),
            downloadButton("download_basic_stats", ".csv", class = "btn-primary"),
            downloadButton("download_basic_stats_txt", ".txt", class = "btn-outline-primary")
          ),
          tabPanel("Gene Diversity by Population",
            h5("Expected heterozygosity (Hs) per locus and population", style = "margin-top: 10px;"),
            DTOutput("gene_diversity_table"),
            br(),
            downloadButton("download_gene_diversity", ".csv", class = "btn-primary"),
            downloadButton("download_gene_diversity_txt", ".txt", class = "btn-outline-primary")
          ),
          tabPanel("By Population",
            h5("Population-specific Statistics", style = "margin-top: 10px;"),
            selectInput("selected_pop_overall", "Select Population:", choices = NULL),
            DTOutput("basic_stats_by_pop_selected"),
            br(),
            downloadButton("download_pop_stats", ".csv", class = "btn-primary"),
            downloadButton("download_pop_stats_txt", ".txt", class = "btn-outline-primary")
          ),
          tabPanel("Overall Statistics",
            h5("Comprehensive Summary Statistics", style = "margin-top: 10px;"),
            tabsetPanel(
              tabPanel("Global Averages",
                tableOutput("overall_global_avg"),
                downloadButton("download_global_avg", ".csv", class = "btn-primary"),
                downloadButton("download_global_avg_txt", ".txt", class = "btn-outline-primary")
              ),
              tabPanel("By Population",
                tableOutput("overall_by_pop"),
                br(),
                downloadButton("download_overall_by_pop", ".csv", class = "btn-primary"),
                downloadButton("download_overall_by_pop_txt", ".txt", class = "btn-outline-primary")
              ),
              tabPanel("By Locus",
                tableOutput("overall_by_locus"),
                br(),
                downloadButton("download_overall_by_locus", ".csv", class = "btn-primary"),
                downloadButton("download_overall_by_locus_txt", ".txt", class = "btn-outline-primary")
              )
            )
          )
        ),
        style = "overflow-y: auto; max-height: 600px;"
      )
    ),

    # ===== SECTION 2: BOOTSTRAP FIS ANALYSIS =====
    h2("Advanced FIS Analysis", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #2e8b57; padding: 10px; color: white;", icon("rocket"), "Bootstrap & Permutation Analysis"),
        solidHeader = TRUE,
        status = "success",
        fluidRow(
          column(3,
                 h4(icon("cogs"), "Configuration"),
                 numericInput("n_perm", "Number of Permutations:", value = 100, min = 10, max = 10000),
                 numericInput("n_boot", "Number of Bootstrap Replicates:", value = 100, min = 10, max = 10000),
                 numericInput("conf_level", "Confidence Level:", value = 0.95, min = 0.80, max = 0.99, step = 0.01),
                 selectInput("analysis_level", "Analysis Level:", 
                            choices = c("Global", "By Population", "By Locus")),
                 actionButton("run_bootstrap_fis", "Run Advanced Analysis", icon = icon("chart-line"), class = "btn-success btn-block")
          ),
          column(9,
            h4(icon("chart-line"), "Analysis Summary", style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3, valueBoxOutput("global_fis_box", width = NULL)),
              column(3, valueBoxOutput("global_pvalue_box", width = NULL)),
              column(3, valueBoxOutput("significant_loci_box", width = NULL)),
              column(3, valueBoxOutput("analysis_time_box", width = NULL))
            ),
            # Progress bars for analysis - CORRECTION ICI
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = "fis_progress", value = 0, title = "Overall Progress"),
                fluidRow(
                  column(6, shinyWidgets::progressBar(id = "fis_bootstrap_progress", value = 0, title = "Bootstrap")),
                  column(6, shinyWidgets::progressBar(id = "fis_permutation_progress", value = 0, title = "Permutation"))
                )
              )
            ),
            # Style additionnel pour les valueBox
            tags$style(HTML("
              .small-box {
                border-radius: 12px !important;
                box-shadow: 0 4px 12px rgba(0,0,0,0.08) !important;
                transition: transform 0.2s, box-shadow 0.2s;
                margin-bottom: 15px;
              }
              .small-box:hover {
                transform: translateY(-3px);
                box-shadow: 0 6px 16px rgba(0,0,0,0.12) !important;
              }
              .small-box h3 {
                font-size: 26px !important;
                font-weight: 700 !important;
                padding: 10px 0 !important;
                color: #000000 !important;
              }
              .small-box p {
                font-size: 13px !important;
                opacity: 0.9 !important;
                margin-bottom: 0 !important;
                color: #000000 !important;
              }
            "))
          )
        )
      )
    ),
    
    # ===== SECTION 3: FIS RESULTS TABLE =====
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #4682b4; padding: 10px; color: white;", icon("table"), "FIS Results with Confidence Intervals"),
        solidHeader = TRUE,
        status = "primary",
        fluidRow(
          column(3,
                 h4(icon("sliders"), "Table Options"),
                 selectInput("results_view", "View:", choices = c("All Loci", "Significant Only", "By Population")),
                 checkboxInput("show_ci", "Show Confidence Intervals", TRUE),
                 checkboxInput("show_pvalues", "Show P-values", TRUE),
                 checkboxInput("format_numbers", "Format Numbers", TRUE),
                 downloadButton("download_fis_table", ".csv", class = "btn-primary"),
                 downloadButton("download_fis_table_txt", ".txt", class = "btn-outline-primary")
          ),
          column(9,
                 DTOutput("fis_results_table")
          )
        )
      )
    ),
    
    # ===== SECTION 4: PERMUTATION & BOOTSTRAP VISUALIZATION =====
    h2("Permutation & Bootstrap Visualization", class = "section-title"),
    
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #ff7f50; padding: 10px; color: white;", icon("dice"), "Permutation Test Results"),
        solidHeader = TRUE,
        status = "warning",
        plotOutput("permutation_plot", height = "300px"),
        verbatimTextOutput("permutation_summary"),
        downloadButton("download_perm_plot", ".png", class = "btn-warning")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #9370db; padding: 10px; color: white;", icon("chart-bar"), "Bootstrap Distribution"),
        solidHeader = TRUE,
        status = "info",
        plotOutput("bootstrap_plot", height = "300px"),
        verbatimTextOutput("bootstrap_summary"),
        downloadButton("download_boot_plot", ".png", class = "btn-info")
      )
    ),
    
    # ===== SECTION 5: PERMUTATION & BOOTSTRAP DATA TABLES =====
    h2("Permutation & Bootstrap Data Tables", class = "section-title"),
    
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #ff7f50; padding: 10px; color: white;", icon("table"), "Permutation Test Data"),
        solidHeader = TRUE,
        status = "warning",
        DTOutput("permutation_table"),
        br(),
        downloadButton("download_perm_table", ".csv", class = "btn-warning"),
        downloadButton("download_perm_table_txt", ".txt", class = "btn-outline-warning"),
        verbatimTextOutput("permutation_stats")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #9370db; padding: 10px; color: white;", icon("table"), "Bootstrap Distribution Data"),
        solidHeader = TRUE,
        status = "info",
        DTOutput("bootstrap_table"),
        br(),
        downloadButton("download_boot_table", ".csv", class = "btn-info"),
        downloadButton("download_boot_table_txt", ".txt", class = "btn-outline-info"),
        verbatimTextOutput("bootstrap_stats")
      )
    ),

    h2("Detailed Analysis Reports", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #32cd32; padding: 10px; color: white;", icon("search"), "Detailed Statistical Reports"),
        solidHeader = TRUE,
        status = "success",
        tabsetPanel(
          tabPanel("Locus Details",
                   selectInput("selected_locus", "Select Locus:", choices = NULL),
                   fluidRow(
                     column(6,
                            h4("Observed vs Permuted Distribution"),
                            plotOutput("locus_perm_plot", height = "250px"),
                            br(),
                            downloadButton("download_locus_perm_plot", ".png", class = "btn-warning")
                     ),
                     column(6,
                            h4("Bootstrap Confidence Interval"),
                            plotOutput("locus_boot_plot", height = "250px"),
                            br(),
                            downloadButton("download_locus_boot_plot", ".png", class = "btn-info")
                     )
                   ),
                   br(),
                   verbatimTextOutput("locus_statistical_report")
          ),
          tabPanel("Unilateral P-values",
            h4("One-sided P-values for FIS Deviation"),
            p("This table shows one-sided p-values for directional hypotheses:"),
            tags$ul(
              tags$li(
                tags$strong("P (FIS > 0)"), ": tests for heterozygote deficit"
              ),
              tags$li(
                tags$strong("P (FIS < 0)"), ": tests for heterozygote excess"
              )
            ),
            p(
              tags$em("Note: Bilateral p-values are also computed and used for significance testing. One-sided p-values are derived from bilateral p-values assuming symmetry of the null distribution.")
            ),
            DTOutput("unilateral_pvalues_table"),
            br(),
            downloadButton("download_unilateral_pvals", ".csv", class = "btn-primary"),
            downloadButton("download_unilateral_pvals_txt", ".txt", class = "btn-outline-primary")
          ),
          tabPanel("Quality Control",
                   h4("Bootstrap Quality Metrics"),
                   plotOutput("qc_plot", height = "300px"),
                   verbatimTextOutput("qc_summary"),
                   h4("Data Completeness"),
                   plotOutput("completeness_plot", height = "250px")
          )
        )
      )
    ),

    # ===== SECTION FIT: ADVANCED FIT ANALYSIS =====
    h2("Advanced FIT Analysis", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #ff6347; padding: 10px; color: white;", icon("dna"), "FIT Bootstrap & Permutation Analysis"),
        solidHeader = TRUE,
        status = "danger",
        fluidRow(
          column(3,
            h4(icon("cogs"), "Analysis Configuration"),
            numericInput("n_perm_fit", "Number of Permutations:", value = 1000, min = 100, max = 10000, step = 100),
            numericInput("n_boot_fit", "Number of Bootstrap Replicates:", value = 1000, min = 100, max = 10000, step = 100),
            numericInput("conf_level_fit", "Confidence Level:", value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            selectInput("bootstrap_method_fit", "Bootstrap Method:",
                       choices = c("Two-stage (Populations + Individuals)" = "two_stage",
                                  "Population-only" = "pop_only",
                                  "Both Methods (Comparison)" = "both")),
            selectInput("analysis_level_fit", "Analysis Level:", 
                       choices = c("Global" = "global", 
                                  "By Population" = "by_pop", 
                                  "By Locus" = "by_locus",
                                  "All Levels" = "all")),
            checkboxGroupInput("fit_metrics", "Additional Metrics:",
                              choices = c("Confidence Intervals" = "ci",
                                         "P-values" = "pval",
                                         "Effect Sizes" = "effect",
                                         "Standard Errors" = "se",
                                         "Bias Correction" = "bias_corr")),
            actionButton("run_bootstrap_fit", "Run FIT Analysis", icon = icon("rocket"), 
                        class = "btn-danger btn-block", style = "font-weight: bold;")
          ),
          column(9,
            h4(icon("chart-line"), "FIT Analysis Summary", style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3, 
                valueBoxOutput("global_fit_box", width = NULL),
                valueBoxOutput("fit_ci_width_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("global_fit_pvalue_box", width = NULL),
                valueBoxOutput("fit_power_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("significant_loci_fit_box", width = NULL),
                valueBoxOutput("fit_convergence_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("analysis_time_fit_box", width = NULL),
                valueBoxOutput("fit_quality_box", width = NULL)
              )
            ),
            # Progress bars for analysis - CORRECTION ICI
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                # Utilisation de shinyWidgets::progressBar au lieu de progressBar
                shinyWidgets::progressBar(id = "fit_progress", value = 0, title = "Overall Progress"),
                fluidRow(
                  column(6, shinyWidgets::progressBar(id = "fit_bootstrap_progress", value = 0, title = "Bootstrap")),
                  column(6, shinyWidgets::progressBar(id = "fit_permutation_progress", value = 0, title = "Permutation"))
                )
              )
            )
          )
        )
      )
    ),
    
    # FIT Results Tabs
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #ff6347; padding: 10px; color: white;", icon("table"), "FIT Results with Confidence Intervals"),
        solidHeader = TRUE,
        status = "danger",
        tabsetPanel(
          tabPanel("Main Results",
            fluidRow(
              column(3,
                h4(icon("sliders"), "Table Options"),
                selectInput("results_view_fit", "View Level:", 
                           choices = c("All Loci" = "all_loci", 
                                      "Significant Only" = "significant",
                                      "By Population" = "by_pop",
                                      "Global Summary" = "global")),
                selectInput("sort_by_fit", "Sort By:",
                           choices = c("FIT Value" = "fit",
                                      "P-value" = "pvalue", 
                                      "Confidence Interval Width" = "ci_width",
                                      "Locus Name" = "locus")),
                checkboxGroupInput("fit_display_options", "Display Options:",
                                  choices = c("Show Confidence Intervals" = "show_ci",
                                             "Show P-values" = "show_pvalues",
                                             "Show Standard Errors" = "show_se",
                                             "Format Numbers" = "format_numbers",
                                             "Highlight Significant" = "highlight_sig",
                                             "Show Bootstrap Diagnostics" = "show_diag"),
                                  selected = c("show_ci", "show_pvalues", "format_numbers", "highlight_sig")),
                numericInput("fit_alpha", "Significance Level:", value = 0.05, min = 0.001, max = 0.1, step = 0.01),
                downloadButton("download_fit_table", "Download Table (.csv)", class = "btn-danger"),
                downloadButton("download_fit_table_txt", "Download Table (.txt)", class = "btn-outline-danger")
              ),
              column(9,
                DTOutput("fit_results_table"),
                br(),
                fluidRow(
                  column(6,
                    plotOutput("fit_distribution_plot", height = "200px")
                  ),
                  column(6,
                    plotOutput("fit_qq_plot", height = "200px")
                  )
                )
              )
            )
          ),
          tabPanel("Bootstrap Comparison",
            h4("Bootstrap Method Comparison"),
            fluidRow(
              column(6,
                plotOutput("fit_bootstrap_comparison_plot", height = "300px")
              ),
              column(6,
                plotOutput("fit_ci_comparison_plot", height = "300px")
              )
            ),
            DTOutput("fit_bootstrap_comparison_table"),
            downloadButton("download_fit_comparison", "Download Comparison Table", class = "btn-warning")
          ),
          tabPanel("Detailed Diagnostics",
            h4("Bootstrap Diagnostics"),
            fluidRow(
              column(4,
                plotOutput("fit_bootstrap_diagnostics", height = "250px")
              ),
              column(4,
                plotOutput("fit_convergence_plot", height = "250px")
              ),
              column(4,
                plotOutput("fit_bias_plot", height = "250px")
              )
            ),
            verbatimTextOutput("fit_diagnostics_summary")
          )
        )
      )
    ),
    
    # FIT Visualization Section
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #ff8c69; padding: 10px; color: white;", icon("chart-bar"), "FIT Permutation Results"),
        solidHeader = TRUE,
        status = "warning",
        tabsetPanel(
          tabPanel("Distribution Plot",
            plotOutput("permutation_plot_fit", height = "300px"),
            fluidRow(
              column(6,
                numericInput("perm_bins_fit", "Number of Bins:", value = 30, min = 10, max = 100)
              ),
              column(6,
                selectInput("perm_plot_type_fit", "Plot Type:",
                           choices = c("Histogram" = "histogram",
                                      "Density" = "density",
                                      "ECDF" = "ecdf"))
              )
            )
          ),
          tabPanel("Summary Statistics",
            verbatimTextOutput("permutation_summary_fit"),
            tableOutput("permutation_stats_table_fit")
          )
        ),
        downloadButton("download_perm_plot_fit", "Download Plot", class = "btn-warning"),
        downloadButton("download_perm_data_fit", "Download Data", class = "btn-outline-warning")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #ff7f50; padding: 10px; color: white;", icon("chart-line"), "FIT Bootstrap Results"),
        solidHeader = TRUE,
        status = "info",
        tabsetPanel(
          tabPanel("Bootstrap Distribution",
            plotOutput("bootstrap_plot_fit", height = "300px"),
            fluidRow(
              column(6,
                selectInput("boot_plot_type_fit", "Plot Type:",
                           choices = c("Density" = "density",
                                      "Histogram" = "histogram",
                                      "Boxplot" = "boxplot",
                                      "Violin" = "violin"))
              ),
              column(6,
                checkboxInput("show_true_value_fit", "Show True Value", TRUE)
              )
            )
          ),
          tabPanel("CI Analysis",
            plotOutput("fit_ci_plot", height = "300px"),
            verbatimTextOutput("bootstrap_summary_fit")
          )
        ),
        downloadButton("download_boot_plot_fit", "Download Plot", class = "btn-info"),
        downloadButton("download_boot_data_fit", "Download Data", class = "btn-outline-info")
      )
    ),
    
    # FIT Data Tables Section
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #ff8c69; padding: 10px; color: white;", icon("table"), "FIT Permutation Data"),
        solidHeader = TRUE,
        status = "warning",
        DTOutput("permutation_table_fit"),
        br(),
        fluidRow(
          column(6,
            downloadButton("download_perm_table_fit", "Download CSV", class = "btn-warning")
          ),
          column(6,
            downloadButton("download_perm_table_fit_txt", "Download TXT", class = "btn-outline-warning")
          )
        ),
        verbatimTextOutput("permutation_stats_fit")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #ff7f50; padding: 10px; color: white;", icon("database"), "FIT Bootstrap Data"),
        solidHeader = TRUE,
        status = "info",
        DTOutput("bootstrap_table_fit"),
        br(),
        fluidRow(
          column(6,
            downloadButton("download_boot_table_fit", "Download CSV", class = "btn-info")
          ),
          column(6,
            downloadButton("download_boot_table_fit_txt", "Download TXT", class = "btn-outline-info")
          )
        ),
        verbatimTextOutput("bootstrap_stats_fit")
      )
    ),

    # ===== SECTION FST: ADVANCED FST ANALYSIS =====
    h2("Advanced FST Analysis", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #4682b4; padding: 10px; color: white;", icon("globe-americas"), "FST Bootstrap & Permutation Analysis"),
        solidHeader = TRUE,
        status = "info",
        fluidRow(
          column(3,
            h4(icon("cogs"), "Analysis Configuration"),
            numericInput("n_perm_fst", "Number of Permutations:", value = 1000, min = 100, max = 10000, step = 100),
            numericInput("n_boot_fst", "Number of Bootstrap Replicates:", value = 1000, min = 100, max = 10000, step = 100),
            numericInput("conf_level_fst", "Confidence Level:", value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            selectInput("bootstrap_method_fst", "Bootstrap Method:",
                       choices = c("Two-stage (Populations + Individuals)" = "two_stage",
                                  "Population-only" = "pop_only",
                                  "Both Methods (Comparison)" = "both",
                                  "Jackknife" = "jackknife")),
            selectInput("fst_estimator", "FST Estimator:",
                       choices = c("Weir & Cockerham" = "wc",
                                  "Nei" = "nei",
                                  "Hudson" = "hudson",
                                  "All Estimators" = "all")),
            selectInput("analysis_level_fst", "Analysis Level:", 
                       choices = c("Global" = "global", 
                                  "By Population" = "by_pop", 
                                  "By Locus" = "by_locus",
                                  "Pairwise" = "pairwise",
                                  "All Levels" = "all")),
            checkboxGroupInput("fst_metrics", "Additional Metrics:",
                              choices = c("Confidence Intervals" = "ci",
                                         "P-values" = "pval",
                                         "Pairwise Comparisons" = "pairwise",
                                         "Differentiation Tests" = "differentiation",
                                         "Population Structure" = "structure")),
            actionButton("run_bootstrap_fst", "Run FST Analysis", icon = icon("rocket"), 
                        class = "btn-info btn-block", style = "font-weight: bold;")
          ),
          column(9,
            h4(icon("chart-line"), "FST Analysis Summary", style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3, 
                valueBoxOutput("global_fst_box", width = NULL),
                valueBoxOutput("fst_ci_width_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("global_fst_pvalue_box", width = NULL),
                valueBoxOutput("fst_power_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("significant_loci_fst_box", width = NULL),
                valueBoxOutput("fst_convergence_box", width = NULL)
              ),
              column(3,
                valueBoxOutput("analysis_time_fst_box", width = NULL),
                valueBoxOutput("fst_quality_box", width = NULL)
              )
            ),
            # Progress bars for FST analysis - CORRECTION ICI
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = "fst_progress", value = 0, title = "Overall Progress"),
                fluidRow(
                  column(6, shinyWidgets::progressBar(id = "fst_bootstrap_progress", value = 0, title = "Bootstrap")),
                  column(6, shinyWidgets::progressBar(id = "fst_permutation_progress", value = 0, title = "Permutation"))
                )
              )
            )
          )
        )
      )
    ),
    
    # FST Results Tabs
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #4682b4; padding: 10px; color: white;", icon("table"), "FST Results with Confidence Intervals"),
        solidHeader = TRUE,
        status = "info",
        tabsetPanel(
          tabPanel("Main Results",
            fluidRow(
              column(3,
                h4(icon("sliders"), "Table Options"),
                selectInput("results_view_fst", "View Level:", 
                           choices = c("All Loci" = "all_loci", 
                                      "Significant Only" = "significant",
                                      "By Population" = "by_pop",
                                      "Pairwise Comparisons" = "pairwise",
                                      "Global Summary" = "global")),
                selectInput("sort_by_fst", "Sort By:",
                           choices = c("FST Value" = "fst",
                                      "P-value" = "pvalue", 
                                      "Confidence Interval Width" = "ci_width",
                                      "Differentiation" = "differentiation",
                                      "Locus Name" = "locus")),
                checkboxGroupInput("fst_display_options", "Display Options:",
                                  choices = c("Show Confidence Intervals" = "show_ci",
                                             "Show P-values" = "show_pvalues",
                                             "Show Pairwise FST" = "show_pairwise",
                                             "Format Numbers" = "format_numbers",
                                             "Highlight Significant" = "highlight_sig",
                                             "Show Population Structure" = "show_structure"),
                                  selected = c("show_ci", "show_pvalues", "format_numbers", "highlight_sig")),
                numericInput("fst_alpha", "Significance Level:", value = 0.05, min = 0.001, max = 0.1, step = 0.01),
                downloadButton("download_fst_table", "Download Table (.csv)", class = "btn-info"),
                downloadButton("download_fst_table_txt", "Download Table (.txt)", class = "btn-outline-info")
              ),
              column(9,
                DTOutput("fst_results_table"),
                br(),
                fluidRow(
                  column(6,
                    plotOutput("fst_distribution_plot", height = "200px")
                  ),
                  column(6,
                    plotOutput("fst_estimator_comparison", height = "200px")
                  )
                )
              )
            )
          ),
          tabPanel("Pairwise FST",
            h4("Population Pairwise FST Matrix"),
            fluidRow(
              column(6,
                plotOutput("fst_heatmap", height = "300px")
              ),
              column(6,
                plotOutput("fst_mds_plot", height = "300px")
              )
            ),
            DTOutput("pairwise_fst_table"),
            downloadButton("download_pairwise_fst", "Download Pairwise Matrix", class = "btn-success")
          ),
          tabPanel("Bootstrap Comparison",
            h4("Bootstrap Method Comparison"),
            fluidRow(
              column(6,
                plotOutput("fst_bootstrap_comparison_plot", height = "300px")
              ),
              column(6,
                plotOutput("fst_ci_comparison_plot", height = "300px")
              )
            ),
            DTOutput("fst_bootstrap_comparison_table"),
            downloadButton("download_fst_comparison", "Download Comparison Table", class = "btn-warning")
          ),
          tabPanel("Population Structure",
            h4("Population Genetic Structure"),
            fluidRow(
              column(4,
                plotOutput("fst_structure_plot", height = "250px")
              ),
              column(4,
                plotOutput("fst_pca_plot", height = "250px")
              ),
              column(4,
                plotOutput("fst_dendrogram", height = "250px")
              )
            ),
            verbatimTextOutput("fst_structure_summary")
          )
        )
      )
    ),
    
    # FST Visualization Section
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #5f9ea0; padding: 10px; color: white;", icon("chart-bar"), "FST Permutation Results"),
        solidHeader = TRUE,
        status = "warning",
        tabsetPanel(
          tabPanel("Distribution Plot",
            plotOutput("permutation_plot_fst", height = "300px"),
            fluidRow(
              column(6,
                numericInput("perm_bins_fst", "Number of Bins:", value = 30, min = 10, max = 100)
              ),
              column(6,
                selectInput("perm_plot_type_fst", "Plot Type:",
                           choices = c("Histogram" = "histogram",
                                      "Density" = "density",
                                      "ECDF" = "ecdf",
                                      "QQ Plot" = "qq"))
              )
            )
          ),
          tabPanel("Differentiation Tests",
            verbatimTextOutput("differentiation_summary_fst"),
            tableOutput("differentiation_tests_table")
          )
        ),
        downloadButton("download_perm_plot_fst", "Download Plot", class = "btn-warning"),
        downloadButton("download_perm_data_fst", "Download Data", class = "btn-outline-warning")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #6495ed; padding: 10px; color: white;", icon("chart-line"), "FST Bootstrap Results"),
        solidHeader = TRUE,
        status = "info",
        tabsetPanel(
          tabPanel("Bootstrap Distribution",
            plotOutput("bootstrap_plot_fst", height = "300px"),
            fluidRow(
              column(6,
                selectInput("boot_plot_type_fst", "Plot Type:",
                           choices = c("Density" = "density",
                                      "Histogram" = "histogram",
                                      "Boxplot" = "boxplot",
                                      "Violin" = "violin"))
              ),
              column(6,
                checkboxInput("show_true_value_fst", "Show True Value", TRUE)
              )
            )
          ),
          tabPanel("CI Analysis",
            plotOutput("fst_ci_plot", height = "300px"),
            verbatimTextOutput("bootstrap_summary_fst")
          )
        ),
        downloadButton("download_boot_plot_fst", "Download Plot", class = "btn-info"),
        downloadButton("download_boot_data_fst", "Download Data", class = "btn-outline-info")
      )
    ),
    
    # FST Data Tables Section
    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #5f9ea0; padding: 10px; color: white;", icon("table"), "FST Permutation Data"),
        solidHeader = TRUE,
        status = "warning",
        DTOutput("permutation_table_fst"),
        br(),
        fluidRow(
          column(6,
            downloadButton("download_perm_table_fst", "Download CSV", class = "btn-warning")
          ),
          column(6,
            downloadButton("download_perm_table_fst_txt", "Download TXT", class = "btn-outline-warning")
          )
        ),
        verbatimTextOutput("permutation_stats_fst")
      ),
      box(
        width = 6,
        title = div(style = "background-color: #6495ed; padding: 10px; color: white;", icon("database"), "FST Bootstrap Data"),
        solidHeader = TRUE,
        status = "info",
        DTOutput("bootstrap_table_fst"),
        br(),
        fluidRow(
          column(6,
            downloadButton("download_boot_table_fst", "Download CSV", class = "btn-info")
          ),
          column(6,
            downloadButton("download_boot_table_fst_txt", "Download TXT", class = "btn-outline-info")
          )
        ),
        verbatimTextOutput("bootstrap_stats_fst")
      )
    ),

    # ===== SECTION: COMPARATIVE ANALYSIS =====
    h2("Comparative Analysis: FIT vs FST", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #9370db; padding: 10px; color: white;", icon("balance-scale"), "FIT-FST Comparative Analysis"),
        solidHeader = TRUE,
        status = "primary",
        fluidRow(
          column(4,
            h4(icon("cogs"), "Comparison Configuration"),
            selectInput("comparison_metric", "Comparison Metric:",
                       choices = c("Bootstrap Performance" = "bootstrap",
                                  "Statistical Power" = "power",
                                  "Confidence Intervals" = "ci",
                                  "Convergence Rates" = "convergence",
                                  "All Metrics" = "all")),
            checkboxGroupInput("comparison_options", "Comparison Options:",
                              choices = c("Show Method Comparison" = "methods",
                                         "Show Level Comparison" = "levels",
                                         "Show Time Comparison" = "time",
                                         "Show Quality Metrics" = "quality")),
            actionButton("run_comparison", "Run Comparative Analysis", icon = icon("balance-scale"), 
                        class = "btn-primary btn-block")
          ),
          column(8,
            h4(icon("chart-bar"), "Comparative Summary"),
            fluidRow(
              column(3, valueBoxOutput("comparison_correlation_box", width = NULL)),
              column(3, valueBoxOutput("comparison_agreement_box", width = NULL)),
              column(3, valueBoxOutput("comparison_efficiency_box", width = NULL)),
              column(3, valueBoxOutput("comparison_reliability_box", width = NULL))
            ),
            plotOutput("comparison_summary_plot", height = "200px")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #9370db; padding: 10px; color: white;", icon("chart-line"), "Comparative Visualizations"),
        solidHeader = TRUE,
        status = "primary",
        tabsetPanel(
          tabPanel("Method Comparison",
            fluidRow(
              column(6, plotOutput("bootstrap_method_comparison", height = "300px")),
              column(6, plotOutput("statistical_power_comparison", height = "300px"))
            )
          ),
          tabPanel("Performance Metrics",
            fluidRow(
              column(4, plotOutput("convergence_comparison", height = "250px")),
              column(4, plotOutput("ci_width_comparison", height = "250px")),
              column(4, plotOutput("bias_comparison", height = "250px"))
            )
          ),
          tabPanel("Quality Assessment",
            DTOutput("quality_metrics_table"),
            downloadButton("download_quality_metrics", "Download Quality Metrics", class = "btn-success")
          )
        )
      )
    ),
    
    # ===== SECTION 7: EXPORT AND REPORT GENERATION =====
    h2("Export & Report Generation", class = "section-title"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #ff4500; padding: 10px; color: white;", icon("file-export"), "Export Results"),
        solidHeader = TRUE,
        status = "danger",
        fluidRow(
          column(4,
                 h4(icon("file-alt"), "Report Generation"),
                 textInput("report_title", "Report Title:", value = "Genetic Diversity Analysis Report"),
                 selectInput("report_format", "Format:", choices = c("HTML", "PDF", "Word")),
                 actionButton("generate_report", "Generate Full Report", icon = icon("file-export"), class = "btn-danger")
          ),
          column(4,
                 h4(icon("download"), "Data Export"),
                 checkboxGroupInput("export_datasets", "Select Datasets to Export:",
                                   choices = c("FIS Results", "Bootstrap Distributions", "Permutation Tests", "Population Statistics", "Quality Metrics", "All Bootstrap Results")),
                 downloadButton("export_all_data", "Export Selected Data", class = "btn-primary")
          ),
          column(4,
                 h4(icon("info-circle"), "Session Information"),
                 verbatimTextOutput("session_info"),
                 actionButton("save_workspace", "Save Workspace", icon = icon("save"), class = "btn-info")
          )
        )
      )
    )
  )
}