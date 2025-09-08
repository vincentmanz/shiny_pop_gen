# ui_general_stats.R

general_stats_UI <- function() {
  fluidPage(
    useWaiter(),
    
    # Section 1: Basic Statistics
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
      box(
        width = 9,
        title = div(style = "background-color: #756bb1; padding: 10px; color: black;", "General statistics"),
        solidHeader = TRUE,
        uiOutput("basic_stats_ui"),
        style = "overflow-y: auto;"
      )
    ),
    
    # Section 2: Basic Plots
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
    
    # Section 3: Bootstrap on Loci
    tags$hr(),
    h2("Bootstrap Analyses", style = "color: #756bb1; text-align: center;"),
    
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #9b59b6; padding: 10px; color: white;", "Bootstrap on Loci"),
        solidHeader = TRUE,
        p("Resample loci with replacement to obtain confidence intervals for each parameter."),
        numericInput("boot_loci_n", "Number of bootstrap replicates", 
                     value = 5000, min = 100, max = 10000, step = 100),
        h5("Parameters to bootstrap:"),
        checkboxInput("boot_fis_wc", "FIS (Weir & Cockerham)", TRUE),
        checkboxInput("boot_fst_wc", "FST (Weir & Cockerham)", TRUE),
        checkboxInput("boot_fit_wc", "FIT (Weir & Cockerham)", TRUE),
        checkboxInput("boot_hs_nei", "HS (Nei)", TRUE),
        checkboxInput("boot_ht_nei", "HT (Nei)", TRUE),
        h5("Confidence intervals:"),
        checkboxInput("ci_95", "95% CI", TRUE),
        checkboxInput("ci_99", "99% CI", FALSE),
        tags$hr(),
        actionButton("run_boot_loci", "Run Bootstrap on Loci", 
                     icon = icon("dice"), class = "btn-warning")
      ),
      box(
        width = 8,
        title = div(style = "background-color: #9b59b6; padding: 10px; color: white;", "Bootstrap on Loci Results"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("Summary Table", 
                   tableOutput("boot_loci_summary"),
                   downloadButton("download_boot_loci_csv", "Download CSV", class = "btn btn-primary")),
          tabPanel("By Population", 
                   tableOutput("boot_loci_by_pop")),
          tabPanel("By Locus", 
                   tableOutput("boot_loci_by_locus")),
          tabPanel("Overall", 
                   tableOutput("boot_loci_overall"))
        ),
        style = "overflow-y: auto; max-height: 500px;"
      )
    ),
    
    # Section 4: Bootstrap on Populations
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #e67e22; padding: 10px; color: white;", "Bootstrap on Populations"),
        solidHeader = TRUE,
        p("Resample populations with replacement to obtain confidence intervals for each locus."),
        numericInput("boot_pop_n", "Number of bootstrap replicates", 
                     value = 5000, min = 100, max = 10000, step = 100),
        h5("Parameters to bootstrap:"),
        checkboxInput("boot_pop_fis_wc", "FIS (Weir & Cockerham)", TRUE),
        checkboxInput("boot_pop_fst_wc", "FST (Weir & Cockerham)", TRUE),
        checkboxInput("boot_pop_fit_wc", "FIT (Weir & Cockerham)", TRUE),
        checkboxInput("boot_pop_hs_nei", "HS (Nei)", TRUE),
        checkboxInput("boot_pop_ht_nei", "HT (Nei)", TRUE),
        h5("Options:"),
        checkboxInput("exclude_single_pop", "Exclude single population resamples", TRUE),
        checkboxInput("boot_pop_ci_95", "95% CI", TRUE),
        checkboxInput("boot_pop_ci_99", "99% CI", FALSE),
        tags$hr(),
        actionButton("run_boot_pop", "Run Bootstrap on Populations", 
                     icon = icon("users"), class = "btn-warning")
      ),
      box(
        width = 8,
        title = div(style = "background-color: #e67e22; padding: 10px; color: white;", "Bootstrap on Populations Results"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("Summary Table", 
                   tableOutput("boot_pop_summary"),
                   downloadButton("download_boot_pop_csv", "Download CSV", class = "btn btn-primary")),
          tabPanel("By Locus", 
                   tableOutput("boot_pop_by_locus")),
          tabPanel("Overall", 
                   tableOutput("boot_pop_overall")),
          tabPanel("Excluded Samples", 
                   textOutput("boot_pop_excluded_info"))
        ),
        style = "overflow-y: auto; max-height: 500px;"
      )
    ),
    
    # Section 5: Permutation Tests
    tags$hr(),
    h2("Permutation Tests", style = "color: #756bb1; text-align: center;"),
    
    # Local Panmixia Test
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #27ae60; padding: 10px; color: white;", "Local Panmixia Test"),
        solidHeader = TRUE,
        p("Test for random mating within each population by permuting alleles among individuals."),
        numericInput("perm_local_n", "Number of permutations", 
                     value = 10000, min = 100, max = 100000, step = 1000),
        h5("Test parameters:"),
        p("• Randomize alleles within each population"),
        p("• Calculate FIS (Weir & Cockerham) for each permutation"),
        p("• Compare with observed values"),
        h5("Output:"),
        p("• P-value for each locus in each population"),
        p("• Mean across loci within populations"),
        p("• Mean across populations for each locus"),
        p("• Overall mean across all loci and populations"),
        tags$hr(),
        actionButton("run_perm_local", "Run Local Panmixia Test", 
                     icon = icon("shuffle"), class = "btn-success")
      ),
      box(
        width = 8,
        title = div(style = "background-color: #27ae60; padding: 10px; color: white;", "Local Panmixia Results"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("P-values by Pop & Locus", 
                   tableOutput("perm_local_by_pop_locus"),
                   downloadButton("download_perm_local_csv", "Download CSV", class = "btn btn-primary")),
          tabPanel("Mean by Population", 
                   tableOutput("perm_local_by_pop")),
          tabPanel("Mean by Locus", 
                   tableOutput("perm_local_by_locus")),
          tabPanel("Overall Results", 
                   tableOutput("perm_local_overall")),
          tabPanel("Significance Summary",
                   h5("Significant results (α = 0.05):"),
                   tableOutput("perm_local_significant"))
        ),
        style = "overflow-y: auto; max-height: 500px;"
      )
    ),
    
    # Global Panmixia Test
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #2980b9; padding: 10px; color: white;", "Global Panmixia Test"),
        solidHeader = TRUE,
        p("Test for random mating across all populations by permuting alleles among all individuals."),
        numericInput("perm_global_n", "Number of permutations", 
                     value = 10000, min = 100, max = 100000, step = 1000),
        h5("Test parameters:"),
        p("• Randomize alleles among all individuals"),
        p("• Calculate FIT (Weir & Cockerham) for each permutation"),
        p("• Compare with observed values"),
        h5("Output:"),
        p("• P-value for each locus"),
        p("• Overall P-value across all loci"),
        tags$hr(),
        actionButton("run_perm_global", "Run Global Panmixia Test", 
                     icon = icon("globe"), class = "btn-info")
      ),
      box(
        width = 8,
        title = div(style = "background-color: #2980b9; padding: 10px; color: white;", "Global Panmixia Results"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("P-values by Locus", 
                   tableOutput("perm_global_by_locus"),
                   downloadButton("download_perm_global_csv", "Download CSV", class = "btn btn-primary")),
          tabPanel("Overall Results", 
                   tableOutput("perm_global_overall")),
          tabPanel("Significance Summary",
                   h5("Significant results (α = 0.05):"),
                   tableOutput("perm_global_significant")),
          tabPanel("Distribution Plots",
                   plotOutput("perm_global_plots"))
        ),
        style = "overflow-y: auto; max-height: 500px;"
      )
    ),
    
    # Population Subdivision Test
    fluidRow(
      box(
        width = 4,
        title = div(style = "background-color: #8e44ad; padding: 10px; color: white;", "Population Subdivision Test"),
        solidHeader = TRUE,
        p("Test for population subdivision using G-test on allele frequency contingency tables."),
        numericInput("perm_subdiv_n", "Number of permutations", 
                     value = 10000, min = 100, max = 100000, step = 1000),
        h5("Test parameters:"),
        p("• Create contingency table of allele frequencies"),
        p("• Calculate G-statistic (log-likelihood ratio)"),
        p("• Permute complete individuals among populations"),
        p("• Compare observed vs. simulated G-values"),
        h5("Output:"),
        p("• G-statistic and P-value for each locus"),
        p("• Total G-statistic and P-value across all loci"),
        h5("Options:"),
        checkboxInput("show_contingency_tables", "Show contingency tables", FALSE),
        tags$hr(),
        actionButton("run_perm_subdiv", "Run Subdivision Test", 
                     icon = icon("table"), class = "btn-secondary")
      ),
      box(
        width = 8,
        title = div(style = "background-color: #8e44ad; padding: 10px; color: white;", "Population Subdivision Results"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("G-test Results", 
                   tableOutput("perm_subdiv_results"),
                   downloadButton("download_perm_subdiv_csv", "Download CSV", class = "btn btn-primary")),
          tabPanel("By Locus", 
                   tableOutput("perm_subdiv_by_locus")),
          tabPanel("Overall Results", 
                   tableOutput("perm_subdiv_overall")),
          tabPanel("Contingency Tables", 
                   conditionalPanel(
                     condition = "input.show_contingency_tables",
                     uiOutput("contingency_tables_ui")
                   )),
          tabPanel("Significance Summary",
                   h5("Significant results (α = 0.05):"),
                   tableOutput("perm_subdiv_significant"))
        ),
        style = "overflow-y: auto; max-height: 500px;"
      )
    ),
    
    # Section 6: Comprehensive Results Summary
    tags$hr(),
    h2("Results Summary", style = "color: #756bb1; text-align: center;"),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #34495e; padding: 10px; color: white;", "Comprehensive Analysis Summary"),
        solidHeader = TRUE,
        tabsetPanel(
          tabPanel("Statistical Overview", 
                   h4("Basic Statistics Summary"),
                   tableOutput("summary_basic_stats"),
                   h4("Bootstrap Confidence Intervals"),
                   tableOutput("summary_bootstrap"),
                   h4("Permutation Test Results"),
                   tableOutput("summary_permutation")),
          tabPanel("Significance Tests",
                   h4("All Significance Tests (α = 0.05)"),
                   tableOutput("summary_all_significant")),
          tabPanel("Comparison Plots",
                   plotOutput("comparison_plots", height = "800px")),
          tabPanel("Export All Results",
                   h4("Download Complete Analysis"),
                   p("Download all results in a single Excel file with multiple sheets."),
                   downloadButton("download_complete_analysis", "Download Complete Analysis (.xlsx)", 
                                  class = "btn btn-success btn-lg"),
                   br(), br(),
                   h5("Individual Downloads:"),
                   fluidRow(
                     column(3, downloadButton("download_basic_stats_final", "Basic Stats", class = "btn btn-primary")),
                     column(3, downloadButton("download_bootstrap_final", "Bootstrap Results", class = "btn btn-warning")),
                     column(3, downloadButton("download_permutation_final", "Permutation Tests", class = "btn btn-success")),
                     column(3, downloadButton("download_plots_final", "All Plots (.zip)", class = "btn btn-info"))
                   ))
        ),
        style = "overflow-y: auto;"
      )
    )
  )
}