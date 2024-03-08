# server_general_stats.R
################################################################################
# environment variables

filtered_data <- read.csv("~/projects/shiny_pop_gen/shiny_pop_gen/data/data-2023-09-11 (2).csv", header = TRUE)

# information inherited from previous page
n_marker <- 6
n_pop <- 8
marker_start <- 6
marker_end <- 11
sequence_length <- length(marker_start:marker_end)

n_indv <- nrow(filtered_data)
pops <- unique(filtered_data$Population)
## Specify the number of cores available
num_cores <- parallel::detectCores()
## Code for missing data
missing_data_code <- "0/0"
## genotype columns
column_genotype_start <- 6
column_genotype_end <- 11

# Specify the number of bootstrap replicates
R <- 1000
# Specify the number of replicates (HW-Panmixia)
n_rep <- 1000

# selected_stats <-
#   c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)", "GST")

################################################################################
# Define the server logic
server_general_stats <- function(input, output, session) {
  # General stats reactive value
  result_stats_reactive <- reactiveVal(NULL)
  # Missing data reactive value
  missing_data_reac <- reactiveVal(NULL)
  # Filtered data formated
  filtered_data_reac <- reactiveVal(NULL)
  # store the plot reactive values
  plot_output <- reactiveValues()
  # mydata_genind reactive value
  mydata_genind_reac <- reactiveVal(NULL)
  # mydata_hierfstat reactive value
  mydata_hierfstat_reac <- reactiveVal(NULL)

  ## Create genind object
  filtered_data <- data.frame(indv = paste(substr(filtered_data$Population, 1, 3), row.names(filtered_data), sep = "."), filtered_data)
  filtered_data_reac(filtered_data)
  # Compute genind object
  mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end], sep = "/", ncode = 6, ind.names = filtered_data$indv, pop = filtered_data$Population, NA.char = "NA", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL) # nolint: line_length_linter.
  mydata_genind_reac(mydata_genind)
  # Compute hierfstat object
  mydata_hierfstat <- genind2hierfstat(mydata_genind)
  mydata_hierfstat_reac(mydata_hierfstat)
  # create the missing df
  mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end], sep = "/", ncode = 6, ind.names = filtered_data$indv, pop = filtered_data$Population, NA.char = "0/0", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL)
  missing_data <- info_table(mydata_genind, plot = FALSE, percent = TRUE, df = TRUE)
  missing_data <- as.data.frame(missing_data) %>% spread(key = Locus, value = Missing)
  missing_data <- missing_data %>% column_to_rownames(var = "Population") ### !! alphabetically ordered index
  missing_data <- missing_data * 100
  missing_data_reac(missing_data)

  # Function to run the basic.stats and render the result
  observeEvent(input$run_basic_stats, {
    # Retrieve reactive value
    mydata_hierfstat_a <- mydata_hierfstat_reac()
    mydata_genind_a <- mydata_genind_reac()

    # Retrieve the stats selected values
    selected_stats <- c(
      "Ho" = input$ho_checkbox,
      "Hs" = input$hs_checkbox,
      "Ht" = input$ht_checkbox,
      "Fit (W&C)" = input$fit_wc_checkbox,
      "Fis (W&C)" = input$fis_wc_checkbox,
      "Fst (W&C)" = input$fst_wc_checkbox,
      "Fis (Nei)" = input$fis_n_checkbox,
      "Fst (Nei)" = input$fst_n_checkbox,
      "GST" = input$GST_checkbox,
      "GST''" = input$GST_sec_checkbox
    )

    # Check if any statistics are selected
    if (!any(selected_stats)) {
      # Show a shinyalert message if no statistics are selected
      shinyalert(
        title = "No Statistics Selected",
        text = "Please select at least one statistic to compute.",
        type = "warning"
      )

      # Stop execution of the rest of the code
      return(NULL)
    }

    # Compute Nei diversity
    result <- basic.stats(mydata_hierfstat_a) # hierfstat
    df_resutl_basic <- as.data.frame(result$perloc)

    # Compute Weir and Cockrham estimates of F-statistics - FIS and FST
    data <- as.data.frame(as.loci(mydata_genind_a))
    result_f_stats <- Fst(as.loci(data)) # pegas
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")

    # Compute the Gstats
    df_resutl_basic <- df_resutl_basic %>% mutate(
      GST = 1 - Hs / Ht,
      `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
    )

    # Merge the stats and formatting the output table
    result_stats <- merge(result_f_stats, df_resutl_basic, by = "row.names", all.x = TRUE)
    colnames(result_stats)[11:12] <- c("Fst (Nei)", "Fis (Nei)")
    rownames(result_stats) <- result_stats[, 1]
    result_stats <- result_stats[, -1]
    result_stats_reactive(result_stats)

    # Select the required columns
    result_stats_select <- result_stats[, names(selected_stats)[selected_stats], drop = FALSE]

    # Render the result
    output$basic_stats_result <- renderTable({
      req(result_stats_select)
      result_stats_select
    })

    # Downloadable csv of selected dataset
    output$download_gstats_csv <- downloadHandler(
      filename = function() {
        paste("basic_stats_result", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(result_stats_select, file, row.names = TRUE)
      }
    )
  })
  # Function to handle the plot generation
  observeEvent(input$run_plot_heatmap, {
    # Retrieve reactive value
    filtered_data_indv <- filtered_data_reac()
    missing_data <- missing_data_reac()

    # Data shaping
    missing_data <- rownames_to_column(missing_data, var = "location")

    ## Melt the data for ggplot2
    heatmap_data_melted <- melt(missing_data, value.name = "location")
    colnames(heatmap_data_melted) <- c("location", "marker", "percent")

    # Generate the heatmap plot
    heatmap_missing <- ggplot(data = heatmap_data_melted, aes(x = location, y = marker, fill = percent)) +
      geom_tile() +
      labs(x = "Column Names", y = "Row Names", fill = "Value") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better visibility

    output$plot_output <- renderPlot({
      heatmap_missing
    })

    # Downloadable png of the selected plot ----
    output$download_plot_png <- downloadHandler(
      filename = function() {
        "plot_heatmap_missing.png"
      },
      content = function(file) {
        ggsave(filename = file, plot = heatmap_missing, dpi = 300)
      }
    )
  })

  # Function to handle the plot generation for GST
  observeEvent(input$run_plot_GST, {
    # Retrieve reactive values
    result_stats <- result_stats_reactive()

    # Generate the GST plot with linear trend
    plot_GST <- ggplot(data = result_stats, aes(x = GST, y = Hs)) +
      geom_point() +
      geom_smooth(method = lm, color = "red", se = FALSE) +
      theme_ipsum()

    output$plot_output <- renderPlot({
      plot_GST
    })

    # Downloadable png of the selected plot ----
    output$download_plot_png <- downloadHandler(
      filename = function() {
        "plot_GST.png"
      },
      content = function(file) {
        ggsave(filename = file, plot = plot_GST, dpi = 300)
      }
    )
  })

  # Function to handle the plot generation for FIS
  observeEvent(input$run_plot_FIS, {
    # Retrieve reactive values
    result_stats <- result_stats_reactive()
    missing_data <- missing_data_reac()

    # Data formatting
    result_stats <- result_stats %>% rownames_to_column(var = "Markers")
    fis <- as.data.frame(result_stats) %>% select("Fis (W&C)", "Markers")
    fis <- column_to_rownames(fis, var = "Markers")
    missing_data_transposed <- t(as.data.frame(missing_data))
    missing_data_transposed_total <- as.data.frame(missing_data_transposed) %>% select("Total")
    missing_data_transposed_total <- subset(missing_data_transposed_total, !rownames(missing_data_transposed_total) %in% "Mean")
    colnames(missing_data_transposed_total) <- ("Missing %")
    fis_missing_merged <- merge(fis, missing_data_transposed_total, by = "row.names", all.x = TRUE) %>% column_to_rownames("Row.names")

    # Generate the FIS plot and store it in plot_output
    plot_FIS <- ggplot(fis_missing_merged, aes(x = `Missing %`, y = `Fis (W&C)`)) +
      geom_point() +
      theme_ipsum() +
      scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
      geom_smooth(method = "lm", se = FALSE, color = "red")

    # Define the output$plot_output to render the plot
    output$plot_output <- renderPlot({
      plot_FIS
    })
    print(plot_FIS)
    # Downloadable png of the selected plot ----
    output$download_plot_png <- downloadHandler(
      filename = function() {
        "plot_FIS.png"
      },
      content = function(file) {
        ggsave(filename = file, plot = plot_FIS, dpi = 300)
      }
    )
  })

  observeEvent(input$run_panmixia, {
    # retrieve reactive values
    result_stats <- result_stats_reactive()
    filtered_data_indv <- filtered_data_reac()
    # Access the input values
    n_rep <- input$numboot
    ###########################
    level1 <- input$level1 # change the level to be reactive and put a dropdown menu to select from the filtered_data_indv
    ##########################
    # bootstrap over level1, Columns to include in the botraping
    columns_to_fstat <- colnames(filtered_data_indv[, marker_start:marker_end])

    boot_fonction <- function(data, indices, columns) {
      subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
      subset_data <- adegenet::df2genind(
        X = as.matrix(subset_data),
        sep = "/",
        ncode = 6,
        ind.names = filtered_data_indv$indv,
        pop = filtered_data_indv$Population,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
      results_mat <- fst_results %>%
        select(Fis) %>%
        as.matrix()
      return(results_mat)
    }
    print("Starting bootstrapping...")
    # Perform bootstrapping with stratification
    boot_mat_strat <- boot(
      sim = "ordinary",
      data = filtered_data_indv,
      statistic = boot_fonction,
      R = n_rep,
      strata = as.numeric(as.factor(filtered_data_indv$Population)),
      columns = columns_to_fstat,
      parallel = "multicore",
      ncpus = num_cores
    )

    # format the bootstrap results
    boot_mat_strat_CI <- tidy(boot_mat_strat, conf.int = TRUE, conf.method = "perc") # nolint: line_length_linter, object_name_linter.
    boot_mat_strat_CI <- as.data.frame(boot_mat_strat_CI)
    rownames(boot_mat_strat_CI) <- columns_to_fstat
    #  render the result
    output$basic_bbot_result <- renderTable({
      req(boot_mat_strat_CI)
      return(boot_mat_strat_CI)
    })
    print("Done.")
    print(boot_mat_strat_CI)
    # Reset rownames as a column in the data frame
    boot_mat_strat_CI$Marker <- rownames(boot_mat_strat_CI)
    # Convert Sample to a factor (assuming it contains unique values)
    boot_mat_strat_CI$Marker <- as.factor(boot_mat_strat_CI$Marker)
    output$plot_boot <- renderPlot({
      boot_mat_strat_CI %>%
        ggplot(aes(x = Marker, y = statistic)) +
        geom_point() +
        geom_errorbar(aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)), width = 0.2, position = position_dodge(0.05)) + # nolint: line_length_linter.
        xlab("Loci") +
        ylab("Fis (W&C)")
      return(boot_mat_strat_CI)
    })
  })
}
