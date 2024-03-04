# server_general_stats.R 
################################################################################
# environment variables

filtered_data <- read.csv("/media/vincent/vincent/dev/parasiteR/data/data-2023-09-11 (2).csv", header = TRUE)

# information inherited from previous page 
n_marker = 6
n_pop = 8
sequence_length <- length(6:11) 
n_indv = nrow(filtered_data)
pops <- unique(filtered_data$Population)
## Specify the number of cores available
num_cores <- parallel::detectCores()
## Code for missing data
missing_data_code = "0/0"
## genotype columns
column_genotype_start=6
column_genotype_end=11

# Specify the number of bootstrap replicates
R <- 1000
# Specify the number of replicates (HW-Panmixia)
n_rep = 1000

# selected_stats <-
#   c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)", "GST")

################################################################################

# Define the server logic
general_stats_server <- function(input, output, session) {
  
  # Create a reactive value to store the output of the general stats 
  result_stats_reactive <- reactiveVal(NULL)
  # Missing data reactive value
  missing_data_reac <- reactiveVal(NULL)
  # Genind data reactive value
  mydata_genind_reac <- reactiveVal(NULL)
  
  # Function to run the basic.stats and render the result
  observeEvent(input$run_basic_stats, {
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
      "GST''"= input$GST_sec_checkbox
    )
    if (length(selected_stats) > 0) {
      ## Create genind object
      filtered_data <- data.frame(indv = paste(substr(filtered_data$Population, 1, 3),row.names(filtered_data),sep = "."), filtered_data)
      population <- filtered_data$Population
      mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end],sep = "/",ncode = 6,ind.names = filtered_data$indv,pop = filtered_data$Population, NA.char = "NA", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL)
      mydata_hierfstat <- genind2hierfstat(mydata_genind)
      mydata_genind_reac(mydata_genind)
      
      ## Nei diversity 
      result <- basic.stats(mydata_hierfstat)  # hierfstat
      df_resutl_basic <- as.data.frame(result$perloc)
      
      ## Weir and Cockrham estimates of F-statistics - FIS and FST
      data <- as.data.frame(as.loci(mydata_genind))
      result_f_stats <- Fst(as.loci(data))  # pegas
      
      ## compute the Gstats
      df_resutl_basic <-  df_resutl_basic %>% mutate("GST" = 1-Hs/Ht)
      df_resutl_basic <-  df_resutl_basic %>% mutate("GST''" = (n_pop*(Ht-Hs))/((n_pop*Hs-Ht)*(1-Hs)))
      
      ## formatting the output table
      colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
      result_stats <- merge(result_f_stats, df_resutl_basic, by = "row.names", all.x = TRUE)
      colnames(result_stats)[11] <- "Fst (Nei)"
      colnames(result_stats)[13] <- "Fis (Nei)"
      result_stats <- result_stats %>% column_to_rownames(., var = 'Row.names') %>% rownames_to_column(var = "Markers")
      result_stats <- result_stats %>% column_to_rownames(var = "Markers") 
      result_stats_reactive(result_stats)
      
      #### Convert logical vector to character vector of column names
      selected_columns <- c("Markers", names(selected_stats)[selected_stats])
      result_stats_selec <- result_stats %>% select(all_of(selected_columns))
      
      #  render the result
      output$basic_stats_result <- renderTable({req(result_stats_selec)
        return(result_stats_selec) 
      })
    }
  })
  
  # Function to handle the plot generation
  observeEvent(input$run_plot_heatmap, {
    
    # Data shaping
    filtered_data <- read.csv("/media/vincent/vincent/dev/parasiteR/data/data-2023-09-11 (2).csv", header = TRUE)
    filtered_data <- data.frame(indv = paste(substr(filtered_data$Population, 1, 3),row.names(filtered_data),sep = "."), filtered_data)
    population <- filtered_data$Population
    mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end],sep = "/",ncode = 6,ind.names = filtered_data$indv,pop = filtered_data$Population, NA.char = "0/0", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL)
    missing_data <- info_table(mydata_genind, plot = FALSE, percent = TRUE, df = TRUE)
    missing_data <- as.data.frame(missing_data) %>% spread(key = Locus, value = Missing  ) 
    missing_data <- missing_data %>% column_to_rownames(var = "Population") ### !! alphabetically ordered index
    missing_data <- missing_data * 100
    missing_data_reac(missing_data)
    ### heatmap
    # plot_missing_data <- heatmaply((missing_data), 
    #                                dendrogram = "none",
    #                                xlab = "", ylab = "", 
    #                                main = "Heatmap of the missing data in the dataset.",
    #                                scale = "none",
    #                                margins = c(60,100,40,20),
    #                                grid_color = "white",
    #                                grid_width = 0.00001,
    #                                titleX = FALSE,
    #                                hide_colorbar = FALSE,
    #                                branches_lwd = 0.1,
    #                                label_names = c("Population", "Marker", "Percentage of missing data"),
    #                                fontsize_row = 8, fontsize_col = 8,
    #                                labCol = colnames(missing_data),
    #                                labRow = rownames(missing_data),
    #                                heatmap_layers = theme(axis.line=element_blank()),
    #                                colorbar_lab = "Percentages")
    # heatmaply does not print on the shiny app find a new solution 
    
    missing_data <- rownames_to_column(missing_data, var = "location")
    ## Melt the data for ggplot2
    heatmap_data_melted <- reshape2::melt(missing_data,value.name = "location" )
    colnames(heatmap_data_melted) <- c("location", "marker", "percent")
    # Generate the heatmap plot
    output$plot_output <- renderPlot({
      ggplot(data = heatmap_data_melted, aes(x = location, y = marker, fill = percent)) + 
        geom_tile() +
        labs(x = "Column Names", y = "Row Names", fill = "Value") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility
    })
  })
  # Function to handle the plot generation for GST
  observeEvent(input$run_plot_GST, {
    # Retrieve result_stats from the reactive value
    result_stats <- result_stats_reactive() 
    # Generate the GST plot
    output$plot_output <- renderPlot({
      ## Plot GST with linear trend
      ggplot(data = result_stats, aes(x = GST, y = Hs)) + 
        geom_point() +
        geom_smooth(method = lm , color="red", se=FALSE) +
        theme_ipsum()
    })
  })
  
  # Function to handle the plot generation for FIS
  observeEvent(input$run_plot_FIS, {
    # Retrieve result_stats from the reactive value
    result_stats <- result_stats_reactive() 
    missing_data <- missing_data_reac()
    # Data formatting
    fis <- as.data.frame(result_stats) %>% select('Fis (W&C)', "Markers")
    fis <- column_to_rownames(fis, var = "Markers")

    missing_data_transposed <- t(missing_data)
    missing_data_transposed_total <- as.data.frame(missing_data_transposed) %>% select("Total") 
    missing_data_transposed_total <- subset(missing_data_transposed_total, !rownames(missing_data_transposed_total) %in% "Mean")
    colnames(missing_data_transposed_total) <- ('Missing %')
    fis_missing_merged <- merge(fis, missing_data_transposed_total, by="row.names",all.x=TRUE) %>% column_to_rownames('Row.names')

    # Generate the FIS plot
    output$plot_output <- renderPlot({
      ## Plot missing data / FIS
      ggplot(fis_missing_merged, aes(x=`Missing %`, y=`Fis (W&C)`)) +
        geom_point() +
        theme_ipsum() +
        scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
        geom_smooth(method = "lm", se = FALSE, color="red") 
    })
  })
  
  observeEvent(input$run_panmixia, {
    # retrieve mydata_genind from the reactive value
    mydata_genind <- mydata_genind_reac()
    result_stats <- result_stats_reactive()
    
    # Access the input values
    n_rep <- input$numboot
    level1 <- input$level1
    
    ### bootstrap over population

    # Columns to include in the mean calculation
    columns_to_fstat <- colnames(filtered_data[,6:11])
    pop_levels <- levels((filtered_data$Population)) 
    # Convert to numeric and remove rows with missing values
    
    #################### need to find a proper to do it. How to mange the missing data ####################
    filtered_data_na <- na.omit(filtered_data) 
    ####################################################################################################### 
    
    boot_fonction <- function(data, indices, columns) {
      subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
      subset_data <- adegenet::df2genind(
        X = as.matrix(subset_data),
        sep = "/",
        ncode = 6,
        ind.names = filtered_data_na$indv,
        pop = filtered_data_na$Population,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
      results_mat <- fst_results %>% select(Fis) %>% as.matrix()
      return(results_mat)
    }
    
    # Columns to include in the mean calculation
    columns_to_fstat <- colnames(filtered_data[,6:11])
    pop_levels <- levels((filtered_data$level1)) 
    # Convert to numeric and remove rows with missing values
    
    #################### need to find a proper to do it. How to manage the missing data ####################
    filtered_data_na <- na.omit(filtered_data) 
    ####################################################################################################### 
    
    boot_fonction <- function(data, indices, columns) {
      subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
      subset_data <- adegenet::df2genind(
        X = as.matrix(subset_data),
        sep = "/",
        ncode = 6,
        ind.names = filtered_data_na$indv,
        pop = filtered_data_na$level1,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
      results_mat <- fst_results %>% select(Fis) %>% as.matrix()
      return(results_mat)
    }

    # Perform bootstrapping with stratification
    boot_mat_strat <- boot(
      data = filtered_data_na,
      statistic = boot_fonction,
      R = n_rep,
      strata = as.factor(filtered_data_na$level1),
      columns = columns_to_fstat,
      parallel = "multicore",
      ncpus = num_cores
    )
    
    # Display the CI
    boot_mat_strat_CI <- tidy(boot_mat_strat, conf.int=TRUE, conf.method="perc")
    boot_mat_strat_CI <- as.data.frame(boot_mat_strat_CI)
    rownames(boot_mat_strat_CI) <- columns_to_fstat
    print(boot_mat_strat_CI)
    #  render the result
    output$basic_bbot_result <- renderTable({req(boot_mat_strat_CI)
      return(boot_mat_strat_CI) 
    })
    
    # Reset rownames as a column in the data frame
    boot_mat_strat_CI$Marker <- rownames(boot_mat_strat_CI)
    # Convert Sample to a factor (assuming it contains unique values)
    boot_mat_strat_CI$Marker = as.factor(boot_mat_strat_CI$Marker)
    output$plot_boot <- renderPlot({
      boot_mat_strat_CI %>% 
        ggplot(aes(x = Marker, y = statistic)) +
        geom_point() +
        geom_errorbar(aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)), width = 0.2, position = position_dodge(0.05)) +
        xlab("Loci") +
        ylab("Fis (W&C)")
    })
  })
  
}

