# server_general_stats.R 
################################################################################
# environment variables

filtered_data <- read.csv("/media/vincentmanzanilla/vincent/dev/parasiteR/data/data-2023-09-11 (2).csv", header = TRUE)

# Specify the number of bootstrap replicates
R <- 1000
# Specify the number of replicates (HW-Panmixia)
n_rep = 1000


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


selected_stats <-
  c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)", "GST")
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
    filtered_data <- read.csv("/media/vincentmanzanilla/vincent/dev/parasiteR/data/data-2023-09-11 (2).csv", header = TRUE)
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
    
    library(ggplot2)
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
    print("a")
    print(result_stats)
    # Data formatting
    fis <- as.data.frame(result_stats) %>% select('Fis (W&C)', "Markers")
    fis <- column_to_rownames(fis, var = "Markers")
    print("b")
    
    missing_data_transposed <- t(missing_data)
    missing_data_transposed_total <- as.data.frame(missing_data_transposed) %>% select("Total") 
    missing_data_transposed_total <- subset(missing_data_transposed_total, !rownames(missing_data_transposed_total) %in% "Mean")
    colnames(missing_data_transposed_total) <- ('Missing %')
    fis_missing_merged <- merge(fis, missing_data_transposed_total, by="row.names",all.x=TRUE) %>% column_to_rownames('Row.names')
    print("c")
    print(fis_missing_merged)
    
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
    
  observeEvent(input$panmixia, {
    # retrieve mydata_genind from the reactive value
    mydata_genind <- mydata_genind_reac()
    result_stats <- result_stats_reactive()
    print(result_stats)
    
    # shuffle genotypes
    es <- replicate(n_rep, (shufflepop(mydata_genind, method=1)))
    fis_df <- numeric(sequence_length)
    
    cl <- parallel::makeCluster(num_cores)
    registerDoParallel(cl)
    # Create a list to store the results
    fis_list <- foreach(i = 1:n_rep, .combine = 'c') %dopar% {
      library(dplyr)
      # Calculate the statistics for the i-th matrix
      result_fis_stats <- as.data.frame(pegas::Fst(pegas::as.loci(es[[i]]))) 
      result_fis_stats <- result_fis_stats %>%
        tibble::rownames_to_column(var = "Marker") %>%
        arrange(Marker) %>% select(Fis)
      return(result_fis_stats)
    }
    
    # Combine the results into a matrix
    fis_df <-as.data.frame(fis_list)
    # Stop the parallel processing cluster
    stopCluster(cl)
    
    # Set row names as in result_f_stats
    rownames(fis_df) <- rownames(result_f_stats)
    #fis_df <-fis_df[, -1]
    vec <- seq(1, n_rep)
    colnames(fis_df) <- vec
    
    # Initialize an empty data frame to store the counts
    fis_df_Greater <- numeric(sequence_length)
    fis_df_Smaller <- numeric(sequence_length)
    
    # Compare the values in result_f_stats[1] to result_FST for each column
    for (col in colnames(fis_df)) {
      greater_count <- as_tibble(result_f_stats[3] > fis_df[,col])
      smaller_count <- as_tibble(result_f_stats[3] < fis_df[,col])
      fis_df_Greater <- cbind(fis_df_Greater, greater_count$`Fis (W&C)`)
      fis_df_Smaller <- cbind(fis_df_Smaller, smaller_count$`Fis (W&C)`)
    }
    
    # Set row names as in result_f_stats
    rownames(fis_df_Smaller) <- rownames(fis_df_Greater) <- rownames(result_f_stats)
    fis_df_Smaller <-fis_df_Smaller[, -1]
    fis_df_Greater <-fis_df_Greater[, -1]
    vec <- seq(1, n_rep)
    colnames(fis_df_Smaller) <- colnames(fis_df_Greater) <- vec
    
    fis_df_Smaller_av <- as.data.frame(rowMeans(fis_df_Smaller))
    fis_df_Greater_av <-  as.data.frame(rowMeans(fis_df_Greater))
    
    
    fis_df_sg <- merge(fis_df_Smaller_av, fis_df_Greater_av, by="row.names",all.x=TRUE) %>% column_to_rownames('Row.names')
    
    colnames(fis_df_sg) <- c("smaller", "greater")
    
    # Assuming 'smaller' and 'greater' are columns in your 'fis_df_sg' data frame
    fis_df_sg_t <- fis_df_sg %>% mutate(`Two-sided p-values` = smaller + (1 - greater))
    results_merged <- merge(result_f_stats_selec, fis_df_sg_t, by="row.names", all.x=TRUE) 
    rownames(results_merged) <- results_merged$Row.names
    # Return the two sided values tab
    print(results_merged)
  })
  
  
  
  
  
  
  
  
  ### bootstrap over population
  
  library(boot)
  library(dplyr)
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
  
  
  library(boot)
  library(dplyr)
  # Columns to include in the mean calculation
  columns_to_fstat <- colnames(filtered_data[,6:11])
  pop_levels <- levels((filtered_data$Population)) 
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
  
  
  # Perform bootstrapping with stratification
  boot_mat_strat <- boot(
    data = filtered_data_na,
    statistic = boot_fonction,
    R = n_rep,
    strata = as.factor(filtered_data_na$Population),
    columns = columns_to_fstat,
    parallel = "multicore",
    ncpus = num_cores
  )
  
  # Display the CI
  library(broom)
  boot_mat_strat_CI <- tidy(boot_mat_strat, conf.int=TRUE, conf.method="perc")
  boot_mat_strat_CI <- as.data.frame(boot_mat_strat_CI)
  rownames(boot_mat_strat_CI) <- columns_to_fstat
  boot_mat_strat_CI
  
}

