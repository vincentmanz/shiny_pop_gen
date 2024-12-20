  # Load necessary libraries
  library(dplyr)
  library(Rcpp)
  library(hierfstat)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(reticulate)
  library(jsonlite)

  # Load the dataset
  data <- data.frame(
    Individual = c("Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9"),
    Population = c("Population1", "Population1", "Population1", "Population2", "Population2", "Population2", "Population3", "Population3", "Population3"),
    H1 = c("120/165", "120/165", "120/165", "0/0", "0/0", "165/170", "121/164", "171/181", "167/174"),
    H2 = c("120/165", "120/165", "120/165", "120/170", "120/165", "120/170", "122/169", "163/172", "175/186"),
    H3 = c("120/165", "120/165", "120/165", "165/170", "165/170", "120/165", "166/168", "123/173", "125/190"),
    H4 = c("120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "177/179", "129/195", "124/199")
  )
  populations <- unique(data$Population)
  loci <- c("H1", "H2", "H3", "H4")

  data <- read.csv("dummy_data.csv")
  loci <- c( "D12", "C03")


  # Function to create contingency tables for each population without splitting haplotypes
  create_contingency_tables <- function(data, loci) {
    populations <- unique(data$Population)
    
    contingency_tables <- lapply(populations, function(pop) {
      # Subset data for the population
      pop_data <- data[data$Population == pop, ]
      
      # Define locus pairs
      locus_pairs <- combn(loci, 2, simplify = FALSE)
      
      # Create named list of contingency tables
      contingency_list <- setNames(
        lapply(locus_pairs, function(pair) {
          locus1 <- pair[1]
          locus2 <- pair[2]
          
          # Remove individuals with "0/0" in either locus
          pop_data_filtered <- pop_data[pop_data[[locus1]] != "0/0" & pop_data[[locus2]] != "0/0", ]
          
          # Create haplotype data
          haplotype_data <- data.frame(
            Locus1_haplotype = unlist(pop_data_filtered[[locus1]]),
            Locus2_haplotype = unlist(pop_data_filtered[[locus2]])
          )
          
          # Create contingency table
          contingency_table <- table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
          
          return(contingency_table)
        }),
        paste0(sapply(locus_pairs, function(pair) pair[1]), "-", 
              sapply(locus_pairs, function(pair) pair[2]))
      )
      return(contingency_list)
    })
    
    names(contingency_tables) <- populations
    return(contingency_tables)
  }

  # Function to calculate G-statistic for a contingency table
  calculate_g_stat <- function(contingency_table) {
    # Total observations
    nt <- sum(contingency_table)
    
    # Row and column sums
    row_sum <- rowSums(contingency_table)
    col_sum <- colSums(contingency_table)
    
    # Expected frequencies
    expected <- outer(row_sum, col_sum) / nt
    
    # Filter out zero observed values to avoid log(0)
    non_zero <- contingency_table > 0
    
    # Observed and expected values for non-zero cells
    observed_non_zero <- contingency_table[non_zero]
    expected_non_zero <- expected[non_zero]
    
    # G-statistic calculation
    g_stat <- 2 * sum(observed_non_zero * log(observed_non_zero / expected_non_zero), na.rm = TRUE)
    
    # Return a list with expected frequencies and the G-stat
    return(list(
      expected = expected,
      g_stat = g_stat
    ))
  }

  # Add G-statistics to contingency tables  
  add_g_stats <- function(contingency_tables) {
    lapply(contingency_tables, function(pop_tables) {
      # Rename locus pairs to ensure no special characters
      sanitized_names <- make.names(names(pop_tables), unique = TRUE)
      
      setNames(
        lapply(names(pop_tables), function(pair_name) {
          contingency_table <- pop_tables[[pair_name]]
          g_stat <- calculate_g_stat(contingency_table)
          
          list(
            contingency_table = contingency_table,
            expected_contingency_table = g_stat$expected,
            g_stat = g_stat$g_stat
          )
        }),
        sanitized_names
      )
    })
  }

  # Calculate p-values for G-statistics
  calculate_pvalues <- function(observed_g_stats, simulated_g_stats, epsilon = 1e-4) {
    results <- list()
    
    for (pop in names(observed_g_stats)) {
      observed <- observed_g_stats[[pop]]
      simulated <- simulated_g_stats[[pop]]
      pop_results <- list()
      
      for (pair in names(observed)) {
        observed_g <- observed[[pair]]$g_stat
        
        # Check if simulated_g exists and is valid
        if (!is.null(simulated[[pair]]) && length(simulated[[pair]]) > 0) {
          simulated_g <- simulated[[pair]]
          
          # Remove NA values from simulated G-statistics
          simulated_g <- simulated_g[!is.na(simulated_g)]
          
          if (length(simulated_g) > 0) {
            # Calculate p-value with tolerance
            p_value <- mean(simulated_g >= (observed_g - epsilon))
          } else {
            p_value <- NaN
          }
        } else {
          # If no simulations are available, return NaN for p-value
          p_value <- NaN
        }
        
        # Store results
        pop_results[[pair]] <- list(
          observed_g_stat = observed_g,
          p_value = p_value
        )
      }
      results[[pop]] <- pop_results
    }
    return(results)
  }



  # Calculate global p-values
  calculate_global_pvalues <- function(observed_g_stats, simulated_g_stats) {
    locus_pairs <- unique(unlist(lapply(observed_g_stats, names)))
    sapply(locus_pairs, function(pair) {
      g_obs <- unlist(lapply(observed_g_stats, function(pop) {
        pop[[pair]]$g_stat
      }))
      g_sim <- unlist(lapply(simulated_g_stats, function(pop) {
        pop[[pair]]
      }))
      mean(g_sim >= mean(g_obs, na.rm = TRUE))
    })
  }

  # Create a summary table of p-values
  create_summary_table <- function(pvalues, global_pvalues) {
    all_pairs <- unique(unlist(lapply(pvalues, names)))
    summary_table <- data.frame(Locus_Pair = all_pairs)
    for (pop in names(pvalues)) {
      summary_table[[pop]] <- sapply(all_pairs, function(pair) {
        if (!is.null(pvalues[[pop]][[pair]])) {
          pvalues[[pop]][[pair]]$p_value
        } else {
          NA
        }
      })
    }
    summary_table$Global_P_Value <- sapply(all_pairs, function(pair) global_pvalues[pair])
    return(summary_table)
  }


  # Function to generate randomized G-statistics
  randomized_g_stats <- function(data, loci, n_simulations, calculate_g_stat) {
    workers <- parallel::detectCores() - 1  # Default to using available cores minus 1
    
    # Create a cluster
    cl <- makeCluster(workers)
    registerDoParallel(cl)
    
    # Export calculate_g_stat to the cluster
    clusterExport(cl, varlist = c("calculate_g_stat"), envir = environment())
    
    # Extract populations and locus pairs
    populations <- unique(data$Population)
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    
    # Perform simulations in parallel
    results <- foreach(pop = populations, .combine = 'c', .packages = c('dplyr')) %dopar% {
      pop_data <- data[data$Population == pop, ]
      pop_results <- setNames(vector("list", length(locus_pairs)), sapply(locus_pairs, function(pair) gsub("[-`]+", ".", paste(pair[1], pair[2], sep = "."))))
      
      for (pair in locus_pairs) {
        locus1 <- pair[1]
        locus2 <- pair[2]
        g_stats <- numeric(n_simulations)
        
        for (i in 1:n_simulations) {
          # Randomize haplotypes within the population
          randomized_data <- pop_data
          randomized_data[[locus1]] <- sample(pop_data[[locus1]])
          randomized_data[[locus2]] <- sample(pop_data[[locus2]])
          
          # Remove individuals with "0/0" in either locus
          filtered_data <- randomized_data[randomized_data[[locus1]] != "0/0" & randomized_data[[locus2]] != "0/0", ]
          
          if (nrow(filtered_data) == 0) {
            g_stats[i] <- NA  # Handle empty filtered data
            next
          }
          
          # Create contingency table
          haplotype_data <- data.frame(
            Locus1_haplotype = filtered_data[[locus1]],
            Locus2_haplotype = filtered_data[[locus2]]
          )
          contingency_table <- table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
          
          # Calculate G-statistic
          g_stats[i] <- calculate_g_stat(contingency_table)$g_stat
        }
        
        # Store G-statistics for the locus pair as a vector
        clean_name <- gsub("[-`]+", ".", paste(locus1, locus2, sep = "."))
        pop_results[[clean_name]] <- g_stats
      }
      list(setNames(list(pop_results), pop))
    }
    
    # Stop the cluster
    stopCluster(cl)
    
    # Combine results into a proper list
    final_results <- do.call(c, results)
    return(final_results)
  }


  data <- read.csv("data/data-2023-09-11 (2).csv")
  loci <- c("B12", "C07", "D12", "D10", "A12", "C03")

  # Extract unique populations and loci
  populations <- unique(data$Population)
  # Define loci and locus pairs
  loci_pairs <- combn(loci, 2, simplify = FALSE)
  n_simulations <- as.integer(1000)

  # Step 1: Generate contingency tables
  contingency_tables <- create_contingency_tables(data, loci)

  # Step 2: Add observed G-stats
  observed_g_stats <- add_g_stats(contingency_tables)

  # Step 3: Generate randomized G-stats
  # randomized_g_stats <- generate_randomized_g_stats_parallel(data, loci, n_simulations = 1000)
  randomized_g_stats_R <- randomized_g_stats(data, loci, n_simulations, calculate_g_stat)

  # Step 4: Calculate p-values
  pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats_R)

  # Step 5: Calculate global p-values
  global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats_R)

  # Step 6: Create summary table
  summary_table <- create_summary_table(pvalues, global_pvalues)

  # View the final summary table
  print(summary_table)

