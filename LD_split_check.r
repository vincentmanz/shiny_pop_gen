  # Load necessary libraries
  library(dplyr)
  library(Rcpp)
  library(hierfstat)
  library(parallel)
  library(foreach)
  library(doParallel)

  # Load the dataset
  data <- data.frame(
    Individual = c("Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9"),
    Population = c("Population1", "Population1", "Population1", "Population2", "Population2", "Population2", "Population3", "Population3", "Population3"),
    H1 = c("120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "121/164", "171/181", "167/174"),
    H2 = c("120/165", "120/165", "120/165", "120/170", "120/165", "120/170", "122/169", "163/172", "175/186"),
    H3 = c("120/165", "120/165", "120/165", "165/170", "165/170", "120/165", "166/168", "123/173", "125/190"),
    H4 = c("120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "177/179", "129/195", "124/199")
  )

# Define loci and locus pairs
populations <- unique(data$Population)
loci <- c("H1", "H2", "H3", "H4")
loci_pairs <- combn(loci, 2, simplify = FALSE)

# Split the allele pairs into two numeric columns
split_alleles <- function(column) {
  alleles <- strsplit(as.character(column), "/")
  allele1 <- as.numeric(sapply(alleles, `[`, 1))
  allele2 <- as.numeric(sapply(alleles, `[`, 2))
  return(data.frame(allele1 = allele1, allele2 = allele2))
}

# Function to split alleles and generate contingency tables for each population
create_contingency_tables <- function(data, loci) {
  populations <- unique(data$Population)
  contingency_tables <- lapply(populations, function(pop) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    
    # Create named list of contingency tables
    contingency_list <- setNames(
      lapply(locus_pairs, function(pair) {
        locus1 <- pair[1]
        locus2 <- pair[2]
        locus1_split <- split_alleles(pop_data[[locus1]])
        locus2_split <- split_alleles(pop_data[[locus2]])
        allele_data <- data.frame(
          Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
          Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
        )
        table(allele_data$Locus1_allele, allele_data$Locus2_allele)
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

# Randomize haplotypes within a population
randomize_haplotypes_within_population <- function(pop_data, loci) {
  randomized_data <- pop_data
  for (locus in loci) {
    randomized_data[[locus]] <- sample(randomized_data[[locus]])
  }
  return(randomized_data)
}

# Generate randomized G-statistics
generate_randomized_g_stats_parallel <- function(data, loci, n_simulations = 1000) {
  populations <- unique(data$Population)
  # Set up parallel backend
  n_cores <- detectCores() - 1  # Leave one core free
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  # Export required variables and functions
  clusterExport(cl, varlist = c(
    "randomize_haplotypes_within_population", 
    "create_contingency_tables", 
    "split_alleles",  # Export split_alleles function
    "data", 
    "loci"
  ), envir = environment())
  # Compile the C++ function in each worker
  clusterEvalQ(cl, {
    library(Rcpp)
    cppFunction('
    double calculateGStatCpp(NumericMatrix obs) {
        double g_stat = 0.0;
        int nrow = obs.nrow(), ncol = obs.ncol();
        double nt = 0.0;
        for (int i = 0; i < nrow; ++i) {
            for (int j = 0; j < ncol; ++j) {
                nt += obs(i, j);
            }
        }
        NumericVector row_sum(nrow), col_sum(ncol);
        for (int i = 0; i < nrow; ++i) {
            for (int j = 0; j < ncol; ++j) {
                row_sum[i] += obs(i, j);
                col_sum[j] += obs(i, j);
            }
        }
        for (int i = 0; i < nrow; ++i) {
            for (int j = 0; j < ncol; ++j) {
                double expected = (row_sum[i] * col_sum[j]) / nt;
                if (obs(i, j) > 0 && expected > 0) {
                    g_stat += 2 * obs(i, j) * log(obs(i, j) / expected);
                }
            }
        }
        return g_stat;
    }
    ')
  })
  # Parallel computation
  randomized_g_stats <- foreach(pop = populations, .combine = 'c', 
                                .packages = c("dplyr")) %dopar% {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    pop_results <- lapply(locus_pairs, function(pair) {
      replicate(n_simulations, {
        randomized_pop_data <- randomize_haplotypes_within_population(pop_data, loci)
        contingency_tables <- create_contingency_tables(randomized_pop_data, loci)
        contingency_table <- contingency_tables[[pop]][[paste(pair, collapse = "-")]]
        calculateGStatCpp(as.matrix(contingency_table))
      })
    })
    # Use a period (.) instead of a hyphen (-) for naming pairs
    names(pop_results) <- paste0(sapply(locus_pairs, function(pair) pair[1]), ".", 
                                  sapply(locus_pairs, function(pair) pair[2]))
    list(pop = pop_results)
  }
  # Stop the cluster
  stopCluster(cl)
  names(randomized_g_stats) <- populations
  return(randomized_g_stats)
}



# Calculate p-values for G-statistics
calculate_pvalues <- function(observed_g_stats, simulated_g_stats) {
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
          # Calculate p-value
          p_value <- mean(simulated_g >= observed_g)
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




# Step 1: Generate contingency tables
contingency_tables <- create_contingency_tables(data, loci)

# Step 2: Add observed G-stats
observed_g_stats <- add_g_stats(contingency_tables)

# Step 3: Generate randomized G-stats
randomized_g_stats <- generate_randomized_g_stats_parallel(data, loci, n_simulations = 1000)

# Step 4: Calculate p-values
pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats)

# Step 5: Calculate global p-values
global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats)

# Step 6: Create summary table
summary_table <- create_summary_table(pvalues, global_pvalues)

# View the final summary table
print(summary_table)















# # Make controls
#   # check contengcy tables for population 3 pair 1-2  
#     # Access the contingency table
#     contingency_table <- observed_g_stats$Population3$H1.H2$contingency_table
#     # Perform the Chi-squared test to get expected frequencies
#     chisq_result <- chisq.test(contingency_table, simulate.p.value = FALSE)
#     # Extract expected frequencies
#     expected <- chisq_result$expected

#   # check gstat function tables for population 3 pair 1-2  
#     library(vcd)
#     # Access the contingency table for Population 3, pair H1-H2
#     contingency_table <- observed_g_stats$Population3$H1.H2$contingency_table
#     # Perform assocstats on the contingency table
#     assoc_stats <- assocstats(as.table(contingency_table))
#     g_stat_assocstats <- assoc_stats$chisq_tests["Likelihood Ratio", "X^2"]
#     # Extract expected frequencies from assocstats
#     chisq_result <- chisq.test(contingency_table, simulate.p.value = FALSE)
#     expected_chisq <- chisq_result$expected
#     # Compare the expected frequencies from assocstats and chisq.test
#     g_stat_observed <- observed_g_stats$Population3$H1.H2$g_stat
#     expected_observed <- observed_g_stats$Population3$H1.H2$expected_contingency_table
#     # Print results
#     cat("Observed Contingency Table:\n")
#     print(contingency_table)
#     cat("\nExpected Frequencies (from chisq.test):\n")
#     print(expected_chisq)
#     cat("\nExpected Frequencies (from observed_g_stats):\n")
#     print(expected_observed)
#     cat("\nG-Statistic (from assocstats):", g_stat_assocstats, "\n")
#     cat("G-Statistic (from observed_g_stats):", g_stat_observed, "\n")

#     # Validate consistency  
#     if (abs(g_stat_assocstats - g_stat_observed) < 1e-6) {
#     cat("\nG-Statistics are consistent!\n")
#     } else {
#     cat("\nDiscrepancy detected in G-Statistics!\n")
#     }


# # Control on global gstats: 
# # Libraries
# library(vcd)

# # Generate contingency tables, compute expected frequencies, and G-statistics
# compute_stats_for_all_pairs <- function(data, loci) {
#   locus_pairs <- combn(loci, 2, simplify = FALSE)  # Generate all locus pairs
#   results <- list()  # Store results
  
#   for (pair in locus_pairs) {
#     locus1 <- pair[1]
#     locus2 <- pair[2]
    
#     # Generate contingency table for the entire dataset
#     locus1_split <- split_alleles(data[[locus1]])
#     locus2_split <- split_alleles(data[[locus2]])
#     allele_data <- data.frame(
#       Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
#       Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
#     )
#     contingency_table <- table(allele_data$Locus1_allele, allele_data$Locus2_allele)
    
#     # Perform chi-squared test to get expected frequencies
#     chisq_result <- chisq.test(contingency_table, simulate.p.value = FALSE)
#     expected_chisq <- chisq_result$expected
    
#     # Perform assocstats to compute G-statistic
#     assoc_stats <- assocstats(as.table(contingency_table))
#     g_stat_assocstats <- assoc_stats$chisq_tests["Likelihood Ratio", "X^2"]
    
#     # Store results
#     pair_name <- paste0(locus1, ".", locus2)
#     results[[pair_name]] <- list(
#       contingency_table = contingency_table,
#       expected_frequencies = expected_chisq,
#       g_stat_assocstats = g_stat_assocstats
#     )
    
#     # Print results
#     cat("\n-----------------------------------\n")
#     cat("Pair:", pair_name, "\n")
#     cat("Observed Contingency Table:\n")
#     print(contingency_table)
#     cat("\nExpected Frequencies (from chisq.test):\n")
#     print(expected_chisq)
#     cat("\nG-Statistic (from assocstats):", g_stat_assocstats, "\n")
#     cat("-----------------------------------\n")
#   }
  
#   return(results)
# }

# # Split haplotypes into alleles
# split_alleles <- function(column) {
#   alleles <- strsplit(as.character(column), "/")
#   allele1 <- as.numeric(sapply(alleles, `[`, 1))
#   allele2 <- as.numeric(sapply(alleles, `[`, 2))
#   return(data.frame(allele1 = allele1, allele2 = allele2))
# }

# # Run the function for the dataset
# loci <- c("H1", "H2", "H3", "H4")
# results <- compute_stats_for_all_pairs(data, loci)
