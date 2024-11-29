# Load necessary libraries
library(hierfstat)
library(dplyr)

# Load the dataset
data <- read.csv("data/data-2023-09-11 (2).csv")

# Extract unique populations and loci
populations <- unique(data$Population)
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
loci_pairs <- combn(loci, 2, simplify = FALSE)

# Initialize results storage
results <- list()
global_results <- list()

# Number of simulations
n_simulations <- 10

# Split the allele pairs into two numeric columns
split_alleles <- function(column) {
  alleles <- strsplit(as.character(column), "/")
  allele1 <- as.numeric(sapply(alleles, `[`, 1))
  allele2 <- as.numeric(sapply(alleles, `[`, 2))
  return(data.frame(allele1 = allele1, allele2 = allele2))
}

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




###########################
## 1. Contingency Table ###
###########################

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
#########################################################################
#### 2. Add G-statistics obs to the nested list of contingency tables ###
#########################################################################

calculateGStatTorch <- function(contingency_table) {
  # Convert contingency table to a GPU tensor
  obs <- torch_tensor(as.matrix(contingency_table), device = "cuda")
  
  # Calculate sums
  nt <- obs$sum()
  row_sum <- obs$sum(dim = 2)
  col_sum <- obs$sum(dim = 1)
  
  # Calculate expected values
  expected <- torch_outer(row_sum, col_sum) / nt
  
  # Compute G-statistic
  g_stat <- torch_where(
    (obs > 0) & (expected > 0),
    2 * obs * (torch_log(obs / expected)),
    torch_tensor(0, device = "cuda")
  )$sum()
  
  as.numeric(g_stat)  # Return as a scalar
}



add_g_stats_to_population_tables <- function(contingency_tables_by_population) {
  g_stats_by_population <- list()
  
  for (pop in names(contingency_tables_by_population)) {
    pop_tables <- contingency_tables_by_population[[pop]]
    g_stats_for_loci <- list()
    
    for (pair in names(pop_tables)) {
      contingency_table <- pop_tables[[pair]]
      
      # Calculate G-statistic for the contingency table
      g_stat <- calculateGStatTorch(contingency_table)
      
      # Store the G-statistic
      g_stats_for_loci[[pair]] <- list(
        contingency_table = contingency_table,
        g_stat = g_stat
      )
    }
    
    # Store the G-statistics for the population
    g_stats_by_population[[pop]] <- g_stats_for_loci
  }
  
  return(g_stats_by_population)
}
############################################
### 3. Gstat simulation ####################
############################################
# Function to randomize haplotypes within a population
randomize_haplotypes_within_population <- function(pop_data, loci) {
  randomized_data <- pop_data
  for (locus in loci) {
    randomized_data[[locus]] <- sample(randomized_data[[locus]])
  }
  return(randomized_data)
}


generate_randomized_g_stats_optimized <- function(data, loci, n_simulations = 10000, n_cores = 16) {
  populations <- unique(data$Population)
  
  # Define the C++ function code
  cpp_code <- '
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
  '
  
  cl <- parallel::makeCluster(n_cores)
  
  # Export necessary objects and functions to the cluster
  parallel::clusterExport(cl, c("data", "loci", "randomize_haplotypes_within_population", 
                                "split_alleles", "cpp_code", "n_simulations"), envir = environment())
  
  # Compile the C++ function on each worker node
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
    cppFunction(cpp_code)
  })
  
  parallel::clusterEvalQ(cl, library(dplyr))
  
  randomized_g_stats <- parallel::parLapply(cl, populations, function(pop) {
    pop_data <- data %>% filter(Population == pop)
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    pop_results <- list()
    
    randomized_pop_data_list <- lapply(1:n_simulations, function(i) {
      randomize_haplotypes_within_population(pop_data, loci)
    })
    
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      pair_results <- sapply(1:n_simulations, function(i) {
        randomized_pop_data <- randomized_pop_data_list[[i]]
        locus1_split <- split_alleles(randomized_pop_data[[locus1]])
        locus2_split <- split_alleles(randomized_pop_data[[locus2]])
        allele_data <- data.frame(
          Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
          Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
        )
        contingency_table <- table(allele_data$Locus1_allele, allele_data$Locus2_allele)
        calculateGStatCpp(as.matrix(contingency_table))
      })
      pop_results[[paste(locus1, locus2, sep = "-")]] <- pair_results
    }
    return(pop_results)
  })
  
  parallel::stopCluster(cl)
  
  names(randomized_g_stats) <- populations
  return(randomized_g_stats)
}

calculate_pvalues <- function(observed_g_stats, simulated_g_stats) {
  results <- list()
  for (pop in names(observed_g_stats)) {
    observed <- observed_g_stats[[pop]]
    simulated <- simulated_g_stats[[pop]]
    pop_results <- list()
    
    for (pair in names(observed)) {
      observed_g <- observed[[pair]]$g_stat
      simulated_g <- simulated[[pair]]
      
      count_g_simule <- sum(simulated_g >= observed_g)
      total_simulations <- length(simulated_g)
      p_value <- round(count_g_simule / total_simulations, 4)
      
      pop_results[[pair]] <- list(
        observed_g_stat = round(observed_g, 4),
        count_g_simule = count_g_simule,
        total_simulations = total_simulations,
        p_value = p_value
      )
    }
    results[[pop]] <- pop_results
  }
  return(results)
}
##################################################
### 5. Gstat global ##############################
##################################################
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
##############################################################
## 6. make the table with the p-values #######################
##############################################################
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
##################################################
### Full Workflow ###############################
##################################################
# Step 1: Generate contingency tables
contingency_tables <- create_contingency_tables(data, loci)

# Step 2: Add observed G-stats
observed_g_stats <- add_g_stats_to_population_tables(contingency_tables)

# Step 3: Generate randomized G-stats
randomized_g_stats <- generate_randomized_g_stats_optimized(data, loci, n_simulations = 100)

# Step 4: Calculate p-values
pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats)

# Step 5: Calculate global p-values
global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats)

# Step 6: Create summary table
summary_table <- create_summary_table(pvalues, global_pvalues)

# View the final summary table
print(summary_table)
