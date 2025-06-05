# Load necessary libraries
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)

# Function to create contingency tables for each population
create_contingency_tables <- function(data, loci, include_missing = FALSE) {
  populations <- unique(data$Population)
  
  contingency_tables <- lapply(populations, function(pop) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    
    contingency_list <- setNames(
      lapply(locus_pairs, function(pair) {
        locus1 <- pair[1]
        locus2 <- pair[2]
        
        if (!include_missing) {
          pop_data <- pop_data[pop_data[[locus1]] != "0/0" & pop_data[[locus2]] != "0/0", ]
        }
        
        haplotype_data <- data.frame(
          Locus1_haplotype = pop_data[[locus1]],
          Locus2_haplotype = pop_data[[locus2]]
        )
        
        table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
      }),
      sapply(locus_pairs, function(pair) paste(pair[1], pair[2], sep = "-"))
    )
    return(contingency_list)
  })
  names(contingency_tables) <- populations
  return(contingency_tables)
}

# Function to calculate G-statistic
calculate_g_stat <- function(contingency_table) {
  nt <- sum(contingency_table)
  row_sum <- rowSums(contingency_table)
  col_sum <- colSums(contingency_table)
  expected <- outer(row_sum, col_sum) / nt
  non_zero <- contingency_table > 0
  observed_non_zero <- contingency_table[non_zero]
  expected_non_zero <- expected[non_zero]
  g_stat <- 2 * sum(observed_non_zero * log(observed_non_zero / expected_non_zero), na.rm = TRUE)
  return(list(expected = expected, g_stat = g_stat))
}

# Function to add G-statistics to contingency tables
add_g_stats <- function(contingency_tables) {
  lapply(contingency_tables, function(pop_tables) {
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
      gsub("[-`]+", ".", names(pop_tables))
    )
  })
}

# Function to calculate p-values
calculate_pvalues <- function(observed_g_stats, simulated_g_stats, epsilon = 1e-10) {
  results <- lapply(names(observed_g_stats), function(pop) {
    observed <- observed_g_stats[[pop]]
    simulated <- simulated_g_stats[[pop]]
    setNames(
      lapply(names(observed), function(pair) {
        observed_g <- observed[[pair]]$g_stat
        simulated_g <- simulated[[pair]][!is.na(simulated[[pair]])]
        p_value <- if (length(simulated_g) > 0) mean(simulated_g >= (observed_g - epsilon)) else NaN
        list(observed_g_stat = observed_g, p_value = p_value)
      }),
      names(observed)
    )
  })
  names(results) <- names(observed_g_stats)
  return(results)
}

# Function to calculate global p-values
calculate_global_pvalues <- function(observed_g_stats, simulated_g_stats) {
  locus_pairs <- unique(unlist(lapply(observed_g_stats, names)))
  sapply(locus_pairs, function(pair) {
    g_obs <- unlist(lapply(observed_g_stats, function(pop) pop[[pair]]$g_stat))
    g_sim <- unlist(lapply(simulated_g_stats, function(pop) pop[[pair]]))
    mean(g_sim >= mean(g_obs, na.rm = TRUE))
  })
}

# Function to create summary table
create_summary_table <- function(pvalues, global_pvalues) {
  all_pairs <- unique(unlist(lapply(pvalues, names)))
  summary_table <- data.frame(Locus_Pair = all_pairs)
  for (pop in names(pvalues)) {
    summary_table[[pop]] <- sapply(all_pairs, function(pair) pvalues[[pop]][[pair]]$p_value)
  }
  summary_table$Global_P_Value <- sapply(all_pairs, function(pair) global_pvalues[pair])
  return(summary_table)
}

# Function to generate randomized G-statistics
randomized_g_stats <- function(data, loci, n_simulations, calculate_g_stat, include_missing = FALSE) {
  workers <- parallel::detectCores() - 1
  cl <- makeCluster(workers)
  registerDoParallel(cl)
  clusterExport(cl, varlist = c("calculate_g_stat"), envir = environment())
  populations <- unique(data$Population)
  locus_pairs <- combn(loci, 2, simplify = FALSE)
  
  results <- foreach(pop = populations, .combine = 'c', .packages = 'dplyr') %dopar% {
    pop_data <- data[data$Population == pop, ]
    pop_results <- setNames(vector("list", length(locus_pairs)), sapply(locus_pairs, function(pair) gsub("[-`]+", ".", paste(pair[1], pair[2], sep = "."))))
    
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      g_stats <- numeric(n_simulations)
      for (i in 1:n_simulations) {
        randomized_data <- pop_data
        randomized_data[[locus1]] <- sample(pop_data[[locus1]])
        randomized_data[[locus2]] <- sample(pop_data[[locus2]])
        
        if (!include_missing) {
          randomized_data <- randomized_data[randomized_data[[locus1]] != "0/0" & randomized_data[[locus2]] != "0/0", ]
        }
        
        if (nrow(randomized_data) > 0) {
          contingency_table <- table(randomized_data[[locus1]], randomized_data[[locus2]])
          g_stats[i] <- calculate_g_stat(contingency_table)$g_stat
        }
      }
      pop_results[[gsub("[-`]+", ".", paste(locus1, locus2, sep = "."))]] <- g_stats
    }
    list(setNames(list(pop_results), pop))
  }
  stopCluster(cl)
  return(do.call(c, results))
}

# Load data
data <- read.csv("data/data-2023-09-11 (2).csv")
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
n_simulations <- 10000

# User option: include missing data
include_missing <- TRUE  # Set to FALSE to exclude missing data

# Generate contingency tables
contingency_tables <- create_contingency_tables(data, loci, include_missing)
observed_g_stats <- add_g_stats(contingency_tables)
randomized_g_stats_R <- randomized_g_stats(data, loci, n_simulations, calculate_g_stat, include_missing)
pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats_R)
global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats_R)
summary_table <- create_summary_table(pvalues, global_pvalues)
print(summary_table)