# Load necessary libraries
library(dplyr)
library(Rcpp)
library(hierfstat)
library(parallel)
library(foreach)
library(doParallel)
library(reticulate)
library(jsonlite)

# # Load the dataset
# data <- data.frame(
#   Individual = c("Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9"),
#   Population = c("Population1", "Population1", "Population1", "Population2", "Population2", "Population2", "Population3", "Population3", "Population3"),
#   H1 = c("120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "121/164", "171/181", "167/174"),
#   H2 = c("120/165", "120/165", "120/165", "120/170", "120/165", "120/170", "122/169", "163/172", "175/186"),
#   H3 = c("120/165", "120/165", "120/165", "165/170", "165/170", "120/165", "166/168", "123/173", "125/190"),
#   H4 = c("120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "177/179", "129/195", "124/199")
# )
# populations <- unique(data$Population)
# loci <- c("H1", "H2", "H3", "H4")
data <- read.csv("data/data-2023-09-11 (2).csv")

# Extract unique populations and loci
populations <- unique(data$Population)
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")

# Define loci and locus pairs
loci_pairs <- combn(loci, 2, simplify = FALSE)
n_simulations <- as.integer(100)
workers <- 60

# Function to create contingency tables for each population without splitting haplotypes
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
        haplotype_data <- data.frame(
          Locus1_haplotype = unlist(pop_data[[locus1]]),
          Locus2_haplotype = unlist(pop_data[[locus2]])
        )
        contingency_table <- table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
        
        # Remove rows and columns with 0/0
        non_zero_rows <- rownames(contingency_table) != "0/0"
        non_zero_cols <- colnames(contingency_table) != "0/0"
        cleaned_table <- contingency_table[non_zero_rows, non_zero_cols, drop = FALSE]
        
        return(cleaned_table)
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

# Function to flatten the nested list structure and sanitize names
flatten_simulated_stats <- function(simulated_stats) {
  flattened_stats <- list()
  for (pop in names(simulated_stats)) {
    pop_stats <- list()
    for (pair in names(simulated_stats[[pop]])) {
      # Flatten the list of individual simulations into a single vector
      flattened_values <- unlist(simulated_stats[[pop]][[pair]])
      
      # Replace backticks and dashes with dots
      sanitized_pair <- gsub("[-`]", ".", pair)
      
      # Add the sanitized pair to the nested list
      pop_stats[[sanitized_pair]] <- flattened_values
    }
    # Assign sanitized names to the nested list
    flattened_stats[[pop]] <- pop_stats
  }
  
  # Reassign sanitized names to ensure no backticks at any level
  names(flattened_stats) <- gsub("[-`]", ".", names(flattened_stats))
  
  # Sanitize names in the second-level list
  for (pop in names(flattened_stats)) {
    names(flattened_stats[[pop]]) <- gsub("[-`]", ".", names(flattened_stats[[pop]]))
  }
  
  return(flattened_stats)
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

# Function to check and set up Python environment
setup_python_env <- function(env_name = "LD") {
  # Check if the environment exists
  if (!condaenv_exists(env_name)) {
    cat("Environment", env_name, "does not exist. Creating it with mamba...\n")
    
    # Create the environment using mamba
    system(paste("mamba create -n", env_name, "python=3.9 numpy pandas scipy multiprocessing -y"))
  } else {
    cat("Environment", env_name, "already exists. Using it...\n")
  }
  
  # Use the conda environment
  use_condaenv(env_name, required = TRUE)
  cat("Environment", env_name, "is set up and ready.\n")
}

# Call the function to set up and activate the LD environment
setup_python_env("LD")

# Run Python code in the LD environment
py_run_string("
import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Example Python code
print('Python environment is ready and operational.')
")
# Prepare data and variables
data_json <- jsonlite::toJSON(data, dataframe = "rows", auto_unbox = TRUE)
loci_json <- jsonlite::toJSON(loci, auto_unbox = TRUE)

# Save JSON arguments to files if needed
write(data_json, "data.json")
write(loci_json, "loci.json")

# Call Python script within LD environment using mamba
result <- system2(
  command = "mamba",
  args = c("run", "-n", "LD", "python", "LD.py", shQuote(data_json), shQuote(loci_json), n_simulations),
  stdout = TRUE,
  stderr = TRUE
)

# Step 1: Generate contingency tables
contingency_tables <- create_contingency_tables(data, loci)

# Step 2: Add observed G-stats
observed_g_stats <- add_g_stats(contingency_tables)

# Step 3: Generate randomized G-stats
# randomized_g_stats <- generate_randomized_g_stats_parallel(data, loci, n_simulations = 1000)
randomized_g_stats_PY <- randomized_g_stats <- fromJSON(result)
randomized_g_stats_PY <- flatten_simulated_stats(randomized_g_stats_PY)
# Step 4: Calculate p-values
pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats_PY)

# Step 5: Calculate global p-values
global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats_PY)

# Step 6: Create summary table
summary_table <- create_summary_table(pvalues, global_pvalues)

# View the final summary table
print(summary_table)
