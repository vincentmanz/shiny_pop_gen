# Load necessary libraries
library(dplyr)
library(Rcpp)
library(hierfstat)
library(parallel)
library(foreach)
library(doParallel)
library(reticulate)

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
n_simulations <- as.integer(10)
workers <- 60
# Convert R variables to Python
py$data <- data
py$loci <- loci
py$n_simulations <- n_simulations
py$workers <- workers

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

# Function to check and set up Python environment
setup_python_env <- function(env_name = "LD") {
  # Check if the environment exists
  if (!condaenv_exists(env_name)) {
    cat("Environment", env_name, "does not exist. Creating it with mamba...\n")
    
    # Create the environment using mamba
    system(paste("mamba create -n", env_name, "python=3.9 numpy pandas scipy concurrent.futures -y"))
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
from scipy.stats import chi2_contingency
from concurrent.futures import ProcessPoolExecutor

# Example Python code
print('Python environment is ready and operational.')
")

py_run_string("
import pandas as pd
import numpy as np
from concurrent.futures import ProcessPoolExecutor

# Split haplotypes into alleles
def split_alleles(column):
    alleles = column.str.split('/', expand=True)
    return pd.concat([alleles[0].astype(int), alleles[1].astype(int)], axis=1)

# Create contingency tables
def create_contingency_tables(data, loci):
    locus_pairs = [(loci[i], loci[j]) for i in range(len(loci)) for j in range(i + 1, len(loci))]
    contingency_tables = {}
    for pair in locus_pairs:
        locus1_split = split_alleles(data[pair[0]])
        locus2_split = split_alleles(data[pair[1]])
        allele_data = pd.DataFrame({
            'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
            'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
        })
        contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
        contingency_tables[f'{pair[0]}.{pair[1]}'] = contingency_table
    return contingency_tables

# Compute G-statistic
def calculate_g_stat(table):
    total = table.values.sum()
    row_sums = table.sum(axis=1).values
    col_sums = table.sum(axis=0).values
    expected = np.outer(row_sums, col_sums) / total
    observed = table.values
    non_zero = observed > 0  # Avoid log(0)
    g_stat = 2 * np.sum(observed[non_zero] * np.log(observed[non_zero] / expected[non_zero]))
    return g_stat

# Randomize haplotypes within a population
def randomize_haplotypes(data, loci):
    randomized_data = data.copy()
    for locus in loci:
        randomized_data[locus] = np.random.permutation(data[locus])
    return randomized_data

# Simulate G-statistics for a single pair
def simulate_one_pair(args):
    pop_data, pair, loci, n_simulations = args
    g_stats = []
    for _ in range(n_simulations):
        randomized_data = randomize_haplotypes(pop_data, loci)
        locus1_split = split_alleles(randomized_data[pair[0]])
        locus2_split = split_alleles(randomized_data[pair[1]])
        randomized_allele_data = pd.DataFrame({
            'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
            'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
        })
        randomized_table = pd.crosstab(randomized_allele_data['Locus1_allele'], randomized_allele_data['Locus2_allele'])
        g_stats.append(calculate_g_stat(randomized_table))
    return g_stats

# Simulate randomized G-statistics in parallel for all locus pairs for each population
def simulate_randomized_parallel(data, loci, n_simulations, n_workers):
    populations = data['Population'].unique()
    results = {}
    for pop in populations:
        pop_data = data[data['Population'] == pop].copy()
        locus_pairs = [(loci[i], loci[j]) for i in range(len(loci)) for j in range(i + 1, len(loci))]
        pop_results = {}
        
        # Run simulations in parallel for each pair
        with ProcessPoolExecutor(max_workers=n_workers) as executor:
            future_to_pair = {
                executor.submit(simulate_one_pair, (pop_data, pair, loci, n_simulations)): pair
                for pair in locus_pairs
            }
            for future in future_to_pair:
                pair = future_to_pair[future]
                pop_results[f'{pair[0]}.{pair[1]}'] = future.result()
        
        results[pop] = pop_results
    return results

# Run the simulation
data = pd.DataFrame(r.data)
loci = r.loci
n_simulations = int(r.n_simulations)
n_workers = int(r.workers)

# Store G-statistics for each population
simulated_stats = simulate_randomized_parallel(data, loci, n_simulations, n_workers)
")




# Function to flatten the nested list structure
flatten_simulated_stats <- function(simulated_stats) {
  flattened_stats <- list()
  for (pop in names(simulated_stats)) {
    pop_stats <- list()
    for (pair in names(simulated_stats[[pop]])) {
      # Flatten the list of individual simulations into a single vector
      flattened_values <- unlist(simulated_stats[[pop]][[pair]])
      pop_stats[[pair]] <- flattened_values
    }
    flattened_stats[[pop]] <- pop_stats
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




# Step 1: Generate contingency tables
contingency_tables <- create_contingency_tables(data, loci)

# Step 2: Add observed G-stats
observed_g_stats <- add_g_stats(contingency_tables)

# Step 3: Generate randomized G-stats
randomized_g_stats <- generate_randomized_g_stats_parallel(data, loci, n_simulations = 1000)
randomized_g_stats_PY <- py$simulated_stats
randomized_g_stats_PY <- flatten_simulated_stats(py$simulated_stats)
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
