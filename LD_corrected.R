# Load necessary libraries
library(dplyr)
library(Rcpp)
library(hierfstat)
library(parallel)
library(foreach)
library(doParallel)


###################################################################
# for LD.R 1,000,000 simulation 287m55.012s= 4.8hours
#
###################################################################

# # Load the dataset
# data <- read.csv("data/data-2023-09-11 (2).csv")

# # Extract unique populations and loci
# populations <- unique(data$Population)
# loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
# loci_pairs <- combn(loci, 2, simplify = FALSE)



# Load the dataset
data <- data.frame(
  Individual = c("Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9"),
  Population = c("Population1", "Population1", "Population1",
                 "Population2", "Population2", "Population2",
                 "Population3", "Population3", "Population3"),
  H1 = c("120/165", "120/165", "120/165",
         "120/165", "120/170", "165/170",
         "120/165", "170/180", "165/170"),
  H2 = c("120/165", "120/165", "120/165",
         "120/170", "120/165", "120/170",
         "120/170", "165/170", "170/180"),
  H3 = c("120/165", "120/165", "120/165",
         "165/170", "165/170", "120/165",
         "165/170", "120/170", "120/165"),
  H4 = c("120/165", "120/165", "120/165",
         "120/165", "120/170", "165/170",
         "170/180", "120/165", "120/170")
)

# Define loci and locus pairs
populations <- unique(data$Population)
loci <- c("H1", "H2", "H3", "H4")
loci_pairs <- combn(loci, 2, simplify = FALSE)

# data(gtrunchier)
# attach(gtrunchier)
# g.stats(data.frame(Patch,L21.V))

# data <- gtrunchier
# loci <- c("L16.J", "L20.B", "L21.V", "L29.V", "L36.B", "L37.J")
# loci_pairs <- combn(loci, 2, simplify = FALSE)
# populations <- unique(data$Locality)


# C++ function for calculating G-statistics
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

# Function to create contingency tables without splitting haplotypes
create_contingency_matrix_haplotypes <- function(pop_data, locus1, locus2) {
  # Observed contingency table
  haplotype_data <- data.frame(
    Locus1_haplotype = pop_data[[locus1]],
    Locus2_haplotype = pop_data[[locus2]]
  )
  observed_table <- table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
  
  # Calculate row and column sums
  row_totals <- apply(observed_table, 1, sum)
  col_totals <- apply(observed_table, 2, sum)
  total <- sum(observed_table)
  
  # Expected table
  expected_table <- outer(row_totals, col_totals) / total
  
  return(list(observed = observed_table, expected = expected_table))
}

# Generate contingency tables for all populations
create_contingency_tables <- function(data, loci) {
  populations <- unique(data$Population)
  contingency_tables <- lapply(populations, function(pop) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    
    # Create named list of contingency tables (observed and expected)
    contingency_list <- setNames(
      lapply(locus_pairs, function(pair) {
        locus1 <- pair[1]
        locus2 <- pair[2]
        create_contingency_matrix_haplotypes(pop_data, locus1, locus2)
      }),
      paste0(sapply(locus_pairs, function(pair) pair[1]), "-", 
             sapply(locus_pairs, function(pair) pair[2]))
    )
    return(contingency_list)
  })
  names(contingency_tables) <- populations
  return(contingency_tables)
}
# Add G-statistics to contingency tables
add_g_stats_to_population_tables <- function(contingency_tables_by_population) {
  g_stats_by_population <- list()
  
  for (pop in names(contingency_tables_by_population)) {
    pop_tables <- contingency_tables_by_population[[pop]]
    g_stats_for_loci <- list()
    
    for (pair in names(pop_tables)) {
      tables <- pop_tables[[pair]]
      observed <- as.matrix(tables$observed)
      expected <- as.matrix(tables$expected)
      
      # Calculate G-statistic
      g_stat <- 2 * sum(observed * log(observed / expected), na.rm = TRUE)
      
      # Store results
      g_stats_for_loci[[pair]] <- list(
        contingency_table = tables$observed,
        expected_table = tables$expected,
        g_stat = g_stat
      )
    }
    
    g_stats_by_population[[pop]] <- g_stats_for_loci
  }
  
  return(g_stats_by_population)
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
generate_randomized_g_stats_parallel <- function(data, loci, n_simulations = 1000000) {
  populations <- unique(data$Population)
  
  # Set up parallel backend
  n_cores <- detectCores() - 1  # Leave one core free
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Export required variables and functions
  clusterExport(cl, varlist = c("randomize_haplotypes_within_population", 
                                "create_contingency_matrix_haplotypes", 
                                "data", "loci"), envir = environment())
  
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
      locus1 <- pair[1]
      locus2 <- pair[2]
      replicate(n_simulations, {
        randomized_pop_data <- randomize_haplotypes_within_population(pop_data, loci)
        contingency_table <- create_contingency_matrix_haplotypes(randomized_pop_data, locus1, locus2)
        calculateGStatCpp(as.matrix(contingency_table))
      })
    })
    names(pop_results) <- paste0(sapply(locus_pairs, function(pair) pair[1]), "-", 
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
      simulated_g <- simulated[[pair]]
      
      p_value <- mean(simulated_g >= observed_g)
      
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

# Execute full workflow
# Step 1: Generate contingency tables
contingency_tables <- create_contingency_matrix_haplotypes(data, loci)

# Step 2: Add observed G-stats
observed_g_stats <- add_g_stats_to_population_tables(contingency_tables)

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
