#### benchmark ###


################################
### Dummy data #################
################################
set.seed(123) # For reproducibility

# Parameters
populations <- c("Pop1", "Pop2", "Pop3", "Pop4", "Pop5")
n_individuals <- 10 # Number of individuals per population
loci <- c("Locus1", "Locus2", "Locus3", "Locus4", "Locus5")
allele_start <- 100 # Start of allele range
allele_step <- 50   # Step to ensure no overlap between populations

generate_unique_haplotypes <- function(pop_index, n_individuals, loci, allele_start, allele_step) {
  allele_pool <- seq(allele_start + (pop_index - 1) * allele_step, 
                     allele_start + pop_index * allele_step - 1)
  data.frame(
    Population = paste0("Pop", pop_index),
    Individual = paste0("Pop", pop_index, "_Ind", 1:n_individuals),
    sapply(loci, function(locus) {
      replicate(n_individuals, paste0(sample(allele_pool, 1), "/", sample(allele_pool, 1)))
    })
  )
}


# Generate dataset
dummy_data <- do.call(rbind, lapply(seq_along(populations), function(i) {
  generate_unique_haplotypes(i, n_individuals, loci, allele_start, allele_step)
}))

# Rename columns
colnames(dummy_data) <- c("Population", "Individual", loci)

# View the dummy dataset
# print(dummy_data)


################################
### sequencial #################
################################
# Function to randomize haplotypes within a population
randomize_haplotypes_within_population <- function(pop_data, loci) {
  randomized_data <- pop_data
  for (locus in loci) {
    # Shuffle the haplotypes within the population
    randomized_data[[locus]] <- sample(randomized_data[[locus]])
  }
  return(randomized_data)
}

# Function to generate contingency tables and calculate G-statistics with population-specific randomisation
generate_randomized_g_stats <- function(data, loci, n_simulations = 10000) {
  populations <- unique(data$Population)
  randomized_g_stats <- list()
  
  for (pop in populations) {
    # Filter data for the population
    pop_data <- data %>% filter(Population == pop)
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    pop_results <- list()
    
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      pair_results <- list()
      
      for (i in 1:n_simulations) {
        # Randomize haplotypes within the population
        randomized_pop_data <- randomize_haplotypes_within_population(pop_data, loci)
        
        # Split alleles for the randomized data
        locus1_split <- split_alleles(randomized_pop_data[[locus1]])
        locus2_split <- split_alleles(randomized_pop_data[[locus2]])
        
        # Combine alleles into a long format
        allele_data <- data.frame(
          Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
          Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
        )
        
        # Create contingency table
        contingency_table <- table(allele_data$Locus1_allele, allele_data$Locus2_allele)
        
        # Calculate G-statistic
        g_stat <- calculate_g_stat(contingency_table)
        
        # Store the G-statistic for this randomization
        pair_results[[i]] <- g_stat
      }
      
      # Store results for the pair
      pop_results[[paste(locus1, locus2, sep = "-")]] <- unlist(pair_results)
    }
    
    # Store results for the population
    randomized_g_stats[[pop]] <- pop_results
  }
  
  return(randomized_g_stats)
}


# Run randomization and summarization
n_simulations <- 10000

################################
### parallel #################
################################
library(parallel)
# Function to randomize haplotypes within a population
randomize_haplotypes_within_population <- function(pop_data, loci) {
  randomized_data <- pop_data
  for (locus in loci) {
    randomized_data[[locus]] <- sample(randomized_data[[locus]])
  }
  return(randomized_data)
}


# Optimized randomization function
generate_randomized_g_stats_prarallel <- function(data, loci, n_simulations = 10000, n_cores = 16) {
  populations <- unique(data$Population)
  
  # Parallelize across populations
  cl <- parallel::makeCluster(n_cores)
  clusterExport(cl, c("data", "loci", "randomize_haplotypes_within_population", "split_alleles", "calculate_g_stat", "n_simulations"))
  clusterEvalQ(cl, library(dplyr))
  
  randomized_g_stats <- parLapply(cl, populations, function(pop) {
    # Filter data for the current population
    pop_data <- data %>% filter(Population == pop)
    
    # Precompute locus pairs
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    pop_results <- list()
    
    # Generate randomized G-stats for all simulations in bulk
    randomized_pop_data_list <- lapply(1:n_simulations, function(i) {
      randomize_haplotypes_within_population(pop_data, loci)
    })
    
    # Iterate over locus pairs
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      
      # Precompute alleles for the original population
      original_locus1_split <- split_alleles(pop_data[[locus1]])
      original_locus2_split <- split_alleles(pop_data[[locus2]])
      
      # Calculate G-statistics for all simulations
      pair_results <- sapply(1:n_simulations, function(i) {
        randomized_pop_data <- randomized_pop_data_list[[i]]
        locus1_split <- split_alleles(randomized_pop_data[[locus1]])
        locus2_split <- split_alleles(randomized_pop_data[[locus2]])
        
        allele_data <- data.frame(
          Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
          Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
        )
        contingency_table <- table(allele_data$Locus1_allele, allele_data$Locus2_allele)
        calculate_g_stat(contingency_table)
      })
      
      # Store results for the locus pair
      pop_results[[paste(locus1, locus2, sep = "-")]] <- pair_results
    }
    
    return(pop_results)
  })
  
  stopCluster(cl)
  
  # Combine results for all populations
  names(randomized_g_stats) <- populations
  return(randomized_g_stats)
}

################################
### C++        #################
################################
library(Rcpp)


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
library(microbenchmark)
# Define both functions (previous and optimized)
linear_function <- function() {
  generate_randomized_g_stats(data = dummy_data, loci = loci, n_simulations = 10000)
}
# 
# parallel_function <- function() {
#   generate_randomized_g_stats_prarallel(data = dummy_data, loci = loci, n_simulations = 10000, n_cores = 16)
# }

cplus_function <- function(){
  generate_randomized_g_stats_optimized(data = dummy_data, loci = loci, n_simulations = 10000)
}

# Run and compare performance
benchmark_results <- microbenchmark(
  "C++" = cplus_function(),
   # Parallel = parallel_function(),
   Linear = linear_function(),
  times = 5 # Number of repetitions for timing
)

# Summarize results
summary_table <- data.frame(
  Method = c("Previous", "Optimized"),
  Mean_Time = summary(benchmark_results)$mean / 1e6, # Convert to milliseconds
  Median_Time = summary(benchmark_results)$median / 1e6, # Convert to milliseconds
  Min_Time = summary(benchmark_results)$min / 1e6, # Convert to milliseconds
  Max_Time = summary(benchmark_results)$max / 1e6 # Convert to milliseconds
)

# Print the final table
print(summary_table)


