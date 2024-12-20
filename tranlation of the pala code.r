# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("data/data-2023-09-11 (2).csv")

# Define loci and population columns
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
populations <- unique(data$Population)

# Function to calculate G-statistic for a contingency table
calculate_g_stat <- function(contingency_table) {
  g_stat <- sum(contingency_table[contingency_table > 0] * 
                  log(contingency_table[contingency_table > 0]))
  return(g_stat)
}

# Function to compute deltaij, rij, and accd
compute_metrics <- function(contingency_table, nbgam) {
  row_totals <- rowSums(contingency_table)
  col_totals <- colSums(contingency_table)
  
  # Avoid division by zero
  f1 <- row_totals / nbgam
  f2 <- col_totals / nbgam
  deltaij <- contingency_table * 2 / nbgam - outer(f1, f2)
  
  # Compute c1, c2, ct
  c1 <- f1 * (1 - f1) + row_totals * 2 / nbgam - f1^2
  c2 <- f2 * (1 - f2) + col_totals * 2 / nbgam - f2^2
  ct <- outer(c1, c2)
  
  # Avoid division by zero
  ct[ct == 0] <- NA
  rij <- deltaij / sqrt(ct)
  
  # Weighted sum for accd
  accd <- sum((rij^2 * outer(f1, f2)), na.rm = TRUE)
  
  return(list(deltaij = deltaij, rij = rij, accd = accd))
}

# Function to process loci pairs for a population
process_population <- function(pop_data, loci, nbgam) {
  locus_pairs <- combn(loci, 2, simplify = FALSE)
  results <- list()
  
  for (pair in locus_pairs) {
    locus1 <- pair[1]
    locus2 <- pair[2]
    
    # Filter out individuals with missing data ("0/0")
    valid_data <- pop_data %>% 
      filter(!!sym(locus1) != "0/0", !!sym(locus2) != "0/0")
    
    if (nrow(valid_data) == 0) {
      results[[paste(locus1, locus2, sep = "-")]] <- list(g_stat = NA, metrics = NA)
      next
    }
    
    # Build contingency table
    contingency_table <- table(valid_data[[locus1]], valid_data[[locus2]])
    
    # Compute G-statistic
    g_stat <- calculate_g_stat(contingency_table)
    
    # Compute deltaij, rij, accd
    metrics <- compute_metrics(contingency_table, nbgam)
    
    # Store results
    results[[paste(locus1, locus2, sep = "-")]] <- list(
      g_stat = g_stat,
      metrics = metrics
    )
  }
  return(results)
}

# Main function to process all populations
process_all_populations <- function(data, loci, populations) {
  all_results <- list()
  
  for (pop in populations) {
    pop_data <- data %>% filter(Population == pop)
    
    # Compute gamete counts
    nbgam1 <- sum(rowSums(table(pop_data[, loci])))
    nbgam2 <- sum(colSums(table(pop_data[, loci])))
    if (nbgam1 != nbgam2) {
      stop(paste("Gamete counts for population", pop, "do not match!"))
    }
    nbgam <- nbgam1
    
    # Process loci pairs for this population
    all_results[[pop]] <- process_population(pop_data, loci, nbgam)
  }
  return(all_results)
}

# Run the analysis
results <- process_all_populations(data, loci, populations)

# Reformat results for visualization
results_df <- do.call(rbind, lapply(names(results), function(pop) {
  do.call(rbind, lapply(names(results[[pop]]), function(pair) {
    data.frame(
      Population = pop,
      Locus_Pair = pair,
      G_Stat = results[[pop]][[pair]]$g_stat,
      ACCD = results[[pop]][[pair]]$metrics$accd
    )
  }))
}))

# Pivot for visualization
reformatted_results <- pivot_wider(results_df, names_from = Population, values_from = c(G_Stat, ACCD))
print(reformatted_results)


































# Load necessary libraries
library(dplyr)
library(tidyr)

# Load your dataset
data <- read.csv("data/data-2023-09-11 (2).csv")

# Define loci and population columns
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
populations <- unique(data$Population)

# Placeholder for key variables translated from Pascal
maxall <- 0
trans <- list()
nbgenot <- list()
pinpop <- list()
presgenot <- list()
nbcells <- list()

# Step 1: Determine max number of alleles across loci
maxall <- max(sapply(loci, function(locus) {
  max(unique(unlist(strsplit(as.character(data[[locus]]), "/"))))
}))

# Initialize "trans" matrix to store allele transformations
trans <- matrix(0, nrow = length(loci) + 1, ncol = maxall + 1)

# Initialize "ninpop" - a placeholder to hold locus transformations for each population
ninpop <- vector("list", length(loci) + 1)
for (il1 in seq_along(loci)) {
  ninpop[[il1]] <- rep(0, nrow(data))
}

# Step 2: Process each locus to map allele pairs to numeric indices
for (il1 in seq_along(loci)) {
  locus <- loci[il1]
  
  # Create allele mapping
  alleles <- unique(unlist(strsplit(as.character(data[[locus]]), "/")))
  allele_map <- setNames(seq_along(alleles), alleles)
  
  # Apply mapping
  ninpop[[il1]] <- sapply(strsplit(as.character(data[[locus]]), "/"), function(pair) {
    if (pair[1] != "0" && pair[2] != "0") {
      paste(min(allele_map[pair]), max(allele_map[pair]), sep = "-")
    } else {
      "0-0"
    }
  })
}

# Step 3: Compute contingency tables and other metrics for each population
process_population <- function(pop_data, loci) {
  n_loci <- length(loci)
  
  # Initialize variables to store metrics
  obsg <- matrix(0, nrow = n_loci, ncol = n_loci)
  obsgp <- array(0, dim = c(n_loci, n_loci, length(populations)))
  nbcells <- array(FALSE, dim = c(n_loci, n_loci, length(populations)))
  
  # Iterate through pairs of loci
  for (il1 in seq_len(n_loci - 1)) {
    for (il2 in seq((il1 + 1), n_loci)) {
      locus1 <- loci[il1]
      locus2 <- loci[il2]
      
      # Filter out invalid entries
      valid_data <- pop_data %>%
        filter(!!sym(locus1) != "0/0" & !!sym(locus2) != "0/0")
      
      if (nrow(valid_data) == 0) next
      
      # Build contingency table
      contingency_table <- table(valid_data[[locus1]], valid_data[[locus2]])
      
      # Calculate marginal totals
      row_totals <- rowSums(contingency_table)
      col_totals <- colSums(contingency_table)
      
      # Check if valid contingency table exists
      if (sum(row_totals) > 0 && sum(col_totals) > 0) {
        nbcells[il1, il2, ] <- TRUE
        
        # Store observed G-statistics
        g_stat <- sum(contingency_table[contingency_table > 0] * 
                        log(contingency_table[contingency_table > 0]))
        obsg[il1, il2] <- g_stat
      }
    }
  }
  
  # Return results for this population
  return(list(
    obsg = obsg,
    obsgp = obsgp,
    nbcells = nbcells
  ))
}

# Step 4: Process all populations
results <- list()
for (pop in populations) {
  pop_data <- data %>% filter(Population == pop)
  
  results[[pop]] <- process_population(pop_data, loci)
}

# Step 5: Shuffle loci within populations and compute metrics
shuffle_and_compute <- function(pop_data, loci, n_perms = 1000) {
  locus_pairs <- combn(loci, 2, simplify = FALSE)
  shuffled_metrics <- list()
  
  for (pair in locus_pairs) {
    locus1 <- pair[1]
    locus2 <- pair[2]
    
    randomized_g_stats <- numeric(n_perms)
    for (i in seq_len(n_perms)) {
      shuffled_data <- pop_data
      shuffled_data[[locus2]] <- sample(shuffled_data[[locus2]])
      
      # Compute contingency table
      contingency_table <- table(shuffled_data[[locus1]], shuffled_data[[locus2]])
      
      # Calculate G-statistic
      g_stat <- sum(contingency_table[contingency_table > 0] * 
                      log(contingency_table[contingency_table > 0]))
      randomized_g_stats[i] <- g_stat
    }
    
    shuffled_metrics[[paste(locus1, locus2, sep = "-")]] <- randomized_g_stats
  }
  
  return(shuffled_metrics)
}

# Perform shuffling and store results
shuffled_results <- list()
for (pop in populations) {
  pop_data <- data %>% filter(Population == pop)
  shuffled_results[[pop]] <- shuffle_and_compute(pop_data, loci)
}

# Ensure valid results and proper alignment of data
final_results <- lapply(names(results), function(pop) {
  # Extract observed G-statistics
  obsg <- results[[pop]]$obsg
  
  # Check if `obsg` is empty or invalid
  if (is.null(obsg) || all(is.na(obsg))) {
    return(data.frame(Population = pop, Locus_Pair = NA, Observed_G = NA, P_Value = NA))
  }
  
  # Identify valid locus pairs (non-NA values)
  valid_pairs <- which(!is.na(obsg), arr.ind = TRUE)
  if (length(valid_pairs) == 0) {
    return(data.frame(Population = pop, Locus_Pair = NA, Observed_G = NA, P_Value = NA))
  }
  
  # Generate locus pair names
  locus_pair_names <- apply(valid_pairs, 1, function(pair) paste(loci[pair[1]], loci[pair[2]], sep = "-"))
  
  # Prepare results for each locus pair
  data.frame(
    Population = pop,
    Locus_Pair = locus_pair_names,
    Observed_G = obsg[valid_pairs],
    P_Value = sapply(seq_len(nrow(valid_pairs)), function(idx) {
      # Extract observed G
      obs_g <- obsg[valid_pairs[idx, 1], valid_pairs[idx, 2]]
      
      # Extract randomized G values
      random_g <- shuffled_results[[pop]][[locus_pair_names[idx]]]
      
      # Handle missing or NULL `random_g`
      if (is.null(random_g) || length(random_g) == 0) {
        return(NA)
      }
      
      # Compute p-value
      mean(random_g >= obs_g, na.rm = TRUE)
    })
  )
})

# Combine all results into a single data frame
final_results_df <- do.call(rbind, final_results)

# Print the results
print(final_results_df)
