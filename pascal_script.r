# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("data/data-2023-09-11 (2).csv")

# Define loci and population columns
loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
populations <- unique(data$Population)

# Function to compute G-statistic for a contingency table
calculate_g_stat <- function(table) {
  observed <- table
  total <- sum(observed)
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  expected <- outer(row_sums, col_sums) / total
  
  # Avoid division by zero
  expected[expected == 0] <- 1
  
  # Compute G-statistic
  g_stat <- 2 * sum(observed[observed > 0] * log(observed[observed > 0] / expected[observed > 0]))
  return(g_stat)
}

# Function to shuffle loci and compute randomized G-statistics
randomize_and_compute <- function(data, locus1, locus2, n_perms = 1000) {
  randomized_g_stats <- numeric(n_perms)
  for (i in 1:n_perms) {
    shuffled_data <- data
    shuffled_data[[locus2]] <- sample(shuffled_data[[locus2]])
    contingency_table <- table(shuffled_data[[locus1]], shuffled_data[[locus2]])
    randomized_g_stats[i] <- calculate_g_stat(contingency_table)
  }
  return(randomized_g_stats)
}

# Function to compute p-values
compute_p_values <- function(data, loci, populations, n_perms = 1000) {
  p_values <- list()
  
  for (pop in populations) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    p_values[[pop]] <- list()
    
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      
      # Remove individuals with missing data ("0/0")
      valid_data <- pop_data[pop_data[[locus1]] != "0/0" & pop_data[[locus2]] != "0/0", ]
      if (nrow(valid_data) == 0) {
        p_values[[pop]][[paste(locus1, locus2, sep = "-")]] <- NA
        next
      }
      
      # Compute observed G-statistic
      contingency_table <- table(valid_data[[locus1]], valid_data[[locus2]])
      observed_g_stat <- calculate_g_stat(contingency_table)
      
      # Compute randomized G-statistics
      randomized_g_stats <- randomize_and_compute(valid_data, locus1, locus2, n_perms)
      
      # Calculate p-value
      p_value <- mean(randomized_g_stats >= observed_g_stat)
      
      # Store results
      p_values[[pop]][[paste(locus1, locus2, sep = "-")]] <- p_value
    }
  }
  return(p_values)
}

# Run the p-value computation
n_permutations <- 1000
p_values <- compute_p_values(data, loci, populations, n_perms = n_permutations)

# Save p-values to a CSV file
p_values_df <- do.call(rbind, lapply(names(p_values), function(pop) {
  do.call(rbind, lapply(names(p_values[[pop]]), function(pair) {
    data.frame(Population = pop, LocusPair = pair, PValue = p_values[[pop]][[pair]])
  }))
}))
write.csv(p_values_df, "p_values.csv", row.names = FALSE)

# Display the computed p-values
print(p_values_df)
