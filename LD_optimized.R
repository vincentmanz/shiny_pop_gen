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
n_simulations <- 100

# Split the allele pairs into two numeric columns
split_alleles <- function(column) {
  alleles <- strsplit(as.character(column), "/")
  allele1 <- as.numeric(sapply(alleles, `[`, 1))
  allele2 <- as.numeric(sapply(alleles, `[`, 2))
  return(data.frame(allele1 = allele1, allele2 = allele2))
}




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

####################################
## 2. G-statistic Calculation ######
####################################

# Function to calculate G-statistic for a contingency table
calculate_g_stat <- function(contingency_table) {
  nt <- sum(contingency_table) # Total observations
  row_sum <- rowSums(contingency_table)
  col_sum <- colSums(contingency_table)
  expected <- outer(row_sum, col_sum) / nt
  2 * sum(contingency_table * log(contingency_table / expected), na.rm = TRUE)
}

# Add G-statistics to contingency tables
add_g_stats <- function(contingency_tables) {
  lapply(contingency_tables, function(pop_tables) {
    lapply(names(pop_tables), function(pair_name) {
      contingency_table <- pop_tables[[pair_name]]
      list(
        pair = pair_name,  # Include pair name in the result
        contingency_table = contingency_table,
        g_stat = calculate_g_stat(contingency_table)
      )
    })
  })
}

############################################
### 3. G-stat Simulation ###################
############################################

# Randomize haplotypes within populations
randomize_haplotypes <- function(data, loci) {
  data[loci] <- lapply(data[loci], sample)
  return(data)
}

# Generate randomized G-statistics
generate_randomized_g_stats <- function(data, loci, n_simulations = 1000) {
  populations <- unique(data$Population)
  lapply(populations, function(pop) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    lapply(locus_pairs, function(pair) {
      replicate(n_simulations, {
        randomized_data <- randomize_haplotypes(pop_data, loci)
        locus1_split <- split_alleles(randomized_data[[pair[1]]])
        locus2_split <- split_alleles(randomized_data[[pair[2]]])
        allele_data <- data.frame(
          Locus1_allele = c(locus1_split$allele1, locus1_split$allele2),
          Locus2_allele = c(locus2_split$allele1, locus2_split$allele2)
        )
        calculate_g_stat(table(allele_data$Locus1_allele, allele_data$Locus2_allele))
      })
    })
  })
}

####################################################
### 4. Calculate Counts and P-Values ##############
####################################################

# Calculate observed vs. simulated counts and p-values
calculate_pvalues <- function(observed_g_stats, simulated_g_stats) {
  lapply(names(observed_g_stats), function(pop) {
    observed <- observed_g_stats[[pop]]
    simulated <- simulated_g_stats[[pop]]
    lapply(names(observed), function(pair) {
      observed_g <- observed[[pair]]$g_stat
      simulated_g <- simulated[[pair]]
      p_value <- mean(simulated_g >= observed_g)
      list(
        observed_g_stat = observed_g,
        p_value = p_value
      )
    })
  })
}

##################################################
### 5. Global G-Stat P-Values ####################
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

##################################################
### 6. Create Summary Table ######################
##################################################

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
observed_g_stats <- add_g_stats(contingency_tables)

# Step 3: Generate randomized G-stats
randomized_g_stats <- generate_randomized_g_stats(data, loci, n_simulations = 10)

# Step 4: Calculate p-values
pvalues <- calculate_pvalues(observed_g_stats, randomized_g_stats)

# Step 5: Calculate global p-values
global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_g_stats)

# Step 6: Create summary table
summary_table <- create_summary_table(pvalues, global_pvalues)

# View the final summary table
print(summary_table)
