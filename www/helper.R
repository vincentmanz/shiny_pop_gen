# helper.R

# Define a function for concatenate the alleles in columns (server_importdata)
concat_identical_cols <- function(df, ploidy) {
  col_names <- colnames(df)
  col_names <-
    sub("\\.\\d+$", "", col_names)  # Remove .1 or .2 from column names
  
  # Initialize result with one row from temp_df
  col1 <- col_names[1]
  temp_df <- data.frame(col1 = df[[col1]])
  colnames(temp_df) <- col1
  result <- temp_df
  i <- 1
  while (i <= ncol(df)) {
    col1 <- col_names[i]
    if (i + 1 <= ncol(df)) {
      col2 <- col_names[i + 1]
    } else {
      result <- rbind(result, df[[col1]])
      break
    }
    if (identical(col1, col2)) {
      concatenated <-
        ifelse(is.na(df[, i + 1]), as.character(df[, i]), paste(df[, i], df[, i + 1], sep = "/"))
      temp_df <- data.frame(col1 = concatenated)
      colnames(temp_df) <- col1
      result <- cbind(result, temp_df)
      i <- i + ploidy
    } else {
      result <- cbind(result, df[[col1]], df[[col2]])
      i <- i + ploidy
    }
  }
  result <- result[,-1]
  
  return(result)
}


# Define a function for rendering info boxes (server_importdata)
renderInfoBoxUI <- function(title, value, icon_name, color) {
  infoBox(
    title,
    value,
    icon = icon(icon_name),
    color = color,
    fill = TRUE
  )
}

# Define the function for the bootstrap (server_general_stats)
boot_fonction <- function(data, indices, columns) {
      subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
      subset_data <- adegenet::df2genind(
        X = as.matrix(subset_data),
        sep = "/",
        ncode = 6,
        ind.names = data$indv,
        pop = data$Population,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
      results_mat <- fst_results %>%
        select(Fis) %>%
        as.matrix()
      return(results_mat)
}




############################################ LD ########################################



# Function to create contingency tables for each population
create_contingency_tables <- function(data, loci, include_missing = TRUE) {
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
randomized_g_stats <- function(data, loci, n_simulations, calculate_g_stat, include_missing = TRUE) {
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


