




# # Make controls
#   # check contengcy tables for population 3 pair 1-2  
#     # Access the contingency table
#     contingency_table <- observed_g_stats$Population2$H1.H2$contingency_table
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


