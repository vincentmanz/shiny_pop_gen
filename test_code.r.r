




# Make controls
  # check contengcy tables for population 3 pair 1-2  
    # Access the contingency table
    contingency_table <- observed_g_stats$Population2$H1.H2$contingency_table
    # Perform the Chi-squared test to get expected frequencies
    chisq_result <- chisq.test(contingency_table, simulate.p.value = FALSE)
    # Extract expected frequencies
    expected <- chisq_result$expected

  # check gstat function tables for population 3 pair 1-2  
    library(vcd)
    # Access the contingency table for Population 3, pair H1-H2
    contingency_table <- observed_g_stats$Population3$H1.H2$contingency_table
    # Perform assocstats on the contingency table
    assoc_stats <- assocstats(as.table(contingency_table))
    g_stat_assocstats <- assoc_stats$chisq_tests["Likelihood Ratio", "X^2"]
    # Extract expected frequencies from assocstats
    chisq_result <- chisq.test(contingency_table, simulate.p.value = FALSE)
    expected_chisq <- chisq_result$expected
    # Compare the expected frequencies from assocstats and chisq.test
    g_stat_observed <- observed_g_stats$Population3$H1.H2$g_stat
    expected_observed <- observed_g_stats$Population3$H1.H2$expected_contingency_table
    # Print results
    cat("Observed Contingency Table:\n")
    print(contingency_table)
    cat("\nExpected Frequencies (from chisq.test):\n")
    print(expected_chisq)
    cat("\nExpected Frequencies (from observed_g_stats):\n")
    print(expected_observed)
    cat("\nG-Statistic (from assocstats):", g_stat_assocstats, "\n")
    cat("G-Statistic (from observed_g_stats):", g_stat_observed, "\n")

    # Validate consistency  
    if (abs(g_stat_assocstats - g_stat_observed) < 1e-6) {
    cat("\nG-Statistics are consistent!\n")
    } else {
    cat("\nDiscrepancy detected in G-Statistics!\n")
    }


# Control on global gstats: 
# Libraries
library(vcd)

# Run the function for the dataset
loci <- c("H1", "H2", "H3", "H4")
results <- compute_stats_for_all_pairs(data, loci)


# Test pvalues for the G-statistics of the contingency tables for each population and locus pair.

# Fake observed and simulated G-statistics
observed_g_stats <- list(
  Population1 = list(
    "H1-H2" = list(g_stat = 5),
    "H1-H3" = list(g_stat = 10)
  ),
  Population2 = list(
    "H1-H2" = list(g_stat = 15),
    "H1-H3" = list(g_stat = 20)
  )
)

simulated_g_stats <- list(
  Population1 = list(
    "H1-H2" = c(6, 7, 5, 4, 3),  # Simulated values for H1-H2
    "H1-H3" = c(8, 10, 12, 11, 9) # Simulated values for H1-H3
  ),
  Population2 = list(
    "H1-H2" = c(14, 15, 16, 13, 12), # Simulated values for H1-H2
    "H1-H3" = c(19, 21, 18, 22, 20)  # Simulated values for H1-H3
  )
)

# Expected p-values:
# For Population1:
# - H1-H2: Observed = 5, Simulated = [6, 7, 5, 4, 3]. P(>=5) = 3/5 = 0.6
# - H1-H3: Observed = 10, Simulated = [8, 10, 12, 11, 9]. P(>=10) = 3/5 = 0.6
#
# For Population2:
# - H1-H2: Observed = 15, Simulated = [14, 15, 16, 13, 12]. P(>=15) = 2/5 = 0.4
# - H1-H3: Observed = 20, Simulated = [19, 21, 18, 22, 20]. P(>=20) = 3/5 = 0.6

# Run the calculate_pvalues function
results <- calculate_pvalues(observed_g_stats, simulated_g_stats)

# Display the results
print("Calculated p-values:")
print(results)

# Verify the results manually
expected_pvalues <- list(
  Population1 = list(
    "H1-H2" = list(observed_g_stat = 5, p_value = 0.6),
    "H1-H3" = list(observed_g_stat = 10, p_value = 0.6)
  ),
  Population2 = list(
    "H1-H2" = list(observed_g_stat = 15, p_value = 0.4),
    "H1-H3" = list(observed_g_stat = 20, p_value = 0.6)
  )
)

print("Expected p-values:")
print(expected_pvalues)

# Test if the function output matches the expected values
if (all.equal(results, expected_pvalues)) {
  cat("Test passed: The function is working correctly.\n")
} else {
  cat("Test failed: The function output does not match the expected values.\n")
}





# Test using hierfstat

