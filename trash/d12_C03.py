import sys
import json
import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Parameters
DATA_FILE = "dummy_data.csv"  # Path to the CSV file
LOCI = ["B12", "C07", "D12", "D10", "A12", "C03"]  # List of loci
N_SIMULATIONS = 10  # Number of simulations
WORKERS = 60  # Number of workers for multiprocessing

# Function to calculate G-statistic
def calculate_g_stat(contingency_table):
    observed = contingency_table.values
    row_totals = observed.sum(axis=1)
    col_totals = observed.sum(axis=0)
    grand_total = observed.sum()
    expected = np.outer(row_totals, col_totals) / grand_total
    mask = observed > 0
    g_stat = 2 * np.sum(observed[mask] * np.log(observed[mask] / expected[mask]))
    return g_stat

# Function to randomize haplotypes within a population
def randomize_haplotypes_within_population(pop_data, loci):
    randomized_data = pop_data.copy()
    for locus in loci:
        randomized_data[locus] = np.random.permutation(randomized_data[locus])
    return randomized_data

# Function to generate randomized contingency tables and G-statistics for a specific pair and population
def generate_randomized_g_stats_for_population(args):
    pop, pop_data, pair, n_simulations = args
    contingency_tables = []
    g_stats = []
    for _ in range(n_simulations):
        # Randomize haplotypes
        randomized_pop_data = randomize_haplotypes_within_population(pop_data, pair)
        haplotype_data = pd.DataFrame({
            'Locus1_haplotype': randomized_pop_data[pair[0]],
            'Locus2_haplotype': randomized_pop_data[pair[1]],
        })

        # Create contingency table
        contingency_table = pd.crosstab(
            haplotype_data['Locus1_haplotype'],
            haplotype_data['Locus2_haplotype']
        )

        # Skip empty tables
        if contingency_table.empty:
            continue

        # Calculate G-stat for the table
        g_stat = calculate_g_stat(contingency_table)

        # Store results
        contingency_tables.append(contingency_table)
        g_stats.append(g_stat)

    return contingency_tables, g_stats

# Main execution
if __name__ == "__main__":
    try:
        # Read data
        data = pd.read_csv(DATA_FILE)

        # Validate loci
        available_loci = set(data.columns)
        valid_loci = [locus for locus in LOCI if locus in available_loci]
        missing_loci = [locus for locus in LOCI if locus not in available_loci]
        if missing_loci:
            print(f"Warning: The following loci are missing and will be skipped: {missing_loci}")

        # Specify the pair of loci
        pair = ("D12", "C03")
        if pair[0] not in valid_loci or pair[1] not in valid_loci:
            raise ValueError(f"One or both loci in the pair {pair} are not available in the data.")

        # Run simulations
        args = ("A", data[data["Population"] == "A"], pair, N_SIMULATIONS)
        contingency_tables, g_stats = generate_randomized_g_stats_for_population(args)

        # Save each contingency table as a separate CSV file
        for i, table in enumerate(contingency_tables):
            table.to_csv(f"contingency_table_{i}.csv")

        # Output G-stats
        g_stats_df = pd.DataFrame(g_stats, columns=["G_stat"])
        g_stats_df.to_csv("g_stats.csv", index=False)

        print("Simulation completed. Contingency tables saved as individual CSV files and G-stats saved to 'g_stats.csv'.")

    except Exception as e:
        sys.stderr.write(f"Error: {str(e)}\n")
        sys.exit(1)
