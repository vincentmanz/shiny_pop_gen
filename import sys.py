import sys
import json
import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Parameters
DATA_FILE = "data/data-2023-09-11 (2).csv"  # Path to the CSV file
LOCI = ["B12", "C07", "D12", "D10", "A12", "C03"]  # List of loci
N_SIMULATIONS = 100  # Number of simulations
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

# Function to generate randomized G-statistics for a single population
def generate_randomized_g_stats_for_population(args):
    pop, pop_data, loci, loci_pairs, n_simulations = args
    results = {f"{pair[0]}-{pair[1]}": [] for pair in loci_pairs}
    for _ in range(n_simulations):
        randomized_pop_data = randomize_haplotypes_within_population(pop_data, loci)
        for pair in loci_pairs:
            haplotype_data = pd.DataFrame({
                'Locus1_haplotype': randomized_pop_data[pair[0]],
                'Locus2_haplotype': randomized_pop_data[pair[1]],
            })
            contingency_table = pd.crosstab(
                haplotype_data['Locus1_haplotype'],
                haplotype_data['Locus2_haplotype']
            )

            # Clean the contingency table
            non_zero_rows = contingency_table.index != "0/0"
            non_zero_cols = contingency_table.columns != "0/0"
            cleaned_table = contingency_table.loc[non_zero_rows, non_zero_cols]

            # Skip empty tables after cleaning
            if cleaned_table.empty:
                continue

            # Calculate G-stat for the cleaned table
            g_stat = calculate_g_stat(cleaned_table)
            results[f"{pair[0]}-{pair[1]}"].append(g_stat)
    return pop, results

# Function to run simulations in parallel
def generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations):
    populations = data['Population'].unique()
    n_cores = min(WORKERS, cpu_count())  # Use specified or available CPU cores
    args_list = [
        (pop, data[data['Population'] == pop], loci, loci_pairs, n_simulations)
        for pop in populations
    ]
    with Pool(n_cores) as pool:
        results = pool.map(generate_randomized_g_stats_for_population, args_list)
    return {pop: res for pop, res in results}

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
            print(f"Warning: The following loci are missing in the data and will be skipped: {missing_loci}")
        loci_pairs = list(combinations(valid_loci, 2))

        # Run simulations
        randomized_g_stats = generate_randomized_g_stats_parallel(data, valid_loci, loci_pairs, N_SIMULATIONS)

        # Output results
        with open("randomized_g_stats.json", "w") as output_file:
            json.dump(randomized_g_stats, output_file)
        print("Simulation completed. Results saved to 'randomized_g_stats.json'.")

    except Exception as e:
        sys.stderr.write(f"Error: {str(e)}\n")
        sys.exit(1)
print(randomized_g_stats)
