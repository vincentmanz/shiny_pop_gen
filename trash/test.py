import sys
import json
import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Parameters
N_SIMULATIONS = 5  # Reduced for testing
WORKERS = 16  # Number of workers for multiprocessing

# Load the dataset
data = pd.DataFrame({
    "Individual": ["Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9"],
    "Population": ["Population1", "Population1", "Population1", "Population2", "Population2", "Population2", "Population3", "Population3", "Population3"],
    "H1": ["120/165", "120/165", "120/165", "0/0", "0/0", "165/170", "121/164", "171/181", "167/174"],
    "H2": ["120/165", "120/165", "120/165", "120/170", "120/165", "120/170", "122/169", "163/172", "175/186"],
    "H3": ["120/165", "120/165", "120/165", "165/170", "165/170", "120/165", "166/168", "123/173", "125/190"],
    "H4": ["120/165", "120/165", "120/165", "120/165", "120/170", "165/170", "177/179", "129/195", "124/199"]
})

# Extract unique populations and loci
populations = data["Population"].unique()
LOCI = ["H1", "H2", "H3", "H4"]

print("Dataset:")
print(data)
print("\nUnique populations:", populations)
print("Loci:", LOCI)

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
    print(f"\nPopulation: {pop}")
    print("Original Data:")
    print(pop_data)
    for _ in range(n_simulations):
        randomized_pop_data = randomize_haplotypes_within_population(pop_data, loci)
        print("\nRandomized Haplotypes:")
        print(randomized_pop_data)
        for pair in loci_pairs:
            # Remove rows with "0/0" for the specified loci pair
            valid_data = randomized_pop_data[(randomized_pop_data[pair[0]] != "0/0") & (randomized_pop_data[pair[1]] != "0/0")]
            haplotype_data = pd.DataFrame({
                'Locus1_haplotype': valid_data[pair[0]],
                'Locus2_haplotype': valid_data[pair[1]],
            })
            contingency_table = pd.crosstab(
                haplotype_data['Locus1_haplotype'],
                haplotype_data['Locus2_haplotype']
            )
            print(f"\nContingency Table for {pair[0]}-{pair[1]}:")
            print(contingency_table)

            # Calculate G-stat for the contingency table
            g_stat = calculate_g_stat(contingency_table)
            results[f"{pair[0]}-{pair[1]}"].append(g_stat)
    return pop, results


# Main execution
if __name__ == "__main__":
    try:
        # Validate loci
        available_loci = set(data.columns)
        valid_loci = [locus for locus in LOCI if locus in available_loci]
        missing_loci = [locus for locus in LOCI if locus not in available_loci]
        if missing_loci:
            print(f"Warning: The following loci are missing in the data and will be skipped: {missing_loci}")
        loci_pairs = list(combinations(valid_loci, 2))

        # Run simulations
        args_list = [
            (pop, data[data['Population'] == pop], valid_loci, loci_pairs, N_SIMULATIONS)
            for pop in populations
        ]
        for args in args_list:
            generate_randomized_g_stats_for_population(args)

    except Exception as e:
        sys.stderr.write(f"Error: {str(e)}\n")
        sys.exit(1)
