import sys
import json
import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count
from ast import literal_eval 

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
            # Remove rows with "0/0" for the specified loci pair
            pop_data = pop_data[(pop_data[pair[0]] != "0/0") & (pop_data[pair[1]] != "0/0")]
            haplotype_data = pd.DataFrame({
                'Locus1_haplotype': randomized_pop_data[pair[0]],
                'Locus2_haplotype': randomized_pop_data[pair[1]],
            })
            contingency_table = pd.crosstab(
                haplotype_data['Locus1_haplotype'],
                haplotype_data['Locus2_haplotype']
            )

            # Calculate G-stat for the contingency table
            g_stat = calculate_g_stat(contingency_table)
            results[f"{pair[0]}-{pair[1]}"].append(g_stat)
    return pop, results


# Function to run simulations in parallel
def generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations):
    populations = data['Population'].unique()
    n_cores = cpu_count()  # Use specified or available CPU cores
    args_list = [
        (pop, data[data['Population'] == pop], loci, loci_pairs, n_simulations)
        for pop in populations
    ]
    with Pool(n_cores) as pool:
        results = pool.map(generate_randomized_g_stats_for_population, args_list)
    return {pop: res for pop, res in results}

def parse_inputs():
    try:
        if len(sys.argv) < 4:
            raise ValueError("Not enough input arguments provided. Expecting: data, loci, n_simulations.")
        
        # Parse inputs from command-line arguments
        data = pd.DataFrame(json.loads(sys.argv[1]))
        loci = json.loads(sys.argv[2])
        n_simulations = int(sys.argv[3])

        # Validate inputs
        if not isinstance(loci, list) or len(loci) == 0:
            raise ValueError("Loci must be a non-empty list.")
        if n_simulations <= 0:
            raise ValueError("Number of simulations must be a positive integer.")

        return data, loci, n_simulations

    except Exception as e:
        sys.stderr.write(f"Error parsing inputs: {str(e)}\n")
        sys.exit(1)


# Main execution
if __name__ == "__main__":
    # Parse inputs
    data, loci, n_simulations = parse_inputs()
    
    # Generate loci pairs
    loci_pairs = list(combinations(loci, 2))
    # Simulation function
    try:
        randomized_g_stats = generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations)
        # Output results to stdout
        print(json.dumps(randomized_g_stats))
    except Exception as e:
        sys.stderr.write(f"Error during simulation: {str(e)}\n")
        sys.exit(1)
