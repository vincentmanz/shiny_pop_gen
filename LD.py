import sys
import json




import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count
from ast import literal_eval 

try:
    # Print the arguments for debugging
    print("Arguments received:", sys.argv)

    # Parse inputs
    data = pd.DataFrame(json.loads(sys.argv[1]))
    loci = json.loads(sys.argv[2])
    n_simulations = int(sys.argv[3])
    
    # Debug parsed data
    print("Data parsed successfully:")
    print(data.head())

except Exception as e:
    print("Error parsing inputs:", str(e))
    sys.exit(1)





import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# # Dataset
# data = pd.DataFrame({
#     'Individual': ['Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5', 'Ind6', 'Ind7', 'Ind8', 'Ind9'],
#     'Population': ['Population1', 'Population1', 'Population1', 'Population2', 'Population2', 'Population2', 'Population3', 'Population3', 'Population3'],
#     'H1': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '121/164', '171/181', '167/174'],
#     'H2': ['120/165', '120/165', '120/165', '120/170', '120/165', '120/170', '122/169', '163/172', '175/186'],
#     'H3': ['120/165', '120/165', '120/165', '165/170', '165/170', '120/165', '166/168', '123/173', '125/190'],
#     'H4': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '177/179', '129/195', '124/199']
# })



# Read arguments passed from R
data = pd.DataFrame(literal_eval(sys.argv[1]))
loci = literal_eval(sys.argv[2])
n_simulations = int(sys.argv[3])
loci_pairs = list(combinations(loci, 2))

# Function to split alleles into two numeric columns
def split_alleles(column):
    alleles = column.str.split("/", expand=True)
    alleles.columns = ["allele1", "allele2"]
    alleles = alleles.apply(pd.to_numeric, errors='coerce')
    return alleles
    # Check the function
    # sample_alleles = pd.Series(['120/165', '120/165', '120/170', '165/170'])
    # split_result = split_alleles(sample_alleles)
    # print(split_result)


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

# Check the function
import numpy as np
import pandas as pd
from itertools import combinations

# Function to randomize haplotypes within a population
def randomize_haplotypes_within_population(pop_data, loci):
    randomized_data = pop_data.copy()
    for locus in loci:
        randomized_data[locus] = np.random.permutation(randomized_data[locus])
    return randomized_data

# Function to calculate G-statistic
def calculate_g_stat(contingency_table):
    observed = contingency_table.values
    row_totals = observed.sum(axis=1)
    col_totals = observed.sum(axis=0)
    grand_total = observed.sum()
    expected = np.outer(row_totals, col_totals) / grand_total
    mask = observed > 0
    g_stat = 2 * np.sum(observed[mask] * np.log(observed[mask] / expected[mask]))
    return g_stat, expected

# Loci combinations
loci = ['H1', 'H2', 'H3', 'H4']
loci_pairs = list(combinations(loci, 2))

    # # Example data for testing
    # # Dataset
    # data = pd.DataFrame({
    #     'Individual': ['Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5', 'Ind6', 'Ind7', 'Ind8', 'Ind9'],
    #     'Population': ['Population1', 'Population1', 'Population1', 'Population2', 'Population2', 'Population2', 'Population3', 'Population3', 'Population3'],
    #     'H1': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '121/164', '171/181', '167/174'],
    #     'H2': ['120/165', '120/165', '120/165', '120/170', '120/165', '120/170', '122/169', '163/172', '175/186'],
    #     'H3': ['120/165', '120/165', '120/165', '165/170', '165/170', '120/165', '166/168', '123/173', '125/190'],
    #     'H4': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '177/179', '129/195', '124/199']
    # })
    # # Loop over each population to test the randomization and calculate G-statistics
    # for pop in data['Population'].unique():
    #     population_data = data[data['Population'] == pop].copy()
    #     # Before randomization
    #     print(f"\nBefore randomization for {pop}:")
    #     print(population_data[['Individual', 'H1', 'H2', 'H3', 'H4']])
    #     # Calculate G-statistic before randomization
    #     for pair in loci_pairs:
    #         locus1_split = population_data[pair[0]].str.split("/", expand=True).apply(pd.to_numeric, errors='coerce')
    #         locus2_split = population_data[pair[1]].str.split("/", expand=True).apply(pd.to_numeric, errors='coerce')
    #         allele_data = pd.DataFrame({
    #             'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
    #             'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
    #         })
    #         contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
    #         g_stat_before, expected_before = calculate_g_stat(contingency_table)
    #         print(f"\nContingency Table for {pair[0]}-{pair[1]} before randomization:")
    #         print(contingency_table)
    #         print(f"Expected Table for {pair[0]}-{pair[1]} before randomization:")
    #         print(expected_before)
    #         print(f"G-statistic for {pair[0]}-{pair[1]} before randomization: {g_stat_before}")
    #     # Randomize the haplotypes within the population
    #     randomized_data = randomize_haplotypes_within_population(population_data, loci)
    #     # After randomization
    #     print(f"\nAfter randomization for {pop}:")
    #     print(randomized_data[['Individual', 'H1', 'H2', 'H3', 'H4']])
    #     # Calculate G-statistic after randomization
    #     for pair in loci_pairs:
    #         locus1_split = randomized_data[pair[0]].str.split("/", expand=True).apply(pd.to_numeric, errors='coerce')
    #         locus2_split = randomized_data[pair[1]].str.split("/", expand=True).apply(pd.to_numeric, errors='coerce')
    #         allele_data = pd.DataFrame({
    #             'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
    #             'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
    #         })
    #         contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
    #         g_stat_after, expected_after = calculate_g_stat(contingency_table)
    #         print(f"\nContingency Table for {pair[0]}-{pair[1]} after randomization:")
    #         print(contingency_table)
    #         print(f"Expected Table for {pair[0]}-{pair[1]} after randomization:")
    #         print(expected_after)
    #         print(f"G-statistic for {pair[0]}-{pair[1]} after randomization: {g_stat_after}")


# Function to generate randomized G-statistics for a single population
def generate_randomized_g_stats_for_population(args):
    pop, pop_data, loci, loci_pairs, n_simulations = args
    results = {f"{pair[0]}-{pair[1]}": [] for pair in loci_pairs}
    for _ in range(n_simulations):
        randomized_pop_data = randomize_haplotypes_within_population(pop_data, loci)
        for pair in loci_pairs:
            locus1_split = split_alleles(randomized_pop_data[pair[0]])
            locus2_split = split_alleles(randomized_pop_data[pair[1]])
            allele_data = pd.DataFrame({
                'Locus1_allele': pd.concat([locus1_split['allele1'], locus1_split['allele2']]),
                'Locus2_allele': pd.concat([locus2_split['allele1'], locus2_split['allele2']])
            })
            contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
            g_stat = calculate_g_stat(contingency_table)
            results[f"{pair[0]}-{pair[1]}"].append(g_stat)
    return pop, results

# Function to run simulations in parallel
def generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations=100):
    populations = data['Population'].unique()
    n_cores = cpu_count()  # Use all available CPU cores
    args_list = [
        (pop, data[data['Population'] == pop], loci, loci_pairs, n_simulations)
        for pop in populations
    ]
    with Pool(n_cores) as pool:
        results = pool.map(generate_randomized_g_stats_for_population, args_list)
    return {pop: res for pop, res in results}


# Run the simulation
randomized_g_stats = generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations=100)


# Output results to a JSON file or stdout
print(json.dumps(randomized_g_stats))