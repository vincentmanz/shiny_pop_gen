import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Dataset
data = pd.DataFrame({
    'Individual': ['Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5', 'Ind6', 'Ind7', 'Ind8', 'Ind9'],
    'Population': ['Population1', 'Population1', 'Population1', 'Population2', 'Population2', 'Population2', 'Population3', 'Population3', 'Population3'],
    'H1': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '121/164', '171/181', '167/174'],
    'H2': ['120/165', '120/165', '120/165', '120/170', '120/165', '120/170', '122/169', '163/172', '175/186'],
    'H3': ['120/165', '120/165', '120/165', '165/170', '165/170', '120/165', '166/168', '123/173', '125/190'],
    'H4': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '177/179', '129/195', '124/199']
})


# Extract unique populations and loci
populations = data['Population'].unique()
loci = ["H1", "H2", "H3", "H4"]
loci_pairs = list(combinations(loci, 2))

# Function to split alleles into two numeric columns
def split_alleles(column):
    alleles = column.str.split("/", expand=True)
    alleles.columns = ["allele1", "allele2"]
    alleles = alleles.apply(pd.to_numeric, errors='coerce')
    return alleles

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

print(randomized_g_stats)
