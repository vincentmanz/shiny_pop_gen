import pandas as pd
import numpy as np
from multiprocessing import Pool, cpu_count
from numba import njit
from itertools import combinations

# Dataset
data = pd.DataFrame({
    'Individual': ['Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5', 'Ind6', 'Ind7', 'Ind8', 'Ind9'],
    'Population': ['Population1', 'Population1', 'Population1', 'Population2', 'Population2', 'Population2', 'Population3', 'Population3', 'Population3'],
    'H1': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '121/164', '171/181', '167/174'],
    'H2': ['120/165', '120/165', '120/165', '120/170', '120/165', '120/170', '122/169', '163/172', '175/186'],
    'H3': ['120/165', '120/165', '120/165', '165/170', '165/170', '120/165', '166/168', '123/173', '125/190'],
    'H4': ['120/165', '120/165', '120/165', '120/165', '120/170', '165/170', '177/179', '129/195', '124/199']
})

# Variables
loci = ['H1', 'H2', 'H3', 'H4']
n_simulations = 10000
loci_pairs = list(combinations(loci, 2))
n_workers = 64

# Split haplotypes into alleles
def split_alleles(column):
    alleles = column.str.split('/', expand=True)
    alleles.columns = ["allele1", "allele2"]
    return alleles.astype(int)

# Compute G-statistic using Numba for speed
@njit
def calculate_g_stat(observed):
    row_totals = observed.sum(axis=1)
    col_totals = observed.sum(axis=0)
    grand_total = observed.sum()
    expected = np.outer(row_totals, col_totals) / grand_total
    
    g_stat = 0.0
    for i in range(observed.shape[0]):
        for j in range(observed.shape[1]):
            if observed[i, j] > 0 and expected[i, j] > 0:
                g_stat += 2 * observed[i, j] * np.log(observed[i, j] / expected[i, j])
    return g_stat

# Randomize haplotypes by shuffling loci in-place
def randomize_haplotypes(data, loci):
    randomized_data = data.copy()
    for locus in loci:
        randomized_data[locus] = np.random.permutation(data[locus].values)
    return randomized_data

# Simulate G-statistics for a single pair
def simulate_one_pair(args):
    pop_data, pair, n_simulations = args
    g_stats = []
    for _ in range(n_simulations):
        randomized_data = randomize_haplotypes(pop_data, [pair[0], pair[1]])
        locus1_split = split_alleles(randomized_data[pair[0]])
        locus2_split = split_alleles(randomized_data[pair[1]])
        
        allele_data = pd.DataFrame({
            'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
            'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
        })
        contingency_table = pd.crosstab(
            allele_data['Locus1_allele'], allele_data['Locus2_allele']
        ).values
        g_stats.append(calculate_g_stat(contingency_table))
    return g_stats

# Parallel simulation of G-statistics for all locus pairs
def simulate_randomized_parallel(data, loci, n_simulations, n_workers=None):
    populations = data['Population'].unique()
    locus_pairs = [(loci[i], loci[j]) for i in range(len(loci)) for j in range(i + 1, len(loci))]
    if n_workers is None:
        n_workers = cpu_count()
    
    results = {}
    with Pool(n_workers) as pool:
        for pop in populations:
            pop_data = data[data['Population'] == pop].copy()
            args_list = [(pop_data, pair, n_simulations) for pair in locus_pairs]
            pop_results = pool.map(simulate_one_pair, args_list)
            results[pop] = {f"{pair[0]}.{pair[1]}": res for pair, res in zip(locus_pairs, pop_results)}
    return results




# Run the simulation
simulated_stats = simulate_randomized_parallel(data, loci, n_simulations, n_workers)

# Print results
print(simulated_stats)
