import pandas as pd
import numpy as np
from concurrent.futures import ProcessPoolExecutor

# Split haplotypes into alleles
def split_alleles(column):
    alleles = column.str.split('/', expand=True)
    return pd.concat([alleles[0].astype(int), alleles[1].astype(int)], axis=1)

# Create contingency tables
def create_contingency_tables(data, loci):
    locus_pairs = [(loci[i], loci[j]) for i in range(len(loci)) for j in range(i + 1, len(loci))]
    contingency_tables = {}
    for pair in locus_pairs:
        locus1_split = split_alleles(data[pair[0]])
        locus2_split = split_alleles(data[pair[1]])
        allele_data = pd.DataFrame({
            'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
            'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
        })
        contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
        contingency_tables[f'{pair[0]}.{pair[1]}'] = contingency_table
    return contingency_tables

# Compute G-statistic
def calculate_g_stat(table):
    total = table.values.sum()
    row_sums = table.sum(axis=1).values
    col_sums = table.sum(axis=0).values
    expected = np.outer(row_sums, col_sums) / total
    observed = table.values
    non_zero = observed > 0  # Avoid log(0)
    g_stat = 2 * np.sum(observed[non_zero] * np.log(observed[non_zero] / expected[non_zero]))
    return g_stat

# Randomize haplotypes within a population
def randomize_haplotypes(data, loci):
    randomized_data = data.copy()
    for locus in loci:
        randomized_data[locus] = np.random.permutation(data[locus])
    return randomized_data

# Simulate G-statistics for a single pair
def simulate_one_pair(args):
    pop_data, pair, loci, n_simulations = args
    g_stats = []
    for _ in range(n_simulations):
        randomized_data = randomize_haplotypes(pop_data, loci)
        locus1_split = split_alleles(randomized_data[pair[0]])
        locus2_split = split_alleles(randomized_data[pair[1]])
        randomized_allele_data = pd.DataFrame({
            'Locus1_allele': pd.concat([locus1_split[0], locus1_split[1]]),
            'Locus2_allele': pd.concat([locus2_split[0], locus2_split[1]])
        })
        randomized_table = pd.crosstab(randomized_allele_data['Locus1_allele'], randomized_allele_data['Locus2_allele'])
        g_stats.append(calculate_g_stat(randomized_table))
    return g_stats

# Simulate randomized G-statistics in parallel for all locus pairs for each population
def simulate_randomized_parallel(data, loci, n_simulations, n_workers):
    populations = data['Population'].unique()
    results = {}
    for pop in populations:
        pop_data = data[data['Population'] == pop].copy()
        locus_pairs = [(loci[i], loci[j]) for i in range(len(loci)) for j in range(i + 1, len(loci))]
        pop_results = {}
        
        # Run simulations in parallel for each pair
        with ProcessPoolExecutor(max_workers=n_workers) as executor:
            future_to_pair = {
                executor.submit(simulate_one_pair, (pop_data, pair, loci, n_simulations)): pair
                for pair in locus_pairs
            }
            for future in future_to_pair:
                pair = future_to_pair[future]
                pop_results[f'{pair[0]}.{pair[1]}'] = future.result()
        
        results[pop] = pop_results
    return results

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
n_simulations = 10
n_workers = 4
loci_pairs = list(combinations(loci, 2))


# Run the simulation
randomized_g_stats = generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations=100)

# Print results
print(randomized_g_stats)
