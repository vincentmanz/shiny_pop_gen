import numpy as np
import pandas as pd
from itertools import combinations
from multiprocessing import Pool, cpu_count

# Load the dataset
data = pd.read_csv("data/data-2023-09-11 (2).csv")

# Extract unique populations and loci
populations = data['Population'].unique()
loci = ["B12", "C07", "D12", "D10", "A12", "C03"]
loci_pairs = list(combinations(loci, 2))

# Function to split alleles into two numeric columns
def split_alleles(column):
    alleles = column.str.split("/", expand=True)
    alleles.columns = ["allele1", "allele2"]
    alleles = alleles.apply(pd.to_numeric, errors='coerce')
    return alleles

# Optimised G-stat calculation
def calculate_g_stat(contingency_table):
    observed = contingency_table.values
    row_totals = observed.sum(axis=1)
    col_totals = observed.sum(axis=0)
    grand_total = observed.sum()
    
    # Compute expected values
    expected = np.outer(row_totals, col_totals) / grand_total
    
    # Mask invalid divisions (expected == 0)
    with np.errstate(divide='ignore', invalid='ignore'):
        ratio = np.where(expected > 0, observed / expected, 1)  # Avoid division by 0
        log_ratio = np.log(ratio)  # Only take log of valid ratios
        log_ratio[~np.isfinite(log_ratio)] = 0  # Replace invalid logs with 0
    
    # Compute G-statistic
    g_stat = 2 * np.sum(observed * log_ratio)
    return g_stat



# Refactor randomisation to use numpy shuffle for efficiency
def randomize_haplotypes_within_population(pop_data, loci):
    randomized_data = pop_data.copy()
    for locus in loci:
        np.random.shuffle(randomized_data[locus].values)  # Shuffle in-place for efficiency
    return randomized_data

# Refactored parallel function for clarity
def generate_randomized_g_stats_for_population(args):
    pop_data, loci, loci_pairs, n_simulations = args
    results = {f"{pair[0]}-{pair[1]}": [] for pair in loci_pairs}
    for _ in range(n_simulations):
        randomized_pop_data = randomize_haplotypes_within_population(pop_data, loci)
        for pair in loci_pairs:
            locus1_split = split_alleles(randomized_pop_data[pair[0]])
            locus2_split = split_alleles(randomized_pop_data[pair[1]])
            allele_data = pd.concat([
                pd.concat([locus1_split.iloc[:, i], locus2_split.iloc[:, i]])
                for i in range(2)
            ], axis=1, keys=["Locus1_allele", "Locus2_allele"])
            contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
            results[f"{pair[0]}-{pair[1]}"].append(calculate_g_stat(contingency_table))
    return results


# Parallel processing for randomised G-statistics
def generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations=100):
    populations = data['Population'].unique()
    n_cores = cpu_count()  # Use all available CPU cores

    # Prepare arguments with explicit structure
    args_list = [
        (data[data['Population'] == pop], loci, loci_pairs, n_simulations)
        for pop in populations
    ]

    # Use multiprocessing pool
    with Pool(n_cores) as pool:
        results = pool.map(generate_randomized_g_stats_for_population, args_list)
    
    # Combine results into a dictionary
    return {pop: res for pop, res in zip(populations, results)}

# Ensure function matches expected args
def generate_randomized_g_stats_for_population(args):
    # Unpack arguments correctly
    pop_data, loci, loci_pairs, n_simulations = args
    
    # Process G-statistics for each population
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
    return results


# Generate contingency tables
def create_contingency_tables(data, loci):
    contingency_tables = {}
    for pop in populations:
        pop_data = data[data['Population'] == pop]
        contingency_tables[pop] = {}
        for pair in loci_pairs:
            locus1_split = split_alleles(pop_data[pair[0]])
            locus2_split = split_alleles(pop_data[pair[1]])
            allele_data = pd.DataFrame({
                'Locus1_allele': pd.concat([locus1_split['allele1'], locus1_split['allele2']]),
                'Locus2_allele': pd.concat([locus2_split['allele1'], locus2_split['allele2']])
            })
            contingency_table = pd.crosstab(allele_data['Locus1_allele'], allele_data['Locus2_allele'])
            contingency_tables[pop][f"{pair[0]}-{pair[1]}"] = contingency_table
    return contingency_tables

# Add G-statistics to the tables
def add_g_stats_to_population_tables(contingency_tables):
    g_stats_by_population = {}
    for pop, tables in contingency_tables.items():
        g_stats_by_population[pop] = {}
        for pair, table in tables.items():
            g_stat = calculate_g_stat(table)
            g_stats_by_population[pop][pair] = {'contingency_table': table, 'g_stat': g_stat}
    return g_stats_by_population

# Calculate p-values
def calculate_pvalues(observed_g_stats, simulated_g_stats):
    pvalues = {}
    for pop in observed_g_stats:
        pvalues[pop] = {}
        for pair in observed_g_stats[pop]:
            observed_g = observed_g_stats[pop][pair]['g_stat']
            simulated_g = simulated_g_stats[pop][pair]
            p_value = np.mean(np.array(simulated_g) >= observed_g)
            pvalues[pop][pair] = {
                'observed_g_stat': observed_g,
                'p_value': p_value
            }
    return pvalues

# Precompute and optimise global p-value calculation
def calculate_global_pvalues(observed_g_stats, simulated_g_stats):
    all_pairs = set().union(*[set(observed_g_stats[pop].keys()) for pop in observed_g_stats])
    global_pvalues = {}
    for pair in all_pairs:
        g_obs = np.array([observed_g_stats[pop][pair]['g_stat'] for pop in observed_g_stats if pair in observed_g_stats[pop]])
        g_sim = np.concatenate([simulated_g_stats[pop][pair] for pop in simulated_g_stats if pair in simulated_g_stats[pop]])
        global_pvalues[pair] = np.mean(g_sim >= np.mean(g_obs))
    return global_pvalues


# Create summary table
def create_summary_table(pvalues, global_pvalues):
    all_pairs = set().union(*[set(pvalues[pop].keys()) for pop in populations])
    summary_table = pd.DataFrame({'Locus_Pair': list(all_pairs)})
    for pop in populations:
        summary_table[pop] = summary_table['Locus_Pair'].apply(lambda pair: pvalues[pop].get(pair, {}).get('p_value', None))
    summary_table['Global_P_Value'] = summary_table['Locus_Pair'].apply(lambda pair: global_pvalues.get(pair, None))
    return summary_table

# Full Workflow
contingency_tables = create_contingency_tables(data, loci)
observed_g_stats = add_g_stats_to_population_tables(contingency_tables)
randomized_g_stats = generate_randomized_g_stats_parallel(data, loci, loci_pairs, n_simulations=100)
pvalues = calculate_pvalues(observed_g_stats, randomized_g_stats)
global_pvalues = calculate_global_pvalues(observed_g_stats, randomized_g_stats)
summary_table = create_summary_table(pvalues, global_pvalues)

# View the final summary table
print(summary_table)


