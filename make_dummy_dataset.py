import pandas as pd

import numpy as np




# Function to create a reverse-engineered dataset in a table format
def create_reverse_engineered_table():
    # Define populations and loci pairs with target p-values
    populations = ["Pop1", "Pop2"]
    loci_pairs = {
        "Locus1-Locus2": 0.0,
        "Locus3-Locus4": 0.25,
        "Locus5-Locus6": 0.5,
        "Locus7-Locus8": 0.75,
        "Locus9-Locus10": 1.0,
    }

    # Create a list to hold the table rows
    data = []

    for pop in populations:
        for pair, target_pvalue in loci_pairs.items():
            locus1, locus2 = pair.split("-")

            # Number of individuals (haplotypes)
            num_individuals = 100

            # Number of individuals where Randomized_G > Observed_G
            n_greater = int(target_pvalue * num_individuals)

            # Create haplotypes for the table
            for i in range(num_individuals):
                if i < n_greater:
                    haplotype1 = f"{i % 4 + 1}/{i % 3 + 1}"  # High proportion haplotype
                    haplotype2 = f"{i % 4 + 1}/{i % 4 + 2}"  # High proportion haplotype
                else:
                    haplotype1 = f"{i % 4 + 1}/{i % 4 + 1}"  # Low proportion haplotype
                    haplotype2 = f"{i % 4 + 2}/{i % 4 + 3}"  # Low proportion haplotype

                # Add haplotypes in table format
                data.append({
                    "Population": pop,
                    locus1: haplotype1,
                    locus2: haplotype2
                })

    # Convert to a DataFrame
    df = pd.DataFrame(data)
    return df

# Create the reverse-engineered table dataset
reverse_engineered_table = create_reverse_engineered_table()

# Save the dataset as a CSV file
reverse_engineered_table.to_csv("reverse_engineered_table.csv", index=False)

# Display a preview
reverse_engineered_table.head()
