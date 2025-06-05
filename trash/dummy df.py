import pandas as pd

# Define dummy data
populations = ['A', 'B']
individuals_per_population = 5
loci = ['D12', 'C03']
haplotypes = {
    'D12': ['1/1', '1/2', '2/2', '1/1', '2/1'],
    'C03': ['1/1', '2/2', '1/2', '2/1', '1/1']
}

# Create the dummy dataset
data = []
for pop in populations:
    for i in range(individuals_per_population):
        row = {
            'Population': pop,
            'Individual': f'{pop}_{i+1}',
            'D12': haplotypes['D12'][i % len(haplotypes['D12'])],
            'C03': haplotypes['C03'][i % len(haplotypes['C03'])]
        }
        data.append(row)

# Convert to DataFrame
df = pd.DataFrame(data)

# Save to CSV
dummy_csv_path = "dummy_data.csv"
df.to_csv(dummy_csv_path, index=False)

print(f"Dummy data created and saved to {dummy_csv_path}.")
print(df)

