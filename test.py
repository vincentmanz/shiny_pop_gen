import pandas as pd
import matplotlib.pyplot as plt

# Data from LD_parallel.py
parallel_data = pd.DataFrame({
    "Locus_Pair": ["C07-D12", "B12-C03", "B12-A12", "A12-C03", "C07-A12", "B12-C07", "D12-D10",
                   "D10-A12", "B12-D10", "C07-D10", "C07-C03", "B12-D12", "D12-C03", "D12-A12", "D10-C03"],
    "Global_P_Value": [0.476162, 0.414700, 0.536963, 0.342562, 0.358625, 0.345162, 0.519075,
                       0.447675, 0.438088, 0.472000, 0.352975, 0.324225, 0.471788, 0.416212, 0.533100]
})

# Data from LD_sequencial.py
sequencial_data = pd.DataFrame({
    "Locus_Pair": ["B12-A12", "D12-D10", "D12-A12", "B12-D10", "D12-C03", "B12-C07", "B12-D12",
                   "C07-A12", "C07-D10", "C07-C03", "D10-C03", "B12-C03", "D10-A12", "A12-C03", "C07-D12"],
    "Global_P_Value": [0.539400, 0.520925, 0.415338, 0.440238, 0.471162, 0.347425, 0.326950,
                       0.360162, 0.471600, 0.350000, 0.531350, 0.414225, 0.450462, 0.339975, 0.476313]
})

# Data from LD.R
r_data = pd.DataFrame({
    "Locus_Pair": ["B12-C07", "B12-D12", "B12-D10", "B12-A12", "B12-C03", "C07-D12", "C07-D10",
                   "C07-A12", "C07-C03", "D12-D10", "D12-A12", "D12-C03", "D10-A12", "D10-C03", "A12-C03"],
    "Global_P_Value": [0.34345, 0.3269125, 0.4386875, 0.53595, 0.4152, 0.4759, 0.471275,
                       0.360425, 0.3512, 0.5221625, 0.41755, 0.469575, 0.4518125, 0.5327875, 0.337375]
})

# Merge the data for comparison
merged_data = pd.merge(parallel_data, sequencial_data, on="Locus_Pair", suffixes=('_Parallel', '_Sequencial'))
merged_data = pd.merge(merged_data, r_data, on="Locus_Pair")
merged_data.rename(columns={"Global_P_Value": "Global_P_Value_R"}, inplace=True)

# Plot the comparison
plt.figure(figsize=(12, 8))
plt.plot(merged_data["Locus_Pair"], merged_data["Global_P_Value_Parallel"], label="LD_parallel.py", marker='o')
plt.plot(merged_data["Locus_Pair"], merged_data["Global_P_Value_Sequencial"], label="LD_sequencial.py", marker='s')
plt.plot(merged_data["Locus_Pair"], merged_data["Global_P_Value_R"], label="LD.R", marker='^')

# Add labels and legend
plt.title("Comparison of Global P-Values Across Methods")
plt.xlabel("Locus Pair")
plt.ylabel("Global P-Value")
plt.xticks(rotation=45, ha='right')
plt.legend()
plt.tight_layout()

# Display the plot
plt.show()
