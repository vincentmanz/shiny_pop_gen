from Bio.PopGen import GenePop
from Bio.PopGen.GenePop.Controller import GenePopController
import os


# Initialize GenePop controller
controller = GenePopController()

# result = controller.test_ld("population_results/population_1.txt", dememorization=1000, batches=10, iterations=10000)

result = controller.test_ld("population_results/population_2.txt", dememorization=1000, batches=10, iterations=10000)

quit()


# Function to format genotype
def format_genotype(genotype):
    return " ".join(f"{allele1:03d}{allele2:03d}" if allele1 and allele2 else "000000"
                    for allele1, allele2 in genotype)

# Define a function to write a GenePop record to a file
def write_genepop_record(record, file_path):
    with open(file_path, "w") as f:
        # Write the header
        f.write(f"Genepop file\n")
        
        # Write marker names
        for locus in record.loci_list:
            f.write(locus + "\n")
        
        # Write populations
        for i, population in enumerate(record.populations, start=1):
            if i > 1:
                f.write("Pop\n")
            for individual, genotype in population:
                formatted_genotype = format_genotype(genotype)
                f.write(f"{individual}, {formatted_genotype}\n")

# Input file path
input_file_path = "GenePop_UNFLATTENED.txt"

# Output directory
output_dir = "population_results"
os.makedirs(output_dir, exist_ok=True)

# Read the GenePop file
with open(input_file_path) as handle:
    rec = GenePop.read(handle)
    print("File read successfully.")

# Process each population
for i, population in enumerate(rec.populations, start=1):
    # Create a new GenePop record for this population
    new_rec = GenePop.Record()
    new_rec.marker_len = rec.marker_len
    new_rec.loci_list = rec.loci_list
    new_rec.populations = [population]
    
    # Write the population-specific file
    pop_file = os.path.join(output_dir, f"population_{i}.txt")
    write_genepop_record(new_rec, pop_file)
    print(f"Population {i} written to {pop_file}.")

# Initialize GenePop controller
controller = GenePopController()

# Perform LD tests for each population
print("Starting LD calculations...")
for i in range(1, len(rec.populations) + 1):
    pop_file = os.path.join(output_dir, f"population_{i}.txt")
    result_file = os.path.join(output_dir, f"population_{i}_results.txt")
    
    print(f"Performing LD test for Population {i}...")
    result = controller.test_ld(pop_file, dememorization=10, batches=5, iterations=100)
    with open(result_file, "w") as rf:
        for file_iterator in result:
            for line in file_iterator:
                rf.write(line if isinstance(line, str) else line[0])  # Handle tuple or string
            file_iterator.close()
    print(f"Results saved to {result_file}.")

print("\nLD calculations complete for all populations.")
