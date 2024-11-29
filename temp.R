filtered_data <- read.csv("/home/vincent/Documents/shiny_pop_gen/data/data-2023-09-11 (2).csv", header = TRUE)

# information inherited from previous page
n_marker <- 6
n_pop <- 8
marker_start <- 6
marker_end <- 11
sequence_length <- length(marker_start:marker_end)

n_indv <- nrow(filtered_data)
pops <- unique(filtered_data$Population)
## Specify the number of cores available
num_cores <- parallel::detectCores()
## Code for missing data
missing_data_code <- "0/0"
## genotype columns
column_genotype_start <- 6
column_genotype_end <- 11

# Specify the number of bootstrap replicates
R <- 1000
# Specify the number of replicates (HW-Panmixia)
n_rep <- 1000


library(adegenet)
library(GenoPop)
library(graph4lg)
filtered_data <- data.frame(indv = paste(substr(filtered_data$Population, 1, 3), row.names(filtered_data), sep = "."), filtered_data)
mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end], sep = "/", ncode = 6, ind.names = filtered_data$indv, pop = filtered_data$Population, NA.char = "NA", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL) # nolint: line_length_linter.

mydata_genpop <- genind2genpop(mydata_genind)

output_file <- "mydata_genpop.gen"  # Specify output file name
genind_to_genepop(mydata_genind, output = output_file,)


input_file <- "mydata_genpop.gen"
output_file <- "output_LD_results.txt"

# Check format
validate_genepop_format(input_file)

# Run linkage disequilibrium test
test_LD(
  inputFile = mydata_genpop,
  outputFile = output_file,
  dememorization = 10000,
  batches = 100,
  iterations = 5000,
  verbose = TRUE
)

cat("Linkage disequilibrium test results written to:", output_file, "\n")