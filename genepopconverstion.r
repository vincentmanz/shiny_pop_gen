library(genepopedit)
library(dplyr)
library(genepop)

#' @title Convert to Genepop format from a flattened dataframe.
#' @description Function returns Genepop file meta-data with population column support.
#' @param df dataframe with the first column holding sampleIDs (e.g. BON_01), 
#' the second column holding population names, and the remaining columns holding loci. 
#' Column names of loci will be used as loci names in the Genepop output.
#' @param path the filepath and filename of output.
#' @rdname genepop_unflatten
#' @importFrom utils write.table
#' @export
#'
genepop_unflatten <- function(df, path) {

  # Ensure all loci are characters, not factors
  df <- as.data.frame(apply(df, 2, as.character), stringsAsFactors = FALSE)

  # Extract sample names and population names
  sample_ids <- df[, 1]
  populations <- df[, 2]
  loci_data <- df[, 3:ncol(df)]

  # Ensure population names are consistent with ordering
  order_indices <- order(populations)
  sample_ids <- sample_ids[order_indices]
  populations <- populations[order_indices]
  loci_data <- loci_data[order_indices, ]

  # Prepare loci data for Genepop format
  loci_lines <- apply(loci_data, 1, function(row) paste(row, collapse = " "))
  loci_lines <- paste0(sample_ids, " , ", loci_lines)

  # Insert "POP" labels for each population
  unique_pops <- unique(populations)
  loci_with_pops <- c()
  for (pop in unique_pops) {
    loci_with_pops <- c(loci_with_pops, "POP", loci_lines[populations == pop])
  }

  # Prepare output with Genepop metadata
  output <- c(
    "No STACKS version specified",
    colnames(df)[3:ncol(df)],  # Loci names
    loci_with_pops
  )

  # Write to file
  utils::write.table(output, path, col.names = FALSE, row.names = FALSE, quote = FALSE)
}















#1 read csv
data <- read.csv("data/data-2023-09-11 (2).csv")
colnames(data)[1] <- "SampleID"


data <- data %>% select(-Latitude, -Longitude) 
head(data)

# Transform to Genepop format
genepop_file <- genepop_unflatten(data, path = paste0("GenePop_UNFLATTENED.txt"))


test_LD(
"GenePop_UNFLATTENED.txt",
outputFile = "GenePop_UNFLATTENED.genepop_out.txt",
settingsFile = "",
dememorization = 10000,
batches = 100,
iterations = 5000)

locinfile <- genepopExample('sample.txt')
test_LD(locinfile,'sample.txt.DIS')


write_LD_tables("GenePop_UNFLATTENED.txt", 'sample.txt.TAB')


