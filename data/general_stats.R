
### Parameters 

filtered_data <- read.csv("data/data-2023-09-11 (2).csv", header = TRUE)


selected_stats <- c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)"
)


n_rep=10

sequence_length <- length(6:11) 

######

filtered_data <- filtered_data
filtered_data <- data.frame(indv = paste(substr(filtered_data$Population,1,3), row.names(filtered_data), sep="."), filtered_data)
# Create mydata_genind
population <- filtered_data$Population
mydata_genind <- df2genind(
  X = filtered_data[,6:11],
  sep = "/",
  ncode = 6,
  ind.names = filtered_data$indv,
  pop = filtered_data$Population,
  NA.char = "0/0",
  ploidy = 2,
  type = "codom",
  strata = NULL,
  hierarchy = NULL
)
mydata_genind
mydata_hierfstat <- genind2hierfstat(mydata_genind)

# Run basic.stats and render the result
result <- basic.stats(mydata_hierfstat)
df_resutl_basic<-as.data.frame(result$perloc)



# Weir and Cockrham estimates of Fstatistics - FIS and FST 

result_f_stats <- wc(mydata_hierfstat)
result_f_stats <- as.data.frame(result_f_stats$per.loc)
colnames(result_f_stats) <- c("Fis (W&C)", "Fst (W&C)")
result_f_stats <- merge(result_f_stats, df_resutl_basic,by="row.names",all.x=TRUE)
colnames(result_f_stats)[10] <- "Fst (Nei)"
colnames(result_f_stats)[12] <- "Fis (Nei)"

result_f_stats_selec <- result_f_stats %>% select(all_of(selected_stats))
mat  <- mydata_hierfstat[,2:7]
level_pop <- mydata_hierfstat[,1]
head(mat)



#shuffled_matrices <- replicate(n_rep, mat[sample(nrow(mat)), ], simplify = FALSE)
shuffled_matrices <- replicate(n_rep, mat[sample(length(mat), replace = FALSE)], simplify = FALSE)
##################
# shuffle only the genotype and add the pop column later for each matrices. 
#in the loop? 
###############


# Create a list to store the wc
fst_df <- numeric(sequence_length)
fis_df <- numeric(sequence_length)

# Calculate the average for each shuffled matrix

# Iterate through the shuffled matrices
for (i in 1:n_rep) {
  # Calculate the statistics for the i-th matrix
  #HERE THE COLUMN POP
  merged_df <- cbind(level_pop, shuffled_matrices[[i]])
  result_f_stats <- wc(shuffled_matrices[[i]]) ######################################## Error in 1:sum(data[, 1] == i) : NA/NaN argument
  result_f_stats <- as.data.frame(result_f_stats$per.loc)
  # Extract FST and FIS values
  fst_values <- result_f_stats$FST
  fis_values <- result_f_stats$FIS
  print( fst_values)
  # Assign values to the data frames
  fst_df <- cbind(fst_df, result_f_stats$FST)
  fis_df <- cbind(fis_df, result_f_stats$FIS)
}

# Set row names as in result_f_stats

rownames(fst_df) <- rownames(fis_df) <- rownames(result_f_stats)
result_FST <- fst_df[, -1]
fis_df <-fis_df[, -1]
vec <- seq(1, n_rep)
colnames(result_FST) <- colnames(fis_df) <- vec


result_FST[1,] 

count (result_f_stats[,1][1] > result_FST[1,] )
count <- sum(result_f_stats[,1][1] > result_FST[1, ])



# Initialize an empty data frame to store the counts
count_df <- data.frame(
  Greater = numeric(length(result_FST)),
  Smaller = numeric(length(result_FST))
)

# Compare the values in result_f_stats[1] to result_FST for each column
for (col in colnames(result_FST)) {
  greater_count <- sum(result_f_stats[1] > result_FST[, col])
  smaller_count <- sum(result_f_stats[1] < result_FST[, col])
  count_df$Greater[col] <- greater_count
  count_df$Smaller[col] <- smaller_count
}

# Print the count data frame
print(count_df)

