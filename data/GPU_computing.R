mat  <- mydata_hierfstat
head(mat)

n_rep=10

shuffled_matrices <- replicate(n_rep, mat[sample(nrow(mat)), ], simplify = FALSE)

# Create a list to store the wc
fst_df <- numeric(n_rep)
fis_df <- numeric(n_rep)

# Calculate the average for each shuffled matrix

# Iterate through the shuffled matrices
for (i in 1:n_rep) {
  # Calculate the statistics for the i-th matrix
  result_f_stats <- wc(shuffled_matrices[[i]])
  result_f_stats <- as.data.frame(result_f_stats$per.loc)
  # Extract FST and FIS values
  fst_values <- result_f_stats$per.loc$FST
  fis_values <- result_f_stats$per.loc$FIS
  # Assign values to the data frames
  fst_df <- cbind(fst_df, result_f_stats$FST)
  fis_df <- cbind(fis_df, result_f_stats$FIS)
}

# Set row names as in result_f_stats
rownames(fst_df) <- rownames(fis_df) <- rownames(result_f_stats$per.loc)
fst_df <- fst_df[, -1]
fis_df <-fis_df[, -1]
vec <- seq(1, n_rep)
colnames(fst_df) <- colnames(fis_df) <- vec






wc(as.data.frame(shuffled_matrices[[839]]))















# Combine the shuffled columns into a new matrix
shuffled_mat <- do.call(cbind, shuffled_matrices)






# Set the column and row names
colnames(shuffled_mat) <- colnames(mat)
rownames(shuffled_mat) <- rownames(mat)

# Print the shuffled matrix
print(shuffled_mat)





# Calculate average for each column
averages <- sapply(shuffled_matrices, function(mat) colMeans(mat))

# Create data frame with column names and iteration numbers
result <- data.frame(matrix(NA, nrow = length(names(df)), ncol = 1000))
colnames(result) <- 1:1000
rownames(result) <- names(df)

# Fill in the result data frame with average values
for (i in 1:1000) {
  result[, i] <- averages[, i]
}

#result Specify the range for random integers
min_value <- 1
max_value <- 10

# Create a matrix with random integers
mat <- matrix(sample(min_value:max_value, 7 * 7, replace = TRUE), nrow = 7, ncol = 7)

# Set the column and row names
colnames(mat) <- paste0(letters[1:7], "1")
rownames(mat) <- paste0(letters[1:7], "2")

# Print the matrix
print(mat)




