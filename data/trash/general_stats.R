


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

result_f_stats <- Fst(as.loci(mydata_genind))
result_f_stats <- result_f_stats[,2:3]
colnames(result_f_stats) <- c("Fst (W&C)", "Fis (W&C)")
result_f_stats <- merge(result_f_stats, df_resutl_basic, by="row.names",all.x=TRUE)
colnames(result_f_stats)[10] <- "Fst (Nei)"
colnames(result_f_stats)[12] <- "Fis (Nei)"
result_f_stats <- result_f_stats %>% column_to_rownames(., var = 'Row.names')
result_f_stats_selec <- result_f_stats %>% select(all_of(selected_stats))













######################## Missing data ######################## 
missing_data <- info_table(mydata_genind, plot = FALSE)


# Libraries
library(heatmaply)



# Matrix format
mat <- as.matrix(missing_data)
# heatmap
p <- heatmaply(mat, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Population", "Marker", "Value"),
               fontsize_row = 8, fontsize_col = 8,
               labCol = colnames(mat),
               labRow = rownames(mat),
               heatmap_layers = theme(axis.line=element_blank())
)
p



########################################################################## DEV ########################################################################## 

######################## HW - Panmixia ######################## 

library("pegas")

hw.test(as.loci(mydata_genind), B = 1000)


######################## Linkage Disequilibrium ######################## 

LD(as.loci(mydata_genind$tab), locus = c(1, 2), details = TRUE)

a<-LDscan(as.loci(mydata_hierfstat[,2:7]))

LDmap(a)


head(mydata_hierfstat[,2:7])


library("poppr")
pair.ia(mydata_genind, sample = 9)



######################## shuffle df ######################## 


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
  result_f_stats <- wc(shuffled_matrices[[i]]) 
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

######################## ######################## 