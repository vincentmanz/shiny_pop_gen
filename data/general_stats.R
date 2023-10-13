filtered_data <- read.csv("data/data-2023-09-11 (2).csv", header = TRUE)


selected_stats <- c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)"
)


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


n_rep = 100

library(parallel)
library(dplyr)


# Number of CPU cores to use
num_cores <- detectCores()

# Set result storage for each statistic in each worker
result_FST <- numeric(n_rep)
result_FIS <- numeric(n_rep)

# Parallel loop using mclapply
results <- mclapply(1:n_rep, mc.cores = num_cores, function(i) {
  set.seed(i)  # Set a different seed for each worker
  wc_results_FST <- numeric(n_rep)  # Create a result vector for each worker
  wc_results_FIS <- numeric(n_rep)  # Create a result vector for each worker
  
  for (j in 1:n_rep) {
    library(dplyr)
    
    # Shuffle the data in columns 3 to 7
    shuffled_data <- mydata_hierfstat
    shuffled_data[, 3:7] <- mydata_hierfstat[, sample(3:7)]
    
    # Compute the wc (replace 'wc_function' with the actual function you want to use)
    wc_result <- wc(shuffled_data)
    wc_result <- as.data.frame(wc_result$per.loc)
    
    # Store the FST and FIS results
    wc_result_FST <-  wc_result %>% select(FST)
    wc_result_FIS <- wc_result %>% select(FIS)
  }
  
  result_FST <- wc_result_FST
  result_FIS <- wc_result_FIS
  
  return(list(FST = result_FST, FIS = result_FIS))
})

# Combine results into data frames
result_FST <- data.frame(do.call(cbind, lapply(results, function(x) x$FST)))
result_FIS <- data.frame(do.call(cbind, lapply(results, function(x) x$FIS)))

# Set row names
rownames(result_FST) <- rownames(wc_result)
rownames(result_FIS) <- rownames(wc_result)

# Print the result data frames
#print(result_FST)
#print(result_FIS)

#parLapply: 84.537 sec elapsed
#mclapply: 75.443 sec elapsed

# Define the reference FST values
reference_FST <- result_f_stats$FST
# Calculate the percentage of times shuffled FST values are greater
percentage_greater <- round(rowMeans(result_FST > reference_FST) * 100, 3)

# Format the percentages with exactly three decimal places
formatted_percentages <- sprintf("%.3f", percentage_greater)

# Create a data frame with the formatted percentages
percentage_data <- data.frame(Percentage = formatted_percentages)

# Print the data frame
print(percentage_data)
