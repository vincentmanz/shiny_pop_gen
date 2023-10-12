filtered_data <- read.csv("data/data-2023-09-11 (2).csv", header = TRUE)
filtered_data

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

# Randomising alleles within samples.
mydata_hierfstat[2:7]


























