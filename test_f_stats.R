
#test boot 
# 
# CSV_BOU <- read.csv("data/filtered_data.csv")
# formatted_data <- list(
#   Population   = formatted_data$Population,
#   individual   = paste0("Ind", "_", substr(formatted_data$Population, 1, 3), "_", row.names(filtered_data)),
#   ploidy       = 2,
#   haplotype    = as.data.frame(CSV_BOU[, 5:10]),   # subset cols 5–10
#   marker       = formatted_data$marker,
#   missing_code = "0/0",
#   GPS          = formatted_data$GPS,
#   level1       = formatted_data$level1,
#   level2       = formatted_data$level2,
#   level3       = formatted_data$level3
# )
# # 
# save(formatted_data, file = "data/formatted_data.RData")

# Load the saved RData object
load("data/formatted_data.RData") 

df_formated <- data.frame(
  Population = formatted_data$Population,
  formatted_data$GPS,
  formatted_data$level1,
  formatted_data$level2,
  formatted_data$level3,
  formatted_data$haplotype,
  stringsAsFactors = FALSE
)

# Basic metadata
n_marker <- ncol(formatted_data$haplotype)
n_pop <- length(unique(formatted_data$Population))
n_indv <- nrow(formatted_data)
pops <- unique(formatted_data$Population)

# Genotype columns
column_genotype_start <- 1 
column_genotype_end <- n_marker

# Missing data code
missing_data_code <-formatted_data$missing_code

# Compute genind object
mydata_genind <- adegenet::df2genind(X = as.matrix(formatted_data$haplotype), sep = "/", ncode = 6, ind.names = formatted_data$individual, pop = formatted_data$Population, NA.char = formatted_data$missing_code, ploidy = formatted_data$ploidy, type = "codom", strata = NULL, hierarchy = NULL)
# Compute hierfstat object
mydata_hierfstat <- genind2hierfstat(mydata_genind)

# Run basic.stats and render the result 
result <- basic.stats(mydata_hierfstat)
df_result_basic <- as.data.frame(result$perloc)



# Compute Weir and Cockerham F-statistics
data <- as.data.frame(as.loci(mydata_genind))
result_f_stats <- Fst(as.loci(data))
colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")

# Compute G statistics
df_result_basic <- df_result_basic %>% mutate(
  GST = 1 - Hs / Ht,
  `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
)
