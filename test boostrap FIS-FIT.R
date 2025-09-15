## ---- Setup ----
# Packages you need
library(adegenet)
library(hierfstat)
library(pegas)

# Load formatted data (expects formatted_data with $haplotype as "aaa/bbb")
load("data/formatted_data.RData")

## Basic metadata
loci_names <- colnames(formatted_data$haplotype)
n_loci     <- length(loci_names)
n_perm     <- 100
set.seed(1)

## ---- Build genind (slash format) and hierfstat (numeric) ----
# genind from original "aaa/bbb"
mydata_genind <- adegenet::df2genind(
  X         = as.matrix(formatted_data$haplotype),
  sep       = "/",
  ncode     = 3,   # 3 digits per allele
  ind.names = formatted_data$individual,
  pop       = formatted_data$Population,
  NA.char   = formatted_data$missing_code,
  ploidy    = 2,
  type      = "codom"
)

# hierfstat numeric from genind (pop in column 1, loci coded a1a2 as 6-digit ints)
mydata_hierfstat_a <- hierfstat::genind2hierfstat(mydata_genind)

## ---- Helpers (encoding + permutations) ----
.decode_gt <- function(gt) {
  if (is.na(gt) || gt == 0) return(NULL)
  a1 <- floor(gt / 1000); a2 <- gt %% 1000
  if (a1 <= 0 || a2 <= 0) return(NULL)
  c(a1, a2)
}

.encode_gt <- function(a1, a2) a1 * 1000 + a2

.allele_pool <- function(geno_vec) {
  alleles <- integer(0); valid_idx <- integer(0)
  for (i in seq_along(geno_vec)) {
    pair <- .decode_gt(geno_vec[i])
    if (!is.null(pair)) { alleles <- c(alleles, pair); valid_idx <- c(valid_idx, i) }
  }
  list(alleles = alleles, valid_idx = unique(valid_idx))
}

assign_shuffled <- function(geno_vec, shuffled, valid_idx) {
  out <- geno_vec; k <- 1
  for (i in valid_idx) {
    if (k + 1 <= length(shuffled)) {
      out[i] <- .encode_gt(shuffled[k], shuffled[k + 1]); k <- k + 2
    } else out[i] <- NA
  }
  out
}

# FIS null: shuffle alleles within each pop × locus
permute_alleles_within_pops <- function(hierfstat_data) {
  stopifnot(ncol(hierfstat_data) >= 2)
  out <- hierfstat_data
  pops <- unique(hierfstat_data[, 1])
  loci_idx <- 2:ncol(hierfstat_data)
  for (j in loci_idx) {
    for (p in pops) {
      rows <- which(hierfstat_data[, 1] == p)
      if (length(rows) <= 1) next
      pool <- .allele_pool(hierfstat_data[rows, j])
      if (length(pool$alleles) >= 2 && length(pool$valid_idx) >= 1) {
        out[rows, j] <- assign_shuffled(
          hierfstat_data[rows, j],
          shuffled = sample(pool$alleles),
          valid_idx = pool$valid_idx  # relative indices (correct)
        )
      }
    }
  }
  out
}

# Robust: convert hierfstat numeric to genind via "aaa/bbb"
hierf_to_genind_slash <- function(hier_df, pop_col = 1, missing_code = 0) {
  stopifnot(ncol(hier_df) >= 2)
  pop <- hier_df[[pop_col]]
  X   <- hier_df[-pop_col, drop = FALSE]
  X[X == missing_code] <- NA
  X[] <- lapply(X, function(v) ifelse(is.na(v), NA, sprintf("%06d", as.integer(v))))
  Xslash <- as.data.frame(lapply(X, function(s) {
    ifelse(is.na(s), NA, paste0(substr(s, 1, 3), "/", substr(s, 4, 6)))
  }), stringsAsFactors = FALSE)
  indn <- rownames(hier_df); if (is.null(indn)) indn <- paste0("ind", seq_len(nrow(hier_df)))
  adegenet::df2genind(
    X         = as.matrix(Xslash),
    sep       = "/",
    ncode     = 3,
    ind.names = indn,
    pop       = pop,
    NA.char   = NA,
    ploidy    = 2,
    type      = "codom"
  )
}

p_two <- function(obs, perm_vec) {
  v <- perm_vec[!is.na(perm_vec)]
  if (length(v) == 0 || is.na(obs)) return(NA_real_)
  min(1, 2 * min(mean(v >= obs), mean(v <= obs)))
}

## ---- Observed (W&C) FIS per locus ----
obs_wc <- pegas::Fst(pegas::as.loci(mydata_genind))  
obs_fis_wc_by_locus <- obs_wc[, "Fis"]
# align to loci order
obs_fis_wc_by_locus <- obs_fis_wc_by_locus[match(loci_names, names(obs_fis_wc_by_locus))]

## ---- Permutations: local panmixia (FIS null) ----
perm_fis_by_locus <- matrix(NA_real_, n_perm, n_loci, dimnames = list(NULL, loci_names))

for (perm_i in seq_len(n_perm)) {
  perm_data   <- permute_alleles_within_pops(mydata_hierfstat_a)
  perm_genind <- hierf_to_genind_slash(perm_data, pop_col = 1, missing_code = 0)
  perm_wc     <- pegas::Fst(pegas::as.loci(perm_genind))
  fis_vec     <- perm_wc[, "Fis"]
  fis_vec     <- fis_vec[match(loci_names, names(fis_vec))]
  perm_fis_by_locus[perm_i, ] <- fis_vec
}

## ---- P-values (per locus + overall) ----
pval_fis_by_locus <- sapply(seq_along(loci_names), function(j) {
  p_two(obs_fis_wc_by_locus[j], perm_fis_by_locus[, j])
})
names(pval_fis_by_locus) <- loci_names

obs_fis_wc_overall <- mean(obs_fis_wc_by_locus, na.rm = TRUE)
perm_fis_overall   <- apply(perm_fis_by_locus, 1, function(v) mean(v, na.rm = TRUE))
pval_fis_overall   <- p_two(obs_fis_wc_overall, perm_fis_overall)

## ---- Results table ----
fis_perm_result <- data.frame(
  Locus           = loci_names,
  Observed_FIS_WC = round(obs_fis_wc_by_locus, 6),
  P_value         = round(pval_fis_by_locus, 6),
  stringsAsFactors = FALSE
)

print(head(fis_perm_result))
cat("Overall FIS (WC):", round(obs_fis_wc_overall, 6),
    "  p-value:", round(pval_fis_overall, 6), "\n")

## ---- Optional diagnostic plot ----
# hist(perm_fis_overall, main = "Null distribution (overall FIS, W&C)",
#      xlab = "overall FIS (permutation)", breaks = 30)
# abline(v = obs_fis_wc_overall, lwd = 2)









## ==== BOOTSTRAP CI FOR W&C FIS ====

# 1) Multilocus (overall) FIS CI by bootstrapping loci
#    - Resample loci with replacement; compute mean FIS each time.
boot_fis_loci <- function(fis_by_locus, B = 1000, conf = 0.95) {
  fis_by_locus <- fis_by_locus[!is.na(fis_by_locus)]
  if (length(fis_by_locus) == 0) return(list(mean = NA, lower = NA, upper = NA, dist = NA))
  nL <- length(fis_by_locus)
  dist <- replicate(B, mean(sample(fis_by_locus, nL, replace = TRUE), na.rm = TRUE))
  alpha <- (1 - conf) / 2
  list(
    mean  = mean(fis_by_locus, na.rm = TRUE),
    lower = unname(quantile(dist, probs = alpha,        na.rm = TRUE)),
    upper = unname(quantile(dist, probs = 1 - alpha,    na.rm = TRUE)),
    dist  = dist
  )
}

# Example: 95% CI for overall FIS (WC) via loci bootstrap
bootL_fis95 <- boot_fis_loci(obs_fis_wc_by_locus, B = 2000, conf = 0.95)
bootL_fis99 <- boot_fis_loci(obs_fis_wc_by_locus, B = 2000, conf = 0.99)

cat(sprintf(
  "Bootstrap (loci) overall FIS (WC): mean=%.6f  95%% CI [%.6f, %.6f]  99%% CI [%.6f, %.6f]\n",
  bootL_fis95$mean, bootL_fis95$lower, bootL_fis95$upper, bootL_fis99$lower, bootL_fis99$upper
))

# 2) Optional: Block bootstrap over populations (resample whole populations)
#    - Each replicate: sample populations with replacement, rebuild dataset,
#      recompute W&C FIS per locus, take the multilocus mean.
hierf_to_genind_slash <- hierf_to_genind_slash  # reuse your helper

boot_fis_popblocks <- function(hier_df, B = 1000, conf = 0.95, missing_code = 0) {
  pops <- unique(hier_df[,1])
  if (length(pops) < 2) return(list(mean = NA, lower = NA, upper = NA, dist = NA))
  dist <- numeric(B)
  for (b in seq_len(B)) {
    samp_pops <- sample(pops, length(pops), replace = TRUE)
    boot_df <- do.call(rbind, lapply(samp_pops, function(p) hier_df[hier_df[,1] == p, , drop = FALSE]))
    boot_gen <- hierf_to_genind_slash(boot_df, pop_col = 1, missing_code = missing_code)
    wc       <- pegas::Fst(pegas::as.loci(boot_gen))
    dist[b]  <- mean(wc[, "Fis"], na.rm = TRUE)
  }
  alpha <- (1 - conf) / 2
  list(
    mean  = mean(dist, na.rm = TRUE),                 # bootstrap mean
    lower = unname(quantile(dist, probs = alpha,     na.rm = TRUE)),
    upper = unname(quantile(dist, probs = 1 - alpha, na.rm = TRUE)),
    dist  = dist
  )
}

# Example: 95% & 99% CI via population-block bootstrap
set.seed(2)
bootP_fis95 <- boot_fis_popblocks(mydata_hierfstat_a, B = 1000, conf = 0.95, missing_code = 0)
bootP_fis99 <- boot_fis_popblocks(mydata_hierfstat_a, B = 1000, conf = 0.99, missing_code = 0)

cat(sprintf(
  "Bootstrap (pop blocks) overall FIS (WC): mean=%.6f  95%% CI [%.6f, %.6f]  99%% CI [%.6f, %.6f]\n",
  bootP_fis95$mean, bootP_fis95$lower, bootP_fis95$upper, bootP_fis99$lower, bootP_fis99$upper
))

## Optional: package both CI styles into a tidy table
fis_boot_summary <- data.frame(
  Method  = c("Loci bootstrap", "Population blocks"),
  Mean    = c(bootL_fis95$mean, bootP_fis95$mean),
  CI_95_L = c(bootL_fis95$lower, bootP_fis95$lower),
  CI_95_U = c(bootL_fis95$upper, bootP_fis95$upper),
  CI_99_L = c(bootL_fis99$lower, bootP_fis99$lower),
  CI_99_U = c(bootL_fis99$upper, bootP_fis99$upper)
)
print(fis_boot_summary)