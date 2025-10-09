# boot_fit_two_stage_optimized.R
# Optimized version with better performance and memory management

library(Rcpp)
library(ggplot2)
library(data.table)  # For faster data manipulation

# ============================================================================
# OPTIMIZED CORE FUNCTIONS
# ============================================================================

# Load C++ functions once with error handling
.load_cpp_functions <- function() {
  cpp_files <- c("www/boot_per_locus_two_stage_wc.cpp", 
                 "www/boot_wc_pop_only_fast.cpp")
  
  for (file in cpp_files) {
    if (file.exists(file)) {
      tryCatch({
        Rcpp::sourceCpp(file)
      }, error = function(e) {
        warning("Failed to load ", file, ": ", e$message)
      })
    } else {
      warning("File not found: ", file)
    }
  }
}

# Initialize on load
.load_cpp_functions()

# ============================================================================
# OPTIMIZED DATA PREPARATION
# ============================================================================

# Fast integer encoding with data.table
.prepare_data_fast <- function(hf) {
  if (!is.data.frame(hf)) stop("Input must be a data.frame")
  
  # Use data.table for efficient operations
  if (!data.table::is.data.table(hf)) {
    hf_dt <- data.table::as.data.table(hf)
  } else {
    hf_dt <- hf
  }
  
  # Ensure first column is 'pop'
  original_names <- copy(names(hf_dt))
  if (names(hf_dt)[1] != "pop") {
    data.table::setnames(hf_dt, 1, "pop")
  }
  
  # Fast integer conversion
  hf_dt[, pop := as.integer(as.factor(pop))]
  
  # Convert locus columns to integer efficiently
  locus_cols <- setdiff(names(hf_dt), "pop")
  for (col in locus_cols) {
    if (!is.integer(hf_dt[[col]])) {
      hf_dt[, (col) := as.integer(get(col))]
    }
  }
  
  # Remove missing values efficiently
  hf_dt <- hf_dt[complete.cases(hf_dt), ]
  
  return(list(
    data = hf_dt,
    original_names = original_names,
    locus_names = locus_cols
  ))
}

# ============================================================================
# OPTIMIZED BOOTSTRAP WRAPPERS
# ============================================================================

# Fast quantile extraction
.extract_quantiles_fast <- function(QxL, probs, locus_names) {
  rn <- as.numeric(rownames(QxL))
  indices <- vapply(probs, function(p) which.min(abs(rn - p)), integer(1))
  result <- QxL[indices, , drop = FALSE]
  colnames(result) <- locus_names
  return(result)
}

# Fast table builder using data.table
.build_table_fast <- function(QxL, stat, obs_vec, locus_names, digits, alpha) {
  fmt <- function(x) formatC(x, digits = digits, format = "f", digits = digits)
  
  # Extract quantiles efficiently
  rn <- as.numeric(rownames(QxL))
  i_lo <- which.min(abs(rn - alpha))
  i_md <- which.min(abs(rn - 0.5))
  i_hi <- which.min(abs(rn - (1 - alpha)))
  
  data.table::data.table(
    `Locus names` = locus_names,
    Observed = fmt(obs_vec),
    Statistic = fmt(QxL[i_md, ]),
    `95%CI-i` = fmt(QxL[i_lo, ]),
    `95%CI-s` = fmt(QxL[i_hi, ])
  ) %>% data.table::setnames("Statistic", stat)
}

# Fast overall row creation
.make_overall_row_fast <- function(qvec, stat, digits, observed_scalar) {
  fmt <- function(x) formatC(x, digits = digits, format = "f", digits = digits)
  data.table::data.table(
    `Locus names` = "All",
    Observed = fmt(observed_scalar),
    Statistic = fmt(qvec[2]),
    `95%CI-i` = fmt(qvec[1]),
    `95%CI-s` = fmt(qvec[3])
  ) %>% data.table::setnames("Statistic", stat)
}

# ============================================================================
# OPTIMIZED MAIN WRAPPERS
# ============================================================================

#' Fast Two-Stage Bootstrap Confidence Intervals
#' 
#' @param hf Input data frame with 'pop' as first column
#' @param conf Confidence level (default: 0.95)
#' @param digits Number of digits for formatting (default: 6)
#' @param n_boot Number of bootstrap replicates (default: 1000)
#' @param seed Random seed (default: 42)
#' @param n_threads Number of threads for parallel computation
#' @return List with bootstrap results for FST, FIT, HS, HI
wc_bootstrap_ci_optimized <- function(hf,
                                     conf = 0.95,
                                     digits = 6,
                                     n_boot = 1000,
                                     seed = 42L,
                                     n_threads = max(1L, parallel::detectCores() - 1L)) {
  
  # Prepare data
  prepared <- .prepare_data_fast(hf)
  hf_dt <- prepared$data
  locus_names <- prepared$locus_names
  
  # Convert to integer matrix efficiently
  mat_int <- as.matrix(hf_dt)
  storage.mode(mat_int) <- "integer"
  pop_col <- 0L  # First column after conversion
  
  # Compute observed statistics
  obs_cpp <- wc_per_locus_observed_cpp(mat_int, pop_col, 0L, n_threads)
  
  # Extract observed values efficiently
  obs <- list(
    FST = as.numeric(obs_cpp$FST),
    FIT = as.numeric(obs_cpp$FIT),
    HS  = as.numeric(obs_cpp$HS),
    HI  = as.numeric(obs_cpp$HI)
  )
  
  obs_overall <- list(
    FST = as.numeric(obs_cpp$FST_overall),
    FIT = as.numeric(obs_cpp$FIT_overall),
    HS  = as.numeric(obs_cpp$HS_overall),
    HI  = as.numeric(obs_cpp$HI_overall)
  )
  
  # Bootstrap quantiles
  alpha <- (1 - conf) / 2
  probs <- c(alpha, 0.5, 1 - alpha)
  
  res <- boot_wc_two_stage_fast_cpp(
    mat_int = mat_int,
    colnames = names(hf_dt),
    pop_col = pop_col,
    missing_code = 0L,
    B = as.integer(n_boot),
    probs = probs,
    seed = as.integer(seed),
    n_threads = as.integer(n_threads)
  )
  
  # Build tables efficiently
  build_stat_table <- function(stat) {
    QxL <- res[[stat]]
    colnames(QxL) <- locus_names
    
    # Per-locus table
    tbl <- .build_table_fast(QxL, stat, obs[[stat]], locus_names, digits, alpha)
    
    # Add overall row
    ov_row <- .make_overall_row_fast(
      as.numeric(res[[paste0(stat, "_overall")]]), 
      stat, digits, obs_overall[[stat]]
    )
    
    rbind(tbl, ov_row)
  }
  
  tbl_FST <- build_stat_table("FST")
  tbl_FIT <- build_stat_table("FIT")
  tbl_HS <- build_stat_table("HS")
  tbl_HI <- build_stat_table("HI")
  
  # Create combined long format efficiently
  to_long_fast <- function(tbl, stat_name) {
    data.table::data.table(
      `Locus names` = tbl[["Locus names"]],
      Stat = stat_name,
      Observed = tbl[["Observed"]],
      Estimate = tbl[[stat_name]],
      `95%CI-i` = tbl[["95%CI-i"]],
      `95%CI-s` = tbl[["95%CI-s"]]
    )
  }
  
  combined <- rbind(
    to_long_fast(tbl_FST, "FST"),
    to_long_fast(tbl_FIT, "FIT"),
    to_long_fast(tbl_HS, "HS"),
    to_long_fast(tbl_HI, "HI")
  )
  
  # Convert to data.frame for compatibility
  list(
    FST = as.data.frame(tbl_FST),
    FIT = as.data.frame(tbl_FIT),
    HS = as.data.frame(tbl_HS),
    HI = as.data.frame(tbl_HI),
    combined = as.data.frame(combined),
    meta = list(
      conf = conf, 
      digits = digits, 
      n_boot = n_boot, 
      seed = seed, 
      n_threads = n_threads
    )
  )
}

#' Fast Population-Only Bootstrap Confidence Intervals
wc_bootstrap_ci_poponly_optimized <- function(hf,
                                             conf = 0.95,
                                             digits = 6,
                                             n_boot = 1000,
                                             seed = 43L,
                                             n_threads = max(1L, parallel::detectCores() - 1L)) {
  
  # Reuse data preparation
  prepared <- .prepare_data_fast(hf)
  hf_dt <- prepared$data
  locus_names <- prepared$locus_names
  
  mat_int <- as.matrix(hf_dt)
  storage.mode(mat_int) <- "integer"
  pop_col <- 0L
  
  # Observed statistics (same as two-stage)
  obs_cpp <- wc_per_locus_observed_cpp(mat_int, pop_col, 0L, n_threads)
  
  obs <- list(
    FST = as.numeric(obs_cpp$FST),
    FIT = as.numeric(obs_cpp$FIT),
    HS  = as.numeric(obs_cpp$HS),
    HI  = as.numeric(obs_cpp$HI)
  )
  
  obs_overall <- list(
    FST = as.numeric(obs_cpp$FST_overall),
    FIT = as.numeric(obs_cpp$FIT_overall),
    HS  = as.numeric(obs_cpp$HS_overall),
    HI  = as.numeric(obs_cpp$HI_overall)
  )
  
  # Population-only bootstrap
  alpha <- (1 - conf) / 2
  probs <- c(alpha, 0.5, 1 - alpha)
  
  res <- boot_wc_pop_only_fast_cpp(
    mat_int = mat_int,
    colnames = names(hf_dt),
    pop_col = pop_col,
    missing_code = 0L,
    B = as.integer(n_boot),
    probs = probs,
    seed = as.integer(seed),
    n_threads = as.integer(n_threads)
  )
  
  # Reuse table building logic
  build_stat_table <- function(stat) {
    QxL <- res[[stat]]
    colnames(QxL) <- locus_names
    
    tbl <- .build_table_fast(QxL, stat, obs[[stat]], locus_names, digits, alpha)
    
    ov_row <- .make_overall_row_fast(
      as.numeric(res[[paste0(stat, "_overall")]]), 
      stat, digits, obs_overall[[stat]]
    )
    
    rbind(tbl, ov_row)
  }
  
  tbl_FST <- build_stat_table("FST")
  tbl_FIT <- build_stat_table("FIT")
  tbl_HS <- build_stat_table("HS")
  tbl_HI <- build_stat_table("HI")
  
  # Combined table
  to_long_fast <- function(tbl, stat_name) {
    data.table::data.table(
      `Locus names` = tbl[["Locus names"]],
      Stat = stat_name,
      Observed = tbl[["Observed"]],
      Estimate = tbl[[stat_name]],
      `95%CI-i` = tbl[["95%CI-i"]],
      `95%CI-s` = tbl[["95%CI-s"]]
    )
  }
  
  combined <- rbind(
    to_long_fast(tbl_FST, "FST"),
    to_long_fast(tbl_FIT, "FIT"),
    to_long_fast(tbl_HS, "HS"),
    to_long_fast(tbl_HI, "HI")
  )
  
  list(
    FST = as.data.frame(tbl_FST),
    FIT = as.data.frame(tbl_FIT),
    HS = as.data.frame(tbl_HS),
    HI = as.data.frame(tbl_HI),
    combined = as.data.frame(combined),
    meta = list(
      conf = conf, 
      digits = digits, 
      n_boot = n_boot, 
      seed = seed, 
      n_threads = n_threads
    )
  )
}

# ============================================================================
# OPTIMIZED VISUALIZATION
# ============================================================================

#' Fast data extraction for plotting
.extract_stat_tbl_fast <- function(x, stat) {
  df <- x[[stat]]
  df <- df[df[["Locus names"]] != "All", ]
  
  # Efficient numeric conversion
  to_num <- function(v) {
    if (is.character(v)) {
      as.numeric(gsub(",", ".", v))
    } else {
      as.numeric(v)
    }
  }
  
  data.table::data.table(
    Locus = df[["Locus names"]],
    Observed = to_num(df[["Observed"]]),
    Median = to_num(df[[stat]]),
    LCL = to_num(df[["95%CI-i"]]),
    UCL = to_num(df[["95%CI-s"]])
  )
}

#' Optimized Forest Plot
make_forest_plot_optimized <- function(two_stage, pop_only, stat = "FST",
                                      clamp0 = FALSE, 
                                      order_by = c("two-stage", "pop-only", "mean")) {
  
  order_by <- match.arg(order_by)
  
  # Fast data extraction
  df_two <- .extract_stat_tbl_fast(two_stage, stat)
  df_pop <- .extract_stat_tbl_fast(pop_only, stat)
  
  df_two[, Method := "Two-stage"]
  df_pop[, Method := "Pop-only"]
  
  df <- rbind(df_two, df_pop)
  
  if (clamp0) df[, LCL := pmax(LCL, 0)]
  
  # Efficient ordering
  ord_vec <- switch(order_by,
    "two-stage" = setNames(df_two$Median, df_two$Locus),
    "pop-only" = setNames(df_pop$Median, df_pop$Locus),
    "mean" = df[, .(mean_median = mean(Median, na.rm = TRUE)), by = Locus]$mean_median
  )
  
  locus_order <- names(sort(ord_vec, decreasing = FALSE))
  df[, Locus := factor(Locus, levels = locus_order)]
  
  # Optimized ggplot
  pd <- position_dodge(width = 0.6)
  
  ggplot(df, aes(x = Locus, y = Median, color = Method, fill = Method)) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0, 
                  position = pd, alpha = 0.6, linewidth = 0.8) +
    geom_point(position = pd, size = 2.5, alpha = 0.8) +
    geom_point(aes(y = Observed), position = pd, 
               shape = 21, color = "black", fill = "white", 
               size = 2.2, stroke = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
    coord_flip() +
    labs(
      title = paste0(stat, " per locus: observed (hollow) vs. 95% CI"),
      subtitle = "Comparison of bootstrap methods",
      x = "Locus", 
      y = stat,
      color = "Method",
      fill = "Method"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 9)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1")
}

# ============================================================================
# OPTIMIZED COMPARISON FUNCTION
# ============================================================================

#' Fast comparison of bootstrap methods
compare_bootstrap_methods <- function(hf, n_boot = 1000, 
                                     conf = 0.95, digits = 6) {
  
  # Run both methods in sequence
  two_stage <- wc_bootstrap_ci_optimized(
    hf, conf = conf, digits = digits, n_boot = n_boot, seed = 42L
  )
  
  pop_only <- wc_bootstrap_ci_poponly_optimized(
    hf, conf = conf, digits = digits, n_boot = n_boot, seed = 43L
  )
  
  # Create comparison plots
  plots <- list(
    FST = make_forest_plot_optimized(two_stage, pop_only, "FST", clamp0 = TRUE),
    FIT = make_forest_plot_optimized(two_stage, pop_only, "FIT", clamp0 = TRUE),
    HS = make_forest_plot_optimized(two_stage, pop_only, "HS", clamp0 = TRUE),
    HI = make_forest_plot_optimized(two_stage, pop_only, "HI", clamp0 = TRUE)
  )
  
  return(list(
    two_stage = two_stage,
    pop_only = pop_only,
    plots = plots
  ))
}

# ============================================================================
# USAGE EXAMPLE
# ============================================================================

# Example usage (commented out)
# results <- compare_bootstrap_methods(hf = mydata_hierfstat_a, n_boot = 1000)
# results$plots$FST  # View FST comparison plot
# results$two_stage$FST  # View two-stage FST results