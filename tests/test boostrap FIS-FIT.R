# ___ test bootstrap FIS - Version optimisée ________________________________
# Purpose:
#   Compute Weir & Cockerham FIS per locus (and multilocus mean),
#   test significance via a population-structured permutation (local panmixia),
#   and quantify uncertainty via two bootstraps:
#     (i) per-locus CI: resample INDIVIDUALS within each population
#     (s) overall CI: resample POPULATIONS as blocks (population-block bootstrap).
#
# Key improvements:
#   - Better error handling and validation
#   - Memory optimization for large datasets
#   - Parallel processing improvements
#   - Cleaner code structure
# __________________________________________________________________________

## ---- Setup et configuration ----
library(adegenet)
library(hierfstat)
library(pegas)
library(Rcpp)
library(parallel)

# Chargement des helpers
Rcpp::sourceCpp("www/permute.cpp")
Rcpp::sourceCpp("www/fis_bootstrap.cpp")
source("helper_test.R")

## ---- Configuration ----
n_perm   <- 1000   # Number of permutations for P-values
n_boot   <- 1000   # Number of bootstrap replicates for CIs
conf     <- 0.95   # Confidence level
digits   <- 6      # Printing precision
abs_ci   <- FALSE  # Keep FALSE to allow negative CI bounds (more accurate)

# Configuration parallèle
configure_parallel <- function() {
  # Prevent BLAS oversubscription
  Sys.setenv(
    OMP_NUM_THREADS = "1", 
    MKL_NUM_THREADS = "1", 
    OPENBLAS_NUM_THREADS = "1"
  )
  
  # Reproducible parallel RNG
  RNGkind("L'Ecuyer-CMRG")
  set.seed(1)
  
  n_workers <- max(1, detectCores() - 1)
  
  # Calculate chunk sizes for parallel processing
  chunk_sizes <- rep(n_perm %/% n_workers, n_workers)
  remainder <- n_perm %% n_workers
  if (remainder > 0) {
    chunk_sizes[1:remainder] <- chunk_sizes[1:remainder] + 1
  }
  
  return(list(
    n_workers = n_workers,
    chunk_sizes = chunk_sizes[chunk_sizes > 0]
  ))
}

parallel_config <- configure_parallel()

## ---- Chargement et validation des données ----
load_data <- function(file_path = "data/formatted_data.RData") {
  if (!file.exists(file_path)) {
    stop("Fichier de données introuvable: ", file_path)
  }
  
  load(file_path)
  
  # Validation des données
  required_components <- c("haplotype", "individual", "Population")
  missing_components <- setdiff(required_components, names(formatted_data))
  if (length(missing_components) > 0) {
    stop("Composants manquants dans formatted_data: ", 
         paste(missing_components, collapse = ", "))
  }
  
  return(formatted_data)
}

formatted_data <- load_data()

## ---- Préparation des données ----
prepare_genetic_data <- function(formatted_data) {
  loci_names <- colnames(formatted_data$haplotype)
  n_loci <- length(loci_names)
  
  # Validation des loci
  if (n_loci == 0) {
    stop("Aucun locus trouvé dans les données")
  }
  
  # Construction du genind
  mydata_genind <- tryCatch({
    adegenet::df2genind(
      X = as.matrix(formatted_data$haplotype),
      sep = "/",
      ncode = 3,
      ind.names = formatted_data$individual,
      pop = formatted_data$Population,
      NA.char = ifelse(is.null(formatted_data$missing_code), "0", formatted_data$missing_code),
      ploidy = 2,
      type = "codom"
    )
  }, error = function(e) {
    stop("Erreur dans la création du genind: ", e$message)
  })
  
  # Conversion en format hierfstat
  mydata_hierfstat <- hierfstat::genind2hierfstat(mydata_genind)
  if (!identical(colnames(mydata_hierfstat)[1], "pop")) {
    colnames(mydata_hierfstat)[1] <- "pop"
  }
  
  return(list(
    loci_names = loci_names,
    n_loci = n_loci,
    genind = mydata_genind,
    hierfstat = mydata_hierfstat
  ))
}

genetic_data <- prepare_genetic_data(formatted_data)
loci_names <- genetic_data$loci_names
n_loci <- genetic_data$n_loci
mydata_hierfstat_a <- genetic_data$hierfstat

## ---- Calcul des FIS observés ----
calculate_observed_fis <- function(hierfstat_data, loci_names) {
  obs_wc_hier <- tryCatch({
    hierfstat::wc(hierfstat_data)
  }, error = function(e) {
    stop("Erreur dans le calcul FIS observé: ", e$message)
  })
  
  obs_fis_wc_by_locus <- obs_wc_hier$per.loc[, "FIS"]
  
  # Assurer l'ordre correct
  if (!identical(rownames(obs_wc_hier$per.loc), loci_names)) {
    obs_fis_wc_by_locus <- obs_fis_wc_by_locus[match(loci_names, rownames(obs_wc_hier$per.loc))]
  }
  
  # Calcul de la moyenne globale
  obs_fis_wc_overall <- mean(obs_fis_wc_by_locus, na.rm = TRUE)
  
  return(list(
    by_locus = obs_fis_wc_by_locus,
    overall = obs_fis_wc_overall
  ))
}

observed_fis <- calculate_observed_fis(mydata_hierfstat_a, loci_names)
obs_fis_wc_by_locus <- observed_fis$by_locus
obs_fis_wc_overall <- observed_fis$overall

## ---- Préparation de la matrice pour C++ ----
prepare_matrix_for_cpp <- function(hierfstat_data) {
  hf <- as.data.frame(hierfstat_data)
  
  # Conversion de la colonne population en entiers
  if (!is.integer(hf[[1]])) {
    hf[[1]] <- as.integer(as.factor(hf[[1]]))
  }
  
  # Conversion des loci en entiers
  for (j in 2:ncol(hf)) {
    if (!is.integer(hf[[j]])) {
      hf[[j]] <- suppressWarnings(as.integer(hf[[j]]))
    }
  }
  
  colnames(hf)[1] <- "pop"
  mat_int <- as.matrix(hf)
  storage.mode(mat_int) <- "integer"
  
  return(mat_int)
}

mat_int <- prepare_matrix_for_cpp(mydata_hierfstat_a)

## ---- Tests de permutation (P-values) ----
run_permutation_tests <- function(mat_int, loci_names, chunk_sizes, n_workers) {
  message("Démarrage des tests de permutation...")
  
  perm_list <- tryCatch({
    mclapply(
      chunk_sizes,
      function(Bi) {
        batch_permute_wc_fis(mat_int, 1, 0, Bi)
      },
      mc.cores = n_workers,
      mc.preschedule = FALSE  # Better for uneven chunk sizes
    )
  }, error = function(e) {
    stop("Erreur dans les tests de permutation: ", e$message)
  })
  
  # Combinaison des résultats
  perm_fis_by_locus <- do.call(rbind, perm_list)
  colnames(perm_fis_by_locus) <- colnames(mat_int)[-1]
  perm_fis_by_locus <- perm_fis_by_locus[, loci_names, drop = FALSE]
  
  return(perm_fis_by_locus)
}

perm_fis_by_locus <- run_permutation_tests(mat_int, loci_names, 
                                          parallel_config$chunk_sizes, 
                                          parallel_config$n_workers)

## ---- Calcul des P-values ----
calculate_pvalues <- function(obs_fis, perm_fis, loci_names) {
  pval_fis_by_locus <- sapply(seq_along(loci_names), function(j) {
    p_two(obs_fis[j], perm_fis[, j])
  })
  names(pval_fis_by_locus) <- loci_names
  
  # P-value globale
  perm_fis_overall <- apply(perm_fis_by_locus, 1, function(x) {
    mean(x, na.rm = TRUE)
  })
  pval_fis_overall <- p_two(mean(obs_fis, na.rm = TRUE), perm_fis_overall)
  
  return(list(
    by_locus = pval_fis_by_locus,
    overall = pval_fis_overall
  ))
}

pvalues <- calculate_pvalues(obs_fis_wc_by_locus, perm_fis_by_locus, loci_names)

## ---- Intervalles de confiance bootstrap ----
calculate_bootstrap_cis <- function(hierfstat_data, loci_names, n_boot, conf) {
  hf <- as.data.frame(hierfstat_data)
  alpha <- (1 - conf) / 2
  
  message("Calcul des IC bootstrap niveau individuel...")
  set.seed(3)
  boot_indiv_mat <- tryCatch({
    boot_indiv_wc_fis(as.matrix(hf), pop_col = 0L, missing_code = 0L, B = n_boot)
  }, error = function(e) {
    stop("Erreur dans le bootstrap individuel: ", e$message)
  })
  
  # IC par locus
  fis_ci_per_locus <- data.frame(
    Locus = loci_names,
    CI_L = apply(boot_indiv_mat, 2, quantile, probs = alpha, na.rm = TRUE),
    CI_U = apply(boot_indiv_mat, 2, quantile, probs = 1 - alpha, na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  message("Calcul des IC bootstrap niveau population...")
  set.seed(2)
  boot_pop_result <- tryCatch({
    boot_popblock_wc_fis(
      as.matrix(hf),
      pop_col = 0L,
      missing_code = 0L,
      B = n_boot,
      return_locus_matrix = FALSE
    )
  }, error = function(e) {
    stop("Erreur dans le bootstrap par population: ", e$message)
  })
  
  overall_boot <- boot_pop_result$overall
  overall_ci <- quantile(overall_boot, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  # IC par resampling des loci (optionnel)
  message("Calcul des IC par resampling des loci...")
  bootL_vec <- boot_loci_mean(obs_fis_wc_by_locus, B = n_boot)
  bootL_ci <- quantile(bootL_vec, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  
  return(list(
    per_locus = fis_ci_per_locus,
    overall = overall_ci,
    locus_resampling = bootL_ci
  ))
}

bootstrap_cis <- calculate_bootstrap_cis(mydata_hierfstat_a, loci_names, n_boot, conf)

## ---- Application de la correction abs_ci si nécessaire ----
apply_ci_correction <- function(cis, abs_ci) {
  if (isTRUE(abs_ci)) {
    cis$per_locus$CI_L <- pmax(cis$per_locus$CI_L, 0)
    cis$overall[1] <- max(cis$overall[1], 0)
    cis$locus_resampling[1] <- max(cis$locus_resampling[1], 0)
  }
  return(cis)
}

bootstrap_cis <- apply_ci_correction(bootstrap_cis, abs_ci)

## ---- Construction du tableau final ----
build_final_table <- function(obs_fis_by_locus, obs_fis_overall, pvalues, 
                             cis, loci_names, digits) {
  # Tableau par locus
  perm_tbl <- data.frame(
    Locus = loci_names,
    Observed_FIS_WC = as.numeric(obs_fis_by_locus),
    P_value = as.numeric(pvalues$by_locus),
    stringsAsFactors = FALSE
  )
  
  # Fusion avec les IC
  res_tbl <- merge(
    perm_tbl,
    cis$per_locus[, c("Locus", "CI_L", "CI_U")],
    by = "Locus",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Réorganisation selon l'ordre original
  res_tbl <- res_tbl[match(loci_names, res_tbl$Locus), ]
  
  # Ligne globale
  overall_row <- data.frame(
    Locus = "Overall",
    Observed_FIS_WC = as.numeric(obs_fis_overall),
    P_value = as.numeric(pvalues$overall),
    CI_L = as.numeric(cis$overall[1]),
    CI_U = as.numeric(cis$overall[2]),
    stringsAsFactors = FALSE
  )
  
  fis_with_overall <- rbind(res_tbl, overall_row)
  
  # Formatage pour l'affichage
  fis_with_overall_print <- within(fis_with_overall, {
    Observed_FIS_WC <- fmt_fixed(Observed_FIS_WC, digits)
    P_value <- fmt_fixed(P_value, digits)
    CI_L <- fmt_fixed(CI_L, digits)
    CI_U <- fmt_fixed(CI_U, digits)
  })
  
  return(list(
    raw = fis_with_overall,
    formatted = fis_with_overall_print
  ))
}

final_results <- build_final_table(
  obs_fis_wc_by_locus, obs_fis_wc_overall, pvalues,
  bootstrap_cis, loci_names, digits
)

## ---- Affichage des résultats ----
print_results <- function(results) {
  message("\n=== RÉSULTATS FIS AVEC INTERVALLES DE CONFIANCE ===\n")
  print(results$formatted, row.names = FALSE)
  
  # Statistiques supplémentaires
  message("\n=== STATISTIQUES SUPPLÉMENTAIRES ===\n")
  cat("Nombre de loci:", n_loci, "\n")
  cat("Nombre de permutations:", n_perm, "\n")
  cat("Nombre de réplicats bootstrap:", n_boot, "\n")
  cat("Niveau de confiance:", conf, "\n")
  
  # Résumé des P-values
  pval_summary <- summary(results$raw$P_value[results$raw$Locus != "Overall"])
  cat("\nRésumé des P-values par locus:\n")
  print(pval_summary)
}

print_results(final_results)

## ---- Sauvegarde des résultats ----
save_results <- function(results, genetic_data, filename = "fis_bootstrap_results.RData") {
  save(
    results = results,
    loci_names = genetic_data$loci_names,
    n_perm = n_perm,
    n_boot = n_boot,
    conf = conf,
    file = filename
  )
  message("Résultats sauvegardés dans: ", filename)
}

save_results(final_results, genetic_data)

## ---- Validation des résultats ----
validate_results <- function(results) {
  issues <- c()
  
  # Vérification des NA
  na_count <- sum(is.na(results$raw$Observed_FIS_WC))
  if (na_count > 0) {
    issues <- c(issues, paste(na_count, "valeurs FIS manquantes"))
  }
  
  # Vérification des intervalles de confiance
  invalid_ci <- sum(results$raw$CI_L > results$raw$CI_U, na.rm = TRUE)
  if (invalid_ci > 0) {
    issues <- c(issues, paste(invalid_ci, "intervalles de confiance invalides"))
  }
  
  if (length(issues) == 0) {
    message("✓ Validation des résultats: OK")
  } else {
    warning("Problèmes détectés: ", paste(issues, collapse = "; "))
  }
}

validate_results(final_results)

message("\n=== ANALYSE TERMINÉE AVEC SUCCÈS ===")