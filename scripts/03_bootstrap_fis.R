# scripts/03_bootstrap_fis.R - Version déboguée
cat("🔄 Tests bootstrap FIS...\n")

# Chargement des données
load("data/formatted_data.RData")

# Configuration
n_perm <- 100
n_boot <- 100
conf_level <- 0.95
digits <- 4

cat("⚙️  Configuration:\n")
cat("   - Permutations:", n_perm, "\n")
cat("   - Bootstrap:", n_boot, "\n")
cat("   - Niveau de confiance:", conf_level, "\n")

# Détection du système
is_windows <- .Platform$OS.type == "windows"
cat("   - Système:", ifelse(is_windows, "Windows", "Unix-like"), "\n")

# Fonction safe pour les permutations Windows
run_permutations_windows <- function(mat_int, n_perm, loci_names) {
  cat("   🪟 Mode Windows - Permutations séquentielles...\n")
  
  perm_fis_by_locus <- matrix(NA, nrow = n_perm, ncol = length(loci_names))
  colnames(perm_fis_by_locus) <- loci_names
  
  for (b in 1:n_perm) {
    tryCatch({
      result <- batch_permute_wc_fis(mat_int, 1, 0, 1)
      # Vérification que le résultat est numérique
      if (is.numeric(result$permutations) && nrow(result$permutations) > 0) {
        perm_fis_by_locus[b, ] <- as.numeric(result$permutations[1, ])
      }
    }, error = function(e) {
      cat("Erreur permutation", b, ":", e$message, "\n")
    })
    
    # Progression
    if (b %% 10 == 0) cat("   ", b, "/", n_perm, "\n")
  }
  
  return(perm_fis_by_locus)
}

# Fonction safe pour calculer les P-values
safe_p_two <- function(obs, perm_vec) {
  if (is.na(obs) || length(perm_vec) == 0) return(NA_real_)
  
  # Nettoyage des valeurs
  v <- perm_vec[!is.na(perm_vec) & is.numeric(perm_vec)]
  if (length(v) == 0) return(NA_real_)
  
  # Conversion en numérique
  v <- as.numeric(v)
  obs <- as.numeric(obs)
  
  p_upper <- mean(v >= obs, na.rm = TRUE)
  p_lower <- mean(v <= obs, na.rm = TRUE)
  
  p_value <- 2 * min(p_upper, p_lower)
  return(min(1.0, p_value))
}

# Fonction safe pour les quantiles
safe_quantile <- function(x, probs, na.rm = TRUE) {
  if (!is.numeric(x) || all(is.na(x))) return(NA_real_)
  x_clean <- as.numeric(x[!is.na(x)])
  if (length(x_clean) == 0) return(NA_real_)
  return(quantile(x_clean, probs = probs, na.rm = na.rm))
}

# Fonction principale avec débogage
run_bootstrap_fis_analysis <- function(formatted_data, n_perm, n_boot, conf_level) {
  
  # Création des objets génétiques
  cat("🔬 Création des objets génétiques...\n")
  mydata_genind <- adegenet::df2genind(
    X = as.matrix(formatted_data$haplotype),
    sep = "/",
    ncode = 3,
    ind.names = formatted_data$individual,
    pop = formatted_data$Population,
    NA.char = formatted_data$missing_code,
    ploidy = 2,
    type = "codom"
  )
  
  mydata_hierfstat <- hierfstat::genind2hierfstat(mydata_genind)
  loci_names <- colnames(mydata_hierfstat)[-1]
  cat("✅ Objets créés -", length(loci_names), "loci:", paste(loci_names, collapse = ", "), "\n")
  
  # FIS observés
  cat("📊 Calcul des FIS observés...\n")
  obs_wc_hier <- hierfstat::wc(mydata_hierfstat)
  obs_fis_wc_by_locus <- as.numeric(obs_wc_hier$per.loc[, "FIS"])
  obs_fis_wc_overall <- mean(obs_fis_wc_by_locus, na.rm = TRUE)
  
  cat("📈 FIS observés - Moyenne:", round(obs_fis_wc_overall, digits), "\n")
  cat("   Valeurs par locus:", round(obs_fis_wc_by_locus, 3), "\n")
  
  # Préparation matrice C++
  cat("🔧 Préparation matrice C++...\n")
  hf <- as.data.frame(mydata_hierfstat)
  hf[[1]] <- as.integer(as.factor(hf[[1]]))
  
  for (j in 2:ncol(hf)) {
    hf[[j]] <- as.integer(hf[[j]])
  }
  
  colnames(hf)[1] <- "pop"
  mat_int <- as.matrix(hf)
  storage.mode(mat_int) <- "integer"
  
  cat("✅ Matrice préparée -", nrow(mat_int), "individus x", ncol(mat_int), "colonnes\n")
  
  # Tests de permutation
  cat("🎲 Tests de permutation...\n")
  set.seed(123)
  
  perm_fis_by_locus <- run_permutations_windows(mat_int, n_perm, loci_names)
  
  # Vérification des résultats de permutation
  cat("🔍 Vérification des permutations...\n")
  cat("   Dimensions perm_fis_by_locus:", dim(perm_fis_by_locus), "\n")
  cat("   Valeurs manquantes:", sum(is.na(perm_fis_by_locus)), "/", length(perm_fis_by_locus), "\n")
  
  # P-values avec sécurité
  cat("📐 Calcul des P-values...\n")
  pval_fis_by_locus <- numeric(length(loci_names))
  
  for (j in seq_along(loci_names)) {
    pval_fis_by_locus[j] <- safe_p_two(obs_fis_wc_by_locus[j], perm_fis_by_locus[, j])
  }
  names(pval_fis_by_locus) <- loci_names
  
  # P-value globale
  perm_fis_overall <- apply(perm_fis_by_locus, 1, mean, na.rm = TRUE)
  pval_fis_overall <- safe_p_two(obs_fis_wc_overall, perm_fis_overall)
  
  cat("✅ Permutations terminées - P-value globale:", round(pval_fis_overall, 4), "\n")
  
  # Intervalles de confiance bootstrap
  cat("📊 Calcul des intervalles de confiance...\n")
  
  # Bootstrap individuel
  cat("   a) Bootstrap individuel...\n")
  set.seed(3)
  boot_indiv_mat <- boot_indiv_wc_fis(as.matrix(hf), pop_col = 0L, missing_code = 0L, B = n_boot)
  
  # Vérification bootstrap
  cat("   🔍 Vérification bootstrap individuel...\n")
  cat("     Dimensions:", dim(boot_indiv_mat), "\n")
  cat("     Valeurs manquantes:", sum(is.na(boot_indiv_mat)), "\n")
  
  alpha <- (1 - conf_level) / 2
  
  # Calcul sécurisé des IC
  ci_lower <- numeric(length(loci_names))
  ci_upper <- numeric(length(loci_names))
  
  for (j in 1:length(loci_names)) {
    ci_lower[j] <- safe_quantile(boot_indiv_mat[, j], probs = alpha)
    ci_upper[j] <- safe_quantile(boot_indiv_mat[, j], probs = 1 - alpha)
  }
  
  fis_ci_per_locus <- data.frame(
    Locus = loci_names,
    CI_L = ci_lower,
    CI_U = ci_upper,
    stringsAsFactors = FALSE
  )
  
  # Bootstrap population
  cat("   b) Bootstrap population...\n")
  set.seed(2)
  bootP_result <- boot_popblock_wc_fis(
    as.matrix(hf),
    pop_col = 0L,
    missing_code = 0L,
    B = n_boot,
    return_locus_matrix = FALSE
  )
  
  overall_ci <- c(
    safe_quantile(bootP_result$overall, probs = alpha),
    safe_quantile(bootP_result$overall, probs = 1 - alpha)
  )
  
  cat("✅ Bootstrap terminé\n")
  
  # Construction du tableau final avec vérifications
  cat("📋 Construction du tableau final...\n")
  
  # Vérification des longueurs
  cat("   Vérification des longueurs:\n")
  cat("   - Loci names:", length(loci_names), "\n")
  cat("   - FIS observés:", length(obs_fis_wc_by_locus), "\n")
  cat("   - P-values:", length(pval_fis_by_locus), "\n")
  cat("   - CI lower:", length(ci_lower), "\n")
  
  # Tableau par locus
  perm_tbl <- data.frame(
    Locus = loci_names,
    Observed_FIS = obs_fis_wc_by_locus,
    P_value = pval_fis_by_locus,
    stringsAsFactors = FALSE
  )
  
  # Fusion avec IC
  res_tbl <- merge(perm_tbl, fis_ci_per_locus, by = "Locus", all.x = TRUE, sort = FALSE)
  res_tbl <- res_tbl[match(loci_names, res_tbl$Locus), ]
  
  # Ligne globale
  overall_row <- data.frame(
    Locus = "Overall",
    Observed_FIS = obs_fis_wc_overall,
    P_value = pval_fis_overall,
    CI_L = overall_ci[1],
    CI_U = overall_ci[2],
    stringsAsFactors = FALSE
  )
  
  final_results <- rbind(res_tbl, overall_row)
  
  # Vérification finale
  cat("🔍 Vérification finale du tableau...\n")
  cat("   Dimensions finales:", dim(final_results), "\n")
  cat("   Colonnes:", names(final_results), "\n")
  cat("   Types de données:\n")
  for (col in names(final_results)) {
    cat("     -", col, ":", class(final_results[[col]]), "\n")
  }
  
  return(list(
    final_table = final_results,
    observed_fis = obs_fis_wc_by_locus,
    permutation_results = perm_fis_by_locus,
    bootstrap_results = list(
      individual = boot_indiv_mat,
      population = bootP_result
    ),
    metadata = list(
      n_loci = length(loci_names),
      n_permutations = n_perm,
      n_bootstrap = n_boot,
      loci_names = loci_names
    )
  ))
}

# Exécution avec gestion d'erreur détaillée
cat("🚀 Démarrage de l'analyse bootstrap FIS...\n")

tryCatch({
  # Réduction pour test
  n_perm_test <- 10
  n_boot_test <- 10
  
  cat("🧪 Mode TEST - Paramètres réduits:\n")
  cat("   - Permutations:", n_perm_test, "\n")
  cat("   - Bootstrap:", n_boot_test, "\n")
  
  # Test d'abord avec peu d'itérations
  bootstrap_results <- run_bootstrap_fis_analysis(formatted_data, n_perm_test, n_boot_test, conf_level)
  
  cat("✅ TEST RÉUSSI! Lancement de l'analyse complète...\n")
  
  # Maintenant l'analyse complète
  bootstrap_results <- run_bootstrap_fis_analysis(formatted_data, n_perm, n_boot, conf_level)
  
  # Formatage pour affichage
  fmt_fixed <- function(x, digits = 4) {
    ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = digits))
  }
  
  final_display <- bootstrap_results$final_table
  final_display$Observed_FIS <- fmt_fixed(final_display$Observed_FIS, digits)
  final_display$P_value <- fmt_fixed(final_display$P_value, digits)
  final_display$CI_L <- fmt_fixed(final_display$CI_L, digits)
  final_display$CI_U <- fmt_fixed(final_display$CI_U, digits)
  
  # Sauvegarde
  save(bootstrap_results, file = "data/bootstrap_fis_results.RData")
  write.csv(bootstrap_results$final_table, "data/bootstrap_fis_table.csv", row.names = FALSE)
  
  # Affichage des résultats
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("🎯 RÉSULTATS BOOTSTRAP FIS\n")
  cat(strrep("=", 60), "\n")
  print(final_display, row.names = FALSE)
  
  cat("\n📊 RÉSUMÉ:\n")
  cat("   - FIS global:", final_display$Observed_FIS[final_display$Locus == "Overall"], "\n")
  cat("   - P-value globale:", final_display$P_value[final_display$Locus == "Overall"], "\n")
  cat("   - Fichiers sauvegardés: data/bootstrap_fis_*.{RData,csv}\n")
  
  cat("✅ ANALYSE TERMINÉE AVEC SUCCÈS!\n")
  
}, error = function(e) {
  cat("\n❌ ERREUR:", e$message, "\n")
  cat("📋 Stack trace:\n")
  print(traceback())
})