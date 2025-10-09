# scripts/02_basic_analysis.R - Analyses de diversité génétique de base
cat("📊 Analyses de diversité génétique...\n")

# Chargement des données
load("data/formatted_data.RData")

# Fonctions d'analyse de base
analyze_genetic_diversity <- function(formatted_data) {
  # Métadonnées de base
  n_marker <- ncol(formatted_data$haplotype)
  n_pop <- length(unique(formatted_data$Population))
  n_indv <- length(formatted_data$individual)
  pops <- unique(formatted_data$Population)
  
  cat("📈 Métadonnées:\n")
  cat("   - Individus:", n_indv, "\n")
  cat("   - Populations:", n_pop, "\n")
  cat("   - Marqueurs:", n_marker, "\n")
  cat("   - Populations:", paste(pops, collapse = ", "), "\n")
  
  # Calcul des objets génétiques
  cat("🔬 Calcul des objets génétiques...\n")
  mydata_genind <- adegenet::df2genind(
    X = as.matrix(formatted_data$haplotype),
    sep = "/",
    ncode = 6,
    ind.names = formatted_data$individual,
    pop = formatted_data$Population,
    NA.char = formatted_data$missing_code,
    ploidy = 2,
    type = "codom"
  )
  
  # Conversion en hierfstat
  mydata_hierfstat <- hierfstat::genind2hierfstat(mydata_genind)
  
  # Statistiques basiques
  cat("📊 Calcul des statistiques de diversité...\n")
  result_basic <- hierfstat::basic.stats(mydata_hierfstat)
  df_result_basic <- as.data.frame(result_basic$perloc)
  
  # Ajout des statistiques G
  df_result_basic <- df_result_basic %>%
    dplyr::mutate(
      GST = 1 - Hs / Ht,
      `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
    )
  
  # F-statistiques de Weir & Cockerham
  cat("📐 Calcul des F-statistiques...\n")
  data_loci <- pegas::as.loci(mydata_genind)
  result_f_stats <- pegas::Fst(data_loci)
  
  if (is.matrix(result_f_stats)) {
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
  }
  
  return(list(
    genind = mydata_genind,
    hierfstat = mydata_hierfstat,
    basic_stats = result_basic,
    df_basic = df_result_basic,
    f_stats = result_f_stats,
    metadata = list(
      n_indv = n_indv,
      n_pop = n_pop,
      n_marker = n_marker,
      pops = pops
    )
  ))
}

# Exécution de l'analyse
tryCatch({
  diversity_results <- analyze_genetic_diversity(formatted_data)
  
  # Sauvegarde des résultats
  save(diversity_results, file = "data/diversity_analysis_results.RData")
  
  # Résumé des résultats
  cat("🎯 RÉSULTATS DE DIVERSITÉ:\n")
  cat("   - Hs moyen:", round(mean(diversity_results$df_basic$Hs, na.rm = TRUE), 4), "\n")
  cat("   - Ht moyen:", round(mean(diversity_results$df_basic$Ht, na.rm = TRUE), 4), "\n")
  cat("   - Fst moyen:", round(mean(diversity_results$df_basic$Fst, na.rm = TRUE), 4), "\n")
  
  if (is.matrix(diversity_results$f_stats)) {
    avg_fis <- mean(diversity_results$f_stats[, "Fis (W&C)"], na.rm = TRUE)
    cat("   - Fis moyen (W&C):", round(avg_fis, 4), "\n")
  }
  
  cat("✅ Analyses de diversité terminées et sauvegardées\n")
  
}, error = function(e) {
  cat("❌ Erreur dans l'analyse de diversité:", e$message, "\n")
})