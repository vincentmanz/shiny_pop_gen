# ==================== FONCTIONS DE BOOTSTRAP OPTIMISÉES ====================

#' Bootstrap FIS par locus avec rééchantillonnage des individus (niveau i)
#' 
#' @param hier_df DataFrame au format hierfstat (première colonne = population)
#' @param locus Nom du locus à analyser
#' @param B Nombre de réplicats bootstrap (défaut: 1000)
#' @param conf Niveau de confiance (défaut: 0.95)
#' @return DataFrame avec FIS observé et intervalle de confiance
boot_fis_per_locus_i <- function(hier_df, locus, B = 1000, conf = 0.95) {
  # Validation des entrées
  if (!is.data.frame(hier_df) || nrow(hier_df) == 0) {
    stop("Le dataframe d'entrée est invalide ou vide")
  }
  
  pop_col <- intersect(c("pop", "Population"), colnames(hier_df))[1]
  if (is.na(pop_col)) {
    stop("Colonne de population introuvable. Attendu: 'pop' ou 'Population'")
  }
  
  if (!locus %in% colnames(hier_df)) {
    stop("Locus '", locus, "' introuvable dans les données")
  }
  
  # Préparation des données
  df_loc <- hier_df[, c(pop_col, locus), drop = FALSE]
  colnames(df_loc)[1] <- "pop"  # Standardisation pour hierfstat::wc
  
  # Valeur observée
  obs_result <- tryCatch({
    hierfstat::wc(df_loc)$per.loc[1, "FIS"]
  }, error = function(e) {
    warning("Erreur dans le calcul FIS observé pour ", locus, ": ", e$message)
    return(NA_real_)
  })
  
  # Bootstrap par individu
  pops <- unique(df_loc$pop)
  dist <- numeric(B)
  valid_boots <- 0
  
  for (b in seq_len(B)) {
    tryCatch({
      # Rééchantillonnage par population
      boot_samples <- lapply(pops, function(p) {
        pop_rows <- which(df_loc$pop == p)
        if (length(pop_rows) == 0) return(NULL)
        df_loc[sample(pop_rows, length(pop_rows), replace = TRUE), , drop = FALSE]
      })
      
      boot_df <- do.call(rbind, boot_samples[!sapply(boot_samples, is.null)])
      
      if (!is.null(boot_df) && nrow(boot_df) > 0) {
        boot_fis <- hierfstat::wc(boot_df)$per.loc[1, "FIS"]
        if (!is.na(boot_fis)) {
          dist[valid_boots + 1] <- boot_fis
          valid_boots <- valid_boots + 1
        }
      }
    }, error = function(e) {
      # Ignorer les erreurs de bootstrap individuelles
    })
  }
  
  # Calcul des intervalles de confiance seulement si suffisamment de réplicats valides
  alpha <- (1 - conf) / 2
  if (valid_boots >= 10) {  # Minimum de 10 réplicats valides
    ci_lower <- unname(quantile(dist[1:valid_boots], probs = alpha, na.rm = TRUE))
    ci_upper <- unname(quantile(dist[1:valid_boots], probs = 1 - alpha, na.rm = TRUE))
  } else {
    ci_lower <- ci_upper <- NA_real_
    warning("Locus ", locus, ": seulement ", valid_boots, " réplicats bootstrap valides")
  }
  
  data.frame(
    Locus = locus,
    FIS = obs_result,
    CI_iL = ci_lower,
    CI_iU = ci_upper,
    n_valid_boots = valid_boots,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Bootstrap FIS par locus avec rééchantillonnage des populations (niveau s)
#' 
#' @param hier_df DataFrame au format hierfstat (première colonne = population)
#' @param locus Nom du locus à analyser
#' @param B Nombre de réplicats bootstrap (défaut: 1000)
#' @param conf Niveau de confiance (défaut: 0.95)
#' @return DataFrame avec FIS observé et intervalle de confiance
boot_fis_per_locus_s <- function(hier_df, locus, B = 1000, conf = 0.95) {
  # Validation des entrées (identique à la fonction i)
  if (!is.data.frame(hier_df) || nrow(hier_df) == 0) {
    stop("Le dataframe d'entrée est invalide ou vide")
  }
  
  pop_col <- intersect(c("pop", "Population"), colnames(hier_df))[1]
  if (is.na(pop_col)) {
    stop("Colonne de population introuvable. Attendu: 'pop' ou 'Population'")
  }
  
  if (!locus %in% colnames(hier_df)) {
    stop("Locus '", locus, "' introuvable dans les données")
  }
  
  # Préparation des données
  df_loc <- hier_df[, c(pop_col, locus), drop = FALSE]
  colnames(df_loc)[1] <- "pop"
  
  # Valeur observée
  obs_result <- tryCatch({
    hierfstat::wc(df_loc)$per.loc[1, "FIS"]
  }, error = function(e) {
    warning("Erreur dans le calcul FIS observé pour ", locus, ": ", e$message)
    return(NA_real_)
  })
  
  # Vérification du nombre de populations
  pops <- unique(df_loc$pop)
  if (length(pops) < 2) {
    warning("Locus ", locus, ": besoin d'au moins 2 populations pour le bootstrap niveau s")
    return(data.frame(
      Locus = locus, 
      FIS = obs_result, 
      CI_sL = NA_real_, 
      CI_sU = NA_real_,
      n_valid_boots = 0,
      row.names = NULL,
      stringsAsFactors = FALSE
    ))
  }
  
  # Bootstrap par population
  dist <- numeric(B)
  valid_boots <- 0
  
  for (b in seq_len(B)) {
    tryCatch({
      # Rééchantillonnage des populations
      samp_pops <- sample(pops, length(pops), replace = TRUE)
      boot_samples <- lapply(samp_pops, function(p) {
        df_loc[df_loc$pop == p, , drop = FALSE]
      })
      
      boot_df <- do.call(rbind, boot_samples)
      
      if (!is.null(boot_df) && nrow(boot_df) > 0) {
        boot_fis <- hierfstat::wc(boot_df)$per.loc[1, "FIS"]
        if (!is.na(boot_fis)) {
          dist[valid_boots + 1] <- boot_fis
          valid_boots <- valid_boots + 1
        }
      }
    }, error = function(e) {
      # Ignorer les erreurs de bootstrap individuelles
    })
  }
  
  # Calcul des intervalles de confiance
  alpha <- (1 - conf) / 2
  if (valid_boots >= 10) {
    ci_lower <- unname(quantile(dist[1:valid_boots], probs = alpha, na.rm = TRUE))
    ci_upper <- unname(quantile(dist[1:valid_boots], probs = 1 - alpha, na.rm = TRUE))
  } else {
    ci_lower <- ci_upper <- NA_real_
    warning("Locus ", locus, ": seulement ", valid_boots, " réplicats bootstrap valides (niveau s)")
  }
  
  data.frame(
    Locus = locus,
    FIS = obs_result,
    CI_sL = ci_lower,
    CI_sU = ci_upper,
    n_valid_boots = valid_boots,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Fonction wrapper pour exécuter les deux types de bootstrap sur tous les loci
#' 
#' @param hier_df DataFrame au format hierfstat
#' @param loci_names Vecteur des noms de locus à analyser
#' @param B Nombre de réplicats bootstrap
#' @param conf Niveau de confiance
#' @param seed Graine aléatoire pour la reproductibilité
#' @return Liste avec résultats niveau i, niveau s, et tableau combiné
run_complete_bootstrap_analysis <- function(hier_df, loci_names, B = 1000, conf = 0.95, seed = 123) {
  # Validation
  if (!all(loci_names %in% colnames(hier_df))) {
    missing_loci <- setdiff(loci_names, colnames(hier_df))
    stop("Locus manquants: ", paste(missing_loci, collapse = ", "))
  }
  
  # Bootstrap niveau i (individus)
  set.seed(seed)
  message("Début du bootstrap niveau i (individus)...")
  perloc_i <- do.call(rbind, lapply(loci_names, function(locus) {
    result <- boot_fis_per_locus_i(hier_df, locus, B, conf)
    message("Locus ", locus, " - i: ", result$n_valid_boots, "/", B, " bootstraps valides")
    result
  }))
  
  # Bootstrap niveau s (populations)
  set.seed(seed + 1)  # Graine différente pour éviter la corrélation
  message("Début du bootstrap niveau s (populations)...")
  perloc_s <- do.call(rbind, lapply(loci_names, function(locus) {
    result <- boot_fis_per_locus_s(hier_df, locus, B, conf)
    message("Locus ", locus, " - s: ", result$n_valid_boots, "/", B, " bootstraps valides")
    result
  }))
  
  # Fusion des résultats
  perloc_combined <- merge(
    perloc_i[, c("Locus", "FIS", "CI_iL", "CI_iU", "n_valid_boots")],
    perloc_s[, c("Locus", "CI_sL", "CI_sU")],
    by = "Locus", 
    all.x = TRUE, 
    sort = FALSE
  )
  
  # Réorganisation selon l'ordre original des loci
  perloc_combined <- perloc_combined[match(loci_names, perloc_combined$Locus), ]
  
  list(
    bootstrap_i = perloc_i,
    bootstrap_s = perloc_s,
    combined = perloc_combined
  )
}

#' Ajoute la ligne "All" avec les résultats globaux
#' 
#' @param perloc_df DataFrame des résultats par locus
#' @param obs_fis_wc_by_locus Vecteur des FIS observés par locus
#' @param bootP_full Résultats du bootstrap par population (objet complet)
#' @return DataFrame avec ligne "All" ajoutée
add_overall_row <- function(perloc_df, obs_fis_wc_by_locus, bootP_full) {
  # Calcul de la moyenne globale (excluant les NA)
  global_fis <- mean(obs_fis_wc_by_locus, na.rm = TRUE)
  
  # Extraction des IC globaux depuis bootP_full
  if (!is.null(bootP_full$overall$lower) && !is.null(bootP_full$overall$upper)) {
    ci_sL <- bootP_full$overall$lower
    ci_sU <- bootP_full$overall$upper
  } else {
    ci_sL <- ci_sU <- NA_real_
    warning("Intervalles de confiance globaux non trouvés dans bootP_full")
  }
  
  overall_row <- data.frame(
    Locus = "All",
    FIS = global_fis,
    CI_iL = NA_real_, 
    CI_iU = NA_real_,  # Non défini pour "All" au niveau i
    CI_sL = ci_sL, 
    CI_sU = ci_sU,
    n_valid_boots = NA_integer_,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # Ajout de la ligne "All"
  rbind(perloc_df, overall_row)
}

#' Formatage des résultats pour l'affichage
#' 
#' @param results_df DataFrame des résultats
#' @param digits Nombre de décimales (défaut: 3)
#' @return DataFrame formaté
format_bootstrap_results <- function(results_df, digits = 3) {
  fmt <- function(x) {
    ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
  }
  
  results_df$FIS <- fmt(results_df$FIS)
  results_df$CI_iL <- fmt(results_df$CI_iL)
  results_df$CI_iU <- fmt(results_df$CI_iU)
  results_df$CI_sL <- fmt(results_df$CI_sL)
  results_df$CI_sU <- fmt(results_df$CI_sU)
  
  results_df
}

# ==================== EXEMPLE D'UTILISATION ====================

# Exemple complet d'analyse
run_example_analysis <- function() {
  # Données d'exemple (à adapter)
  # mydata_hierfstat_a <- ...  # Vos données au format hierfstat
  # loci_names <- c("Locus1", "Locus2", "Locus3")  # Noms des loci
  # obs_fis_wc_by_locus <- ...  # FIS observés par locus
  # bootP_full <- ...  # Résultats bootstrap complets
  
  # Analyse bootstrap complète
  bootstrap_results <- run_complete_bootstrap_analysis(
    hier_df = mydata_hierfstat_a,
    loci_names = loci_names,
    B = 1000,
    conf = 0.95,
    seed = 123
  )
  
  # Ajout de la ligne "All"
  final_results <- add_overall_row(
    bootstrap_results$combined,
    obs_fis_wc_by_locus,
    bootP_full
  )
  
  # Formatage pour l'affichage
  formatted_results <- format_bootstrap_results(final_results, digits = 3)
  
  # Affichage
  print(formatted_results, row.names = FALSE)
  
  return(list(
    raw_results = final_results,
    formatted_results = formatted_results,
    details = bootstrap_results
  ))
}

# ==================== FONCTIONS DE VALIDATION ====================

#' Valide la cohérence des estimateurs FIS
#' 
#' @param hier_df Données au format hierfstat
#' @param loci_names Noms des loci à valider
validate_fis_estimators <- function(hier_df, loci_names) {
  message("Validation des estimateurs FIS...")
  
  for (locus in loci_names) {
    df_loc <- hier_df[, c("pop", locus)]
    
    # Calcul avec hierfstat::wc
    wc_fis <- tryCatch({
      hierfstat::wc(df_loc)$per.loc[1, "FIS"]
    }, error = function(e) NA_real_)
    
    cat(sprintf("Locus %s: hierfstat::wc = %f\n", locus, wc_fis))
  }
}

#' Vérifie la qualité des intervalles de confiance bootstrap
#' 
#' @param bootstrap_results Résultats du bootstrap
check_bootstrap_quality <- function(bootstrap_results) {
  message("Qualité des intervalles de confiance bootstrap:")
  
  for (i in 1:nrow(bootstrap_results)) {
    locus <- bootstrap_results$Locus[i]
    valid_i <- bootstrap_results$n_valid_boots[i]
    
    if (!is.na(valid_i)) {
      coverage <- valid_i / 1000  # Supposant B = 1000
      status <- ifelse(coverage >= 0.9, "✓ OK", "⚠ Faible")
      cat(sprintf("Locus %s: %d/%d bootstraps valides %s\n", locus, valid_i, 1000, status))
    }
  }
}