# test_boot.R - Version optimisée
# Analyse de données génétiques avec calculs de diversité et F-statistiques

# === CHARGEMENT ET PRÉPARATION DES DONNÉES ===

# Chargement des données CSV
load_or_read_data <- function(csv_path = "data/filtered_data.csv", 
                             rdata_path = "data/formatted_data.RData") {
  
  if (file.exists(rdata_path)) {
    message("Chargement des données depuis RData...")
    load(rdata_path)
    return(formatted_data)
  } else if (file.exists(csv_path)) {
    message("Chargement des données depuis CSV...")
    CSV_BOU <- read.csv(csv_path)
    return(CSV_BOU)
  } else {
    stop("Aucun fichier de données trouvé: ", csv_path, " ou ", rdata_path)
  }
}

# Préparation des données formatées
prepare_formatted_data <- function(raw_data) {
  # Validation des données d'entrée
  if (!is.data.frame(raw_data) && !is.list(raw_data)) {
    stop("Les données brutes doivent être un dataframe ou une liste")
  }
  
  # Si c'est un dataframe CSV, créer la structure formatted_data
  if (is.data.frame(raw_data)) {
    formatted_data <- list(
      Population   = raw_data$Population,
      individual   = paste0("Ind_", substr(raw_data$Population, 1, 3), "_", 
                           ifelse("id" %in% names(raw_data), 
                                  raw_data$id, 
                                  seq_len(nrow(raw_data)))),
      ploidy       = 2,
      haplotype    = as.data.frame(raw_data[, 5:10, drop = FALSE]),  # colonnes 5-10
      missing_code = "0/0",
      GPS          = if ("GPS" %in% names(raw_data)) raw_data$GPS else NULL,
      level1       = if ("level1" %in% names(raw_data)) raw_data$level1 else NULL,
      level2       = if ("level2" %in% names(raw_data)) raw_data$level2 else NULL,
      level3       = if ("level3" %in% names(raw_data)) raw_data$level3 else NULL
    )
    
    # Sauvegarde pour usage futur
    save(formatted_data, file = "data/formatted_data.RData")
    message("Données formatées sauvegardées dans data/formatted_data.RData")
    
  } else {
    # Déjà au format formatted_data
    formatted_data <- raw_data
  }
  
  return(formatted_data)
}

# Chargement et préparation des données
raw_data <- load_or_read_data()
formatted_data <- prepare_formatted_data(raw_data)

# === VALIDATION DES DONNÉES ===

validate_genetic_data <- function(formatted_data) {
  required <- c("Population", "individual", "haplotype", "ploidy", "missing_code")
  missing <- setdiff(required, names(formatted_data))
  
  if (length(missing) > 0) {
    stop("Éléments manquants dans formatted_data: ", paste(missing, collapse = ", "))
  }
  
  # Validation des haplotypes
  if (ncol(formatted_data$haplotype) == 0) {
    stop("Aucun marqueur trouvé dans les données d'haplotype")
  }
  
  if (nrow(formatted_data$haplotype) == 0) {
    stop("Aucun individu trouvé dans les données d'haplotype")
  }
  
  # Vérification de la cohérence des longueurs
  n_inds <- length(formatted_data$individual)
  if (nrow(formatted_data$haplotype) != n_inds) {
    stop("Incohérence: ", nrow(formatted_data$haplotype), 
         " lignes d'haplotype pour ", n_inds, " individus")
  }
  
  if (length(formatted_data$Population) != n_inds) {
    stop("Incohérence: ", length(formatted_data$Population), 
         " populations pour ", n_inds, " individus")
  }
  
  message("✓ Données validées: ", n_inds, " individus, ", 
          ncol(formatted_data$haplotype), " marqueurs")
}

validate_genetic_data(formatted_data)

# === CRÉATION DU DATAFRAME FORMATÉ ===

create_formatted_dataframe <- function(formatted_data) {
  # Construction du dataframe principal
  df_formatted <- data.frame(
    Population = formatted_data$Population,
    stringsAsFactors = FALSE
  )
  
  # Ajout des composants optionnels
  optional_components <- c("GPS", "level1", "level2", "level3")
  for (comp in optional_components) {
    if (!is.null(formatted_data[[comp]])) {
      df_formatted[[comp]] <- formatted_data[[comp]]
    }
  }
  
  # Ajout des haplotypes
  df_formatted <- cbind(df_formatted, formatted_data$haplotype)
  
  return(df_formatted)
}

df_formatted <- create_formatted_dataframe(formatted_data)

# === MÉTADONNÉES ===

calculate_metadata <- function(formatted_data) {
  n_marker <- ncol(formatted_data$haplotype)
  n_pop <- length(unique(formatted_data$Population))
  n_indv <- length(formatted_data$individual)
  pops <- unique(formatted_data$Population)
  
  # Calcul des positions des colonnes de génotype
  column_genotype_start <- which(names(df_formatted) == names(formatted_data$haplotype)[1])
  column_genotype_end <- column_genotype_start + n_marker - 1
  
  return(list(
    n_marker = n_marker,
    n_pop = n_pop,
    n_indv = n_indv,
    pops = pops,
    column_genotype_start = column_genotype_start,
    column_genotype_end = column_genotype_end,
    missing_data_code = formatted_data$missing_code
  ))
}

metadata <- calculate_metadata(formatted_data)

message("Métadonnées:")
message("- Marqueurs: ", metadata$n_marker)
message("- Populations: ", metadata$n_pop)
message("- Individus: ", metadata$n_indv)
message("- Populations uniques: ", paste(metadata$pops, collapse = ", "))

# === CRÉATION DES OBJETS GENIND ET HIERFSTAT ===

create_genetic_objects <- function(formatted_data) {
  message("Création des objets génétiques...")
  
  # Objet genind
  mydata_genind <- tryCatch({
    adegenet::df2genind(
      X = as.matrix(formatted_data$haplotype),
      sep = "/",
      ncode = 6,
      ind.names = formatted_data$individual,
      pop = formatted_data$Population,
      NA.char = formatted_data$missing_code,
      ploidy = formatted_data$ploidy,
      type = "codom"
    )
  }, error = function(e) {
    stop("Erreur dans la création de l'objet genind: ", e$message)
  })
  
  # Objet hierfstat
  mydata_hierfstat <- tryCatch({
    hierfstat::genind2hierfstat(mydata_genind)
  }, error = function(e) {
    stop("Erreur dans la conversion vers hierfstat: ", e$message)
  })
  
  message("✓ Objets génétiques créés avec succès")
  
  return(list(
    genind = mydata_genind,
    hierfstat = mydata_hierfstat
  ))
}

genetic_objects <- create_genetic_objects(formatted_data)
mydata_genind <- genetic_objects$genind
mydata_hierfstat <- genetic_objects$hierfstat

# === CALCULS DE DIVERSITÉ GÉNÉTIQUE ===

calculate_diversity_stats <- function(hierfstat_data, n_pop) {
  message("Calcul des statistiques de diversité...")
  
  # Statistiques basiques
  result_basic <- tryCatch({
    hierfstat::basic.stats(hierfstat_data)
  }, error = function(e) {
    stop("Erreur dans basic.stats: ", e$message)
  })
  
  df_result_basic <- as.data.frame(result_basic$perloc)
  
  # Ajout des statistiques G
  df_result_basic <- df_result_basic %>%
    dplyr::mutate(
      GST = 1 - Hs / Ht,
      `GST''` = ifelse(Hs > 0 & Ht > Hs,
                       (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs)),
                       NA_real_)
    )
  
  message("✓ Statistiques de diversité calculées")
  
  return(list(
    basic_stats = result_basic,
    df_perloc = df_result_basic
  ))
}

diversity_results <- calculate_diversity_stats(mydata_hierfstat, metadata$n_pop)
df_result_basic <- diversity_results$df_perloc

# === CALCUL DES F-STATISTIQUES (WEIR & COCKERHAM) ===

calculate_f_statistics <- function(genind_data) {
  message("Calcul des F-statistiques de Weir & Cockerham...")
  
  # Conversion en format loci
  data_loci <- tryCatch({
    pegas::as.loci(genind_data)
  }, error = function(e) {
    stop("Erreur dans la conversion vers format loci: ", e$message)
  })
  
  # Calcul des F-statistiques
  result_f_stats <- tryCatch({
    pegas::Fst(data_loci)
  }, error = function(e) {
    stop("Erreur dans le calcul des F-statistiques: ", e$message)
  })
  
  # Formatage des résultats
  if (is.matrix(result_f_stats)) {
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
  } else if (is.numeric(result_f_stats)) {
    result_f_stats <- matrix(result_f_stats, nrow = 1)
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
  }
  
  message("✓ F-statistiques calculées")
  
  return(result_f_stats)
}

f_statistics <- calculate_f_statistics(mydata_genind)

# === RAPPORT FINAL ===

generate_final_report <- function(metadata, diversity_results, f_statistics) {
  message("\n" + strrep("=", 60))
  message("RAPPORT D'ANALYSE GÉNÉTIQUE")
  message(strrep("=", 60))
  
  message("\nMÉTADONNÉES:")
  message("- Individus: ", metadata$n_indv)
  message("- Populations: ", metadata$n_pop)
  message("- Marqueurs: ", metadata$n_marker)
  message("- Code manquant: ", metadata$missing_data_code)
  
  message("\nDIVERSITÉ GÉNÉTIQUE (moyenne par locus):")
  avg_diversity <- colMeans(diversity_results$df_perloc[, c("Hs", "Ht", "Dst", "Fst")], 
                           na.rm = TRUE)
  for (stat in names(avg_diversity)) {
    message("- ", stat, ": ", round(avg_diversity[stat], 4))
  }
  
  message("\nF-STATISTIQUES (Weir & Cockerham):")
  if (is.matrix(f_statistics) && nrow(f_statistics) > 1) {
    avg_fstats <- colMeans(f_statistics, na.rm = TRUE)
    for (stat in names(avg_fstats)) {
      message("- ", stat, ": ", round(avg_fstats[stat], 4))
    }
  } else {
    for (i in seq_along(f_statistics)) {
      message("- ", colnames(f_statistics)[i], ": ", round(f_statistics[i], 4))
    }
  }
  
  message("\n" + strrep("=", 60))
}

generate_final_report(metadata, diversity_results, f_statistics)

# === SAUVEGARDE DES RÉSULTATS ===

save_analysis_results <- function(df_formatted, diversity_results, f_statistics, 
                                 genetic_objects, metadata) {
  results <- list(
    metadata = list(
      n_individuals = metadata$n_indv,
      n_populations = metadata$n_pop,
      n_markers = metadata$n_marker,
      populations = metadata$pops,
      missing_code = metadata$missing_data_code
    ),
    formatted_data = df_formatted,
    diversity_stats = diversity_results$df_perloc,
    f_statistics = f_statistics,
    genetic_objects = genetic_objects,
    timestamp = Sys.time()
  )
  
  save(results, file = "data/genetic_analysis_results.RData")
  message("✓ Résultats sauvegardés dans data/genetic_analysis_results.RData")
  
  # Export CSV des résultats principaux
  write.csv(diversity_results$df_perloc, 
            "data/diversity_statistics.csv", 
            row.names = TRUE)
  message("✓ Statistiques de diversité exportées dans data/diversity_statistics.csv")
  
  return(results)
}

final_results <- save_analysis_results(df_formatted, diversity_results, 
                                      f_statistics, genetic_objects, metadata)

# === FONCTIONS UTILITAIRES SUPPLÉMENTAIRES ===

# Résumé rapide des données
quick_summary <- function(formatted_data) {
  cat("Résumé rapide des données:\n")
  cat("-------------------------\n")
  cat("Individus:", length(formatted_data$individual), "\n")
  cat("Populations:", length(unique(formatted_data$Population)), "\n")
  cat("Marqueurs:", ncol(formatted_data$haplotype), "\n")
  cat("Ploidie:", formatted_data$ploidy, "\n")
  
  # Distribution des populations
  pop_table <- table(formatted_data$Population)
  cat("\nDistribution des populations:\n")
  print(pop_table)
}

# Vérification des données manquantes
check_missing_data <- function(formatted_data) {
  haplo_matrix <- as.matrix(formatted_data$haplotype)
  missing_count <- sum(haplo_matrix == formatted_data$missing_code | 
                       is.na(haplo_matrix))
  total_cells <- nrow(haplo_matrix) * ncol(haplo_matrix)
  missing_percent <- round(missing_count / total_cells * 100, 2)
  
  cat("Données manquantes:\n")
  cat("- Absolues:", missing_count, "/", total_cells, "\n")
  cat("- Pourcentage:", missing_percent, "%\n")
  
  return(missing_percent)
}

# Exécution des vérifications
quick_summary(formatted_data)
missing_percent <- check_missing_data(formatted_data)

message("\n=== ANALYSE TERMINÉE AVEC SUCCÈS ===")