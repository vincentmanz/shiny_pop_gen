# helper.R - Fonctions pour l'analyse de données génétiques
# Conservation de tous les noms de fonctions existants

# -------------------- IMPORTATION ET PRÉPARATION DES DONNÉES --------------------

# Corrected function for concatenate the alleles in columns (server_importdata)
concat_identical_cols <- function(df, ploidy) {
  # Get unique column names (removing .1, .2 suffixes if present)
  col_names <- colnames(df)
  unique_names <- unique(sub("\\.\\d+$", "", col_names))
  
  result <- data.frame()
  
  for (marker_name in unique_names) {
    # Find columns for this marker
    marker_cols <- which(grepl(paste0("^", marker_name, "(\\.\\d+)?$"), col_names))
    
    if (length(marker_cols) == 2) {
      # Two columns for this marker - concatenate alleles
      allele1 <- df[, marker_cols[1]]
      allele2 <- df[, marker_cols[2]]
      
      # Concatenate alleles with "/"
      concatenated <- paste(allele1, allele2, sep = "/")
      
      # Create temporary dataframe for this marker
      temp_df <- data.frame(concatenated)
      colnames(temp_df) <- marker_name
      
      # Add to result
      if (ncol(result) == 0) {
        result <- temp_df
      } else {
        result <- cbind(result, temp_df)
      }
    } else if (length(marker_cols) == 1) {
      # Only one column for this marker
      temp_df <- data.frame(df[, marker_cols[1]])
      colnames(temp_df) <- marker_name
      
      if (ncol(result) == 0) {
        result <- temp_df
      } else {
        result <- cbind(result, temp_df)
      }
    }
  }
  
  return(result)
}

# Define a function for rendering info boxes (server_importdata)
renderInfoBoxUI <- function(title, value, icon_name, color) {
  infoBox(
    title,
    value,
    icon = icon(icon_name),
    color = color,
    fill = TRUE
  )
}

# Define the function for the bootstrap (server_general_stats)
boot_fonction <- function(data, indices, columns) {
  subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
  subset_data <- adegenet::df2genind(
    X = as.matrix(subset_data),
    sep = "/",
    ncode = 6,
    ind.names = data$indv,
    pop = data$Population, # data Level
    NA.char = "0/0",
    ploidy = 2,
    type = "codom",
    strata = NULL,
    hierarchy = NULL
  )
  fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
  results_mat <- fst_results %>%
    select(Fis) %>% # Ajouter FIT, FST, FST-max, Hs et Ht
    as.matrix()
  return(results_mat)
}

col_to_rowname <- function(df, colname = "Row.names") {
  stopifnot(colname %in% names(df))
  rownames(df) <- df[[colname]]
  df[[colname]] <- NULL
  return(df)
}

format_numeric_cols <- function(df, digits = 5, exclude = "Locus") {
  for (nm in names(df)) {
    if (nm %in% exclude) next
    x <- df[[nm]]
    if (is.factor(x)) x <- as.character(x)
    if (!is.numeric(x)) {
      suppressWarnings(x_num <- as.numeric(x))
    } else {
      x_num <- x
    }
    if (is.numeric(x_num) && !all(is.na(x_num))) {
      df[[nm]] <- sprintf(paste0("%.", digits, "f"), x_num)
    } else {
      df[[nm]] <- x
    }
  }
  df
}

# -------------------- ÉQUILIBRE DE LIAISON (LINKAGE DISEQUILIBRIUM) --------------------

# Function to create contingency tables for each population
create_contingency_tables <- function(data, loci, include_missing = TRUE) {
  populations <- unique(data$Population)
  
  contingency_tables <- lapply(populations, function(pop) {
    pop_data <- data[data$Population == pop, ]
    locus_pairs <- combn(loci, 2, simplify = FALSE)
    
    contingency_list <- setNames(
      lapply(locus_pairs, function(pair) {
        locus1 <- pair[1]
        locus2 <- pair[2]
        
        if (!include_missing) {
          pop_data <- pop_data[pop_data[[locus1]] != "0/0" & pop_data[[locus2]] != "0/0", ]
        }
        
        haplotype_data <- data.frame(
          Locus1_haplotype = pop_data[[locus1]],
          Locus2_haplotype = pop_data[[locus2]]
        )
        
        table(haplotype_data$Locus1_haplotype, haplotype_data$Locus2_haplotype)
      }),
      sapply(locus_pairs, function(pair) paste(pair[1], pair[2], sep = "-"))
    )
    return(contingency_list)
  })
  names(contingency_tables) <- populations
  return(contingency_tables)
}

# Function to calculate G-statistic
calculate_g_stat <- function(contingency_table) {
  nt <- sum(contingency_table)
  row_sum <- rowSums(contingency_table)
  col_sum <- colSums(contingency_table)
  expected <- outer(row_sum, col_sum) / nt
  non_zero <- contingency_table > 0
  observed_non_zero <- contingency_table[non_zero]
  expected_non_zero <- expected[non_zero]
  g_stat <- 2 * sum(observed_non_zero * log(observed_non_zero / expected_non_zero), na.rm = TRUE)
  return(list(expected = expected, g_stat = g_stat))
}

# Function to add G-statistics to contingency tables
add_g_stats <- function(contingency_tables) {
  lapply(contingency_tables, function(pop_tables) {
    setNames(
      lapply(names(pop_tables), function(pair_name) {
        contingency_table <- pop_tables[[pair_name]]
        g_stat <- calculate_g_stat(contingency_table)
        list(
          contingency_table = contingency_table,
          expected_contingency_table = g_stat$expected,
          g_stat = g_stat$g_stat
        )
      }),
      gsub("[-`]+", ".", names(pop_tables))
    )
  })
}

# Function to calculate p-values
calculate_pvalues <- function(observed_g_stats, simulated_g_stats, epsilon = 1e-10) {
  results <- lapply(names(observed_g_stats), function(pop) {
    observed <- observed_g_stats[[pop]]
    simulated <- simulated_g_stats[[pop]]
    setNames(
      lapply(names(observed), function(pair) {
        observed_g <- observed[[pair]]$g_stat
        simulated_g <- simulated[[pair]][!is.na(simulated[[pair]])]
        p_value <- if (length(simulated_g) > 0) mean(simulated_g >= (observed_g - epsilon)) else NaN
        list(observed_g_stat = observed_g, p_value = p_value)
      }),
      names(observed)
    )
  })
  names(results) <- names(observed_g_stats)
  return(results)
}

# Function to calculate global p-values
calculate_global_pvalues <- function(observed_g_stats, simulated_g_stats) {
  locus_pairs <- unique(unlist(lapply(observed_g_stats, names)))
  sapply(locus_pairs, function(pair) {
    g_obs <- unlist(lapply(observed_g_stats, function(pop) pop[[pair]]$g_stat))
    g_sim <- unlist(lapply(simulated_g_stats, function(pop) pop[[pair]]))
    mean(g_sim >= mean(g_obs, na.rm = TRUE))
  })
}

# Function to create summary table
create_summary_table <- function(pvalues, global_pvalues) {
  all_pairs <- unique(unlist(lapply(pvalues, names)))
  summary_table <- data.frame(Locus_Pair = all_pairs)
  for (pop in names(pvalues)) {
    summary_table[[pop]] <- sapply(all_pairs, function(pair) pvalues[[pop]][[pair]]$p_value)
  }
  summary_table$Global_P_Value <- sapply(all_pairs, function(pair) global_pvalues[pair])
  return(summary_table)
}

# Function to generate randomized G-statistics
randomized_g_stats <- function(data, loci, n_simulations, calculate_g_stat, include_missing = TRUE) {
  workers <- parallel::detectCores() - 1
  cl <- makeCluster(workers)
  registerDoParallel(cl)
  clusterExport(cl, varlist = c("calculate_g_stat"), envir = environment())
  
  populations <- unique(data$Population)
  locus_pairs <- combn(loci, 2, simplify = FALSE)
  
  results <- foreach(pop = populations, .combine = 'c', .packages = 'dplyr') %dopar% {
    pop_data <- data[data$Population == pop, ]
    pop_results <- setNames(vector("list", length(locus_pairs)), sapply(locus_pairs, function(pair) gsub("[-`]+", ".", paste(pair[1], pair[2], sep = "."))))
    
    for (pair in locus_pairs) {
      locus1 <- pair[1]
      locus2 <- pair[2]
      g_stats <- numeric(n_simulations)
      for (i in 1:n_simulations) {
        randomized_data <- pop_data
        randomized_data[[locus1]] <- sample(pop_data[[locus1]])
        randomized_data[[locus2]] <- sample(pop_data[[locus2]])
        
        if (!include_missing) {
          randomized_data <- randomized_data[randomized_data[[locus1]] != "0/0" & randomized_data[[locus2]] != "0/0", ]
        }
        
        if (nrow(randomized_data) > 0) {
          contingency_table <- table(randomized_data[[locus1]], randomized_data[[locus2]])
          g_stats[i] <- calculate_g_stat(contingency_table)$g_stat
        }
      }
      pop_results[[gsub("[-`]+", ".", paste(locus1, locus2, sep = "."))]] <- g_stats
    }
    list(setNames(list(pop_results), pop))
  }
  
  stopCluster(cl)
  return(do.call(c, results))
}

# -------------------- FONCTIONS UTILITAIRES AMÉLIORÉES --------------------

#' Valide la structure des données génétiques
#' 
#' @param data DataFrame à valider
#' @param required_cols Colonnes requises
#' @return TRUE si valide, erreur sinon
validate_genetic_data <- function(data, required_cols = c("Population", "indv")) {
  if (!is.data.frame(data)) {
    stop("Les données doivent être un dataframe")
  }
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(data) == 0) {
    stop("Le dataframe est vide")
  }
  
  return(TRUE)
}

#' Sauvegarde les résultats avec horodatage
#' 
#' @param object Objet à sauvegarder
#' @param prefix Préfixe du fichier
#' @param path Chemin de sauvegarde
save_results <- function(object, prefix = "results", path = ".") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(path, paste0(prefix, "_", timestamp, ".rds"))
  saveRDS(object, filename)
  return(filename)
}

#' Formate un nombre avec un nombre fixe de décimales
#' 
#' @param x Nombre à formater
#' @param digits Nombre de décimales
#' @return Chaîne formatée
format_fixed <- function(x, digits = 5) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f"), x))
}

#' Calcule les statistiques descriptives de base
#' 
#' @param x Vecteur numérique
#' @return Liste avec moyenne, écart-type, etc.
basic_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(mean = NA, sd = NA, n = 0))
  
  list(
    mean = mean(x),
    sd = sd(x),
    n = length(x),
    min = min(x),
    max = max(x),
    median = median(x)
  )
}

#' Vérifie si un package est installé et le charge
#' 
#' @param package Nom du package
#' @param load Si TRUE, charge le package
#' @return TRUE si le package est disponible
check_package <- function(package, load = TRUE) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Le package '", package, "' n'est pas installé")
  }
  if (load) {
    library(package, character.only = TRUE)
  }
  return(TRUE)
}

#' Crée un graphique ggplot2 standardisé
#' 
#' @param data Données pour le graphique
#' @param x Variable x
#' @param y Variable y
#' @param type Type de graphique ("point", "bar", "line")
#' @return Objet ggplot
create_standard_plot <- function(data, x, y, type = "point") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis")
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))
  
  switch(type,
         "point" = p + ggplot2::geom_point(),
         "bar" = p + ggplot2::geom_bar(stat = "identity"),
         "line" = p + ggplot2::geom_line(),
         p + ggplot2::geom_point()  # défaut
  )
}

#' Normalise les valeurs entre 0 et 1
#' 
#' @param x Vecteur numérique
#' @return Vecteur normalisé
normalize <- function(x) {
  if (all(is.na(x))) return(x)
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' Calcule la fréquence allélique à partir d'un génotype
#' 
#' @param genotype Vecteur de génotypes au format "A/B"
#' @return DataFrame avec fréquences alléliques
calculate_allele_freq <- function(genotype) {
  # Séparer les allèles
  alleles <- unlist(strsplit(genotype, "/"))
  alleles <- alleles[!alleles %in% c("0", "NA", "")]
  
  if (length(alleles) == 0) return(data.frame(allele = character(), freq = numeric()))
  
  freq_table <- table(alleles)
  data.frame(
    allele = names(freq_table),
    freq = as.numeric(freq_table) / length(alleles),
    stringsAsFactors = FALSE
  )
}

#' Convertit un objet genind en dataframe pour l'affichage
#' 
#' @param genind_obj Objet genind
#' @return DataFrame formaté
genind_to_dataframe <- function(genind_obj) {
  if (!inherits(genind_obj, "genind")) {
    stop("L'objet doit être de classe genind")
  }
  
  # Extraire les données de génotype
  gen_data <- as.data.frame(genind_obj@tab)
  
  # Ajouter les métadonnées
  if (!is.null(genind_obj@pop)) {
    gen_data$Population <- genind_obj@pop
  }
  if (!is.null(genind_obj@ind.names)) {
    gen_data$Individual <- genind_obj@ind.names
  }
  
  return(gen_data)
}

#' Calcule la diversité génétique de base par population
#' 
#' @param hierfstat_data Données au format hierfstat
#' @return Liste avec statistiques de diversité
calculate_diversity_stats <- function(hierfstat_data) {
  if (!requireNamespace("hierfstat", quietly = TRUE)) {
    stop("Le package hierfstat est requis")
  }
  
  basic_stats <- hierfstat::basic.stats(hierfstat_data)
  
  list(
    Hs = basic_stats$Hs,  # Hétérozygotie observée
    Ho = basic_stats$Ho,  # Hétérozygotie attendue
    Fis = basic_stats$Fis, # Coefficient de consanguinité
    per_loc = basic_stats$perloc,
    overall = basic_stats$overall
  )
}

#' Version améliorée de concat_identical_cols avec gestion d'erreurs
concat_identical_cols_enhanced <- function(df, ploidy) {
  tryCatch({
    # Validation des entrées
    if (!is.data.frame(df)) stop("L'entrée doit être un dataframe")
    if (ncol(df) == 0) stop("Le dataframe est vide")
    
    result <- concat_identical_cols(df, ploidy)
    
    # Vérification du résultat
    if (ncol(result) == 0) {
      warning("Aucune colonne n'a été concaténée")
    }
    
    return(result)
    
  }, error = function(e) {
    stop("Erreur dans concat_identical_cols: ", e$message)
  })
}

#' Version sécurisée de boot_fonction
boot_fonction_safe <- function(data, indices, columns) {
  tryCatch({
    # Validation des entrées
    if (!is.data.frame(data)) stop("data doit être un dataframe")
    if (length(indices) == 0) stop("indices ne peut pas être vide")
    if (length(columns) == 0) stop("columns ne peut pas être vide")
    
    result <- boot_fonction(data, indices, columns)
    return(result)
    
  }, error = function(e) {
    warning("Erreur dans boot_fonction: ", e$message)
    return(matrix(NA, nrow = length(columns), ncol = 1))
  })
}