# server_general_stats_complete.R

library(promises)
library(future)

# Load the saved RData object
load("data/formatted_data.RData")

# # Source C++ files
# Rcpp::sourceCpp("www/permute.cpp")
# Rcpp::sourceCpp("www/fis_bootstrap.cpp")
# Rcpp::sourceCpp("www/permute_fit.cpp")
# Rcpp::sourceCpp("www/boot_per_locus_two_stage_wc.cpp")
# Rcpp::sourceCpp("www/boot_wc_pop_only_fast.cpp")

# ===== CHARGEMENT DES FONCTIONS C++ =====
.load_all_cpp_functions <- function() {
  cpp_files <- c(
    "www/boot_per_locus_two_stage_wc.cpp", 
    "www/boot_wc_pop_only_fast.cpp",
    "www/permute.cpp",
    "www/fis_bootstrap.cpp"
  )
  
  for (file in cpp_files) {
    if (file.exists(file)) {
      tryCatch({
        message("Loading C++ file: ", file)
        Rcpp::sourceCpp(file)
      }, error = function(e) {
        warning("Failed to load ", file, ": ", e$message)
      })
    } else {
      warning("File not found: ", file)
    }
  }
}

# Appeler cette fonction au début du server
.load_all_cpp_functions()

filtered_data <- data.frame(
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
pops <- unique(formatted_data$Population)
n_indv <- nrow(formatted_data$haplotype)
num_cores <- parallel::detectCores()
missing_data_code <- formatted_data$missing_code
nloci <- length(unique(colnames(formatted_data$haplotype)))
loci_names <- unique(colnames(formatted_data$haplotype))

# Define the server logic
server_general_stats <- function(input, output, session) {
  
  # ===== REACTIVE VALUES =====

  # Basic stats
  result_stats_reactive <- reactiveVal(NULL)
  missing_data_reac <- reactiveVal(NULL)
  filtered_data_reac <- reactiveVal(NULL)
  plot_output <- reactiveValues()
  mydata_genind_reac <- reactiveVal(NULL)
  mydata_hierfstat_reac <- reactiveVal(NULL)
  result_stats_download <- reactiveVal(NULL)
  result_stats_numeric_reactive <- reactiveVal(NULL)
  
  # Bootstrap FIS Analysis
  fis_boot_results <- reactiveVal(NULL)
  fis_boot_timing <- reactiveVal(NULL)
  perm_results <- reactiveVal(NULL)

  # Pour les stats "Overall"
  result_stats_select_reactive <- reactiveVal(NULL)
  
  # ===== DATA PREPARATION =====
  
  # Compute genind object
  mydata_genind <- adegenet::df2genind(
    X = as.matrix(formatted_data$haplotype), 
    sep = "/", 
    ncode = 6, 
    ind.names = formatted_data$individual, 
    pop = formatted_data$Population, 
    NA.char = formatted_data$missing_code, 
    ploidy = formatted_data$ploidy, 
    type = "codom", 
    strata = NULL, 
    hierarchy = NULL
  )
  mydata_genind_reac(mydata_genind)
  
  # Compute hierfstat object
  mydata_hierfstat <- genind2hierfstat(mydata_genind)
  mydata_hierfstat_reac(mydata_hierfstat)
  
  # Create the missing data df
  missing_data <- info_table(mydata_genind, plot = FALSE, percent = TRUE, df = TRUE)
  missing_data <- as.data.frame(missing_data) %>% 
    spread(key = Locus, value = Missing)
  missing_data <- missing_data %>% 
    column_to_rownames(var = "Population")
  missing_data <- missing_data * 100
  missing_data_reac(missing_data)
  
  # ===== HELPER FUNCTIONS =====

  # Calculer les stats génétiques par population
  calculate_pop_stats <- function(pop_name, formatted_data) {
    # Extraire les génotypes de cette population
    pop_indices <- which(formatted_data$Population == pop_name)
    if (length(pop_indices) == 0) return(NULL)
    
    pop_haplo <- formatted_data$haplotype[pop_indices, , drop = FALSE]
    n_ind <- nrow(pop_haplo)
    n_loci <- ncol(pop_haplo)
    
    Ho_vals <- numeric(n_loci)
    Hs_vals <- numeric(n_loci)
    Fis_Nei_vals <- numeric(n_loci)
    
    for (j in 1:n_loci) {
      genotypes <- pop_haplo[, j]
      # Retirer les manquants
      genotypes <- genotypes[!is.na(genotypes) & genotypes != formatted_data$missing_code]
      if (length(genotypes) == 0) {
        Ho_vals[j] <- NA
        Hs_vals[j] <- NA
        Fis_Nei_vals[j] <- NA
        next
      }
      
      # Compter les homozygotes observés → Ho
      hom_count <- sum(grepl("^([0-9]+)/\\1$", genotypes))  # ex: "1/1", "2/2"
      Ho_vals[j] <- hom_count / length(genotypes)
      
      # Calculer les fréquences alléliques
      alleles <- unlist(strsplit(genotypes, "/"))
      allele_freq <- table(alleles) / length(alleles)
      # Hs = 1 - somme(pi^2)
      Hs_vals[j] <- 1 - sum(allele_freq^2)
      
      # Fis (Nei) = 1 - Ho/Hs
      Fis_Nei_vals[j] <- if (Hs_vals[j] > 0) 1 - Ho_vals[j] / Hs_vals[j] else NA_real_
    }
    
    df <- data.frame(
      Locus = colnames(pop_haplo),
      Ho = Ho_vals,
      Hs = Hs_vals,
      `Fis (Nei)` = Fis_Nei_vals,
      stringsAsFactors = FALSE
    )
    
    return(df)
  }

  # ===== GENE DIVERSITY BY POPULATION TABLE =====
  # Calculer Hs par locus et par population
  calculate_hs_by_pop_locus <- function(formatted_data) {
    pops <- unique(formatted_data$Population)
    loci <- colnames(formatted_data$haplotype)
    result_matrix <- matrix(NA_real_, nrow = length(loci), ncol = length(pops))
    rownames(result_matrix) <- loci
    colnames(result_matrix) <- pops
    
    for (pop in pops) {
      pop_indices <- which(formatted_data$Population == pop)
      if (length(pop_indices) == 0) next
      pop_haplo <- formatted_data$haplotype[pop_indices, , drop = FALSE]
      
      for (j in seq_along(loci)) {
        locus_name <- loci[j]
        genotypes <- pop_haplo[, locus_name]
        genotypes <- genotypes[!is.na(genotypes) & genotypes != formatted_data$missing_code]
        if (length(genotypes) == 0) {
          result_matrix[locus_name, pop] <- NA_real_
          next
        }
        # Extraire les allèles
        alleles <- unlist(strsplit(genotypes, "/"))
        # Fréquences alléliques
        allele_freq <- table(alleles) / length(alleles)
        # Hs = 1 - sum(pi^2)
        Hs_val <- 1 - sum(allele_freq^2)
        result_matrix[locus_name, pop] <- Hs_val
      }
    }
    return(as.data.frame(result_matrix))
  }

  # Output du tableau
  output$gene_diversity_table <- DT::renderDT({
    req(formatted_data)
    df <- calculate_hs_by_pop_locus(formatted_data)
    # Formater les nombres à 3 décimales
    df_rounded <- df
    numeric_cols <- sapply(df_rounded, is.numeric)
    df_rounded[numeric_cols] <- lapply(df_rounded[numeric_cols], round, 3)
    
    DT::datatable(
      df_rounded,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = TRUE
    ) %>%
      DT::formatStyle(
        columns = colnames(df_rounded),
        backgroundColor = DT::styleColorBar(range(df_rounded, na.rm = TRUE), "lightblue"),
        color = "black"
      )
  })

  # Téléchargement CSV
  output$download_gene_diversity <- downloadHandler(
    filename = function() paste0("gene_diversity_by_pop_", Sys.Date(), ".csv"),
    content = function(file) {
      req(formatted_data)
      df <- calculate_hs_by_pop_locus(formatted_data)
      write.csv(df, file, row.names = TRUE)
    }
  )

  # Téléchargement TXT (tabulé)
  output$download_gene_diversity_txt <- downloadHandler(
    filename = function() paste0("gene_diversity_by_pop_", Sys.Date(), ".txt"),
    content = function(file) {
      req(formatted_data)
      df <- calculate_hs_by_pop_locus(formatted_data)
      write.table(df, file, sep = "\t", row.names = TRUE, quote = FALSE)
    }
  )

  # Convertir genind → data.frame avec génotypes "1/1", "1/2", etc.
  genind_to_hierfstat_text <- function(genind_obj) {
    if (is.null(genind_obj) || !inherits(genind_obj, "genind")) {
      stop("Input must be a valid genind object")
    }
    
    # Extraire les génotypes sous forme de matrice de caractères
    gt_matrix <- as.matrix(genind_obj)
    # Vérifier que c'est bien du texte
    if (!is.character(gt_matrix)) {
      gt_matrix <- apply(gt_matrix, c(1,2), as.character)
    }
    
    # Construire le data.frame
    df <- data.frame(
      pop = as.character(pop(genind_obj)),
      gt_matrix,
      stringsAsFactors = FALSE
    )
    
    return(df)
  }
  
  # Fonction de recodage robuste pour FST-max
  recode_for_fstmax <- function(hierfstat_data) {
    pops <- unique(hierfstat_data[,1])
    loci <- colnames(hierfstat_data)[-1]
    new_data <- hierfstat_data
    
    allele_counter <- 1
    
    for(pop in pops) {
      pop_rows <- which(hierfstat_data[,1] == pop)
      
      for(loc in loci) {
        genotypes <- hierfstat_data[pop_rows, loc]
        alleles <- unique(unlist(strsplit(as.character(na.omit(genotypes[genotypes != "0/0"])), "/")))
        alleles <- alleles[!is.na(alleles) & alleles != "0"]
        
        if(length(alleles) > 0) {
          alleles <- sort(as.numeric(alleles))
          allele_map <- setNames(
            sprintf("%03d", allele_counter:(allele_counter + length(alleles) - 1)),
            as.character(alleles)
          )
          allele_counter <- allele_counter + length(alleles)
          
          for(i in pop_rows) {
            current_gt <- as.character(hierfstat_data[i, loc])
            if(!is.na(current_gt) && current_gt != "0/0") {
              gt_alleles <- strsplit(current_gt, "/")[[1]]
              new_gt <- paste(allele_map[gt_alleles], collapse = "/")
              new_data[i, loc] <- new_gt
            }
          }
        }
      }
    }
    
    for(loc in loci) {
      new_data[,loc] <- ifelse(new_data[,loc] == "0/0", NA, new_data[,loc])
      new_data[,loc] <- suppressWarnings(as.numeric(gsub("/", "", new_data[,loc])))
    }
    
    return(new_data)
  }
  
  # Format numeric columns helper
  format_numeric_cols <- function(df, digits = 5, exclude = character(0)) {
    result <- df
    for(col in colnames(df)) {
      if(col %in% exclude) next
      if(is.numeric(df[[col]])) {
        result[[col]] <- sprintf(paste0("%.", digits, "f"), df[[col]])
      }
    }
    return(result)
  }

  # Fonction pour préparer la matrice pour C++ - VERSION CORRIGÉE
  prepare_matrix_for_cpp <- function(hierfstat_data) {
    hf <- as.data.frame(hierfstat_data)
    
    # Conversion robuste de la colonne population
    if (!is.integer(hf[[1]])) {
      # Convertir en facteur puis en entier pour éviter les problèmes
      pop_factor <- as.factor(hf[[1]])
      hf[[1]] <- as.integer(pop_factor)
    }
    
    # Conversion robuste des génotypes
    for (j in 2:ncol(hf)) {
      if (!is.integer(hf[[j]])) {
        # Nettoyer et convertir en numérique d'abord
        geno_char <- as.character(hf[[j]])
        geno_numeric <- suppressWarnings(as.numeric(geno_char))
        # Remplacer les NA par 0 (code manquant)
        geno_numeric[is.na(geno_numeric)] <- 0L
        hf[[j]] <- as.integer(geno_numeric)
      }
    }
    
    colnames(hf)[1] <- "pop"
    
    # Vérifier que toutes les colonnes sont bien des entiers
    for (j in 1:ncol(hf)) {
      if (!is.integer(hf[[j]])) {
        hf[[j]] <- as.integer(hf[[j]])
      }
    }
    
    mat_int <- as.matrix(hf)
    storage.mode(mat_int) <- "integer"  # Forcer le type integer
    
    # Vérification finale
    if (storage.mode(mat_int) != "integer") {
      storage.mode(mat_int) <- "integer"
    }
    
    return(mat_int)
  }

  # ===== FONCTIONS DE SECOURS =====

  # Fonction de secours pour calculate_observed_fis
  safe_calculate_observed_fis <- function(hierfstat_data) {
    tryCatch({
      # Essayer d'abord la fonction C++
      mat_int <- prepare_matrix_for_cpp(hierfstat_data)
      result <- calculate_observed_fis(mat_int, pop_col = 0, missing_code = 0)
      return(result)
    }, error = function(e) {
      # Fallback vers hierfstat
      hf_df <- as.data.frame(hierfstat_data)
      result <- hierfstat::wc(hf_df)$per.loc[, "FIS"]
      names(result) <- colnames(hf_df)[-1]
      return(result)
    })
  }

  # Fonction de secours pour summarize_fis_results
  safe_summarize_fis_results <- function(boot_mat, conf_level = 0.95) {
    tryCatch({
      # Essayer avec l'argument confidence
      result <- summarize_fis_results(boot_mat, confidence = conf_level)
      return(result)
    }, error = function(e) {
      # Essayer sans l'argument
      tryCatch({
        result <- summarize_fis_results(boot_mat)
        return(result)
      }, error = function(e2) {
        # Fallback manuel
        alpha <- (1 - conf_level) / 2
        results <- list()
        
        for (i in 1:ncol(boot_mat)) {
          locus_data <- boot_mat[, i]
          locus_data <- locus_data[!is.na(locus_data)]
          
          if (length(locus_data) > 0) {
            results[[colnames(boot_mat)[i]]] <- list(
              mean_fis = mean(locus_data),
              lower_ci = quantile(locus_data, alpha),
              upper_ci = quantile(locus_data, 1 - alpha),
              n_valid = length(locus_data)
            )
          }
        }
        return(results)
      })
    })
  }

  # Fonction pour l'analyse bootstrap FIS - VERSION CORRIGÉE
  run_bootstrap_fis_analysis <- function(formatted_data, n_perm, n_boot, conf_level) {
    set.seed(123)
    RNGkind("Mersenne-Twister")
    
    if (is.null(formatted_data) || is.null(formatted_data$haplotype)) {
      stop("Invalid formatted_data structure")
    }
    
    tryCatch({
      # Création des objets génétiques
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
      
      if (length(loci_names) == 0) {
        stop("No loci found in the data")
      }
      
      # FIS observés
      obs_wc_hier <- hierfstat::wc(mydata_hierfstat)
      obs_fis_wc_by_locus <- as.numeric(obs_wc_hier$per.loc[, "FIS"])
      obs_fis_wc_overall <- mean(obs_fis_wc_by_locus, na.rm = TRUE)
      
      # Préparation matrice C++ avec la fonction corrigée
      hf <- as.data.frame(mydata_hierfstat)
      mat_int <- prepare_matrix_for_cpp(hf)
      
      # Tests de permutation
      set.seed(123)
      perm_fis_by_locus <- matrix(NA, nrow = n_perm, ncol = length(loci_names))
      colnames(perm_fis_by_locus) <- loci_names
      
      for (b in 1:n_perm) {
        tryCatch({
          result <- batch_permute_wc_fis(mat_int, 1, 0, 1)
          if (is.numeric(result$permutations) && nrow(result$permutations) > 0) {
            perm_fis_by_locus[b, ] <- as.numeric(result$permutations[1, ])
          }
        }, error = function(e) {
          # En cas d'erreur, on laisse les valeurs NA
        })
      }
      
      # Calcul P-values
      safe_p_two <- function(obs, perm_vec) {
        if (is.na(obs) || length(perm_vec) == 0) return(NA_real_)
        v <- perm_vec[!is.na(perm_vec)]
        n_valid <- length(v)
        if (n_valid == 0) return(NA_real_)
        p_upper <- mean(v >= obs)
        p_lower <- mean(v <= obs)
        p_value <- 2 * min(p_upper, p_lower)
        return(min(1.0, p_value))
      }
      
      pval_fis_by_locus <- numeric(length(loci_names))
      for (j in seq_along(loci_names)) {
        pval_fis_by_locus[j] <- safe_p_two(obs_fis_wc_by_locus[j], perm_fis_by_locus[, j])
      }
      names(pval_fis_by_locus) <- loci_names
      
      # P-value globale
      perm_fis_overall <- apply(perm_fis_by_locus, 1, mean, na.rm = TRUE)
      pval_fis_overall <- safe_p_two(obs_fis_wc_overall, perm_fis_overall)
      
      # Bootstrap individuel - Préparer hf correctement
      set.seed(3)
      hf_prepared <- as.data.frame(mydata_hierfstat)
      hf_prepared[[1]] <- as.integer(as.factor(hf_prepared[[1]]))
      for (j in 2:ncol(hf_prepared)) {
        hf_prepared[[j]] <- as.integer(hf_prepared[[j]])
      }
      
      boot_indiv_mat <- boot_indiv_wc_fis(as.matrix(hf_prepared), pop_col = 0L, missing_code = 0L, B = n_boot)
      
      # Calcul IC
      alpha <- (1 - conf_level) / 2
      safe_quantile <- function(x, probs, na.rm = TRUE) {
        if (!is.numeric(x) || all(is.na(x))) return(NA_real_)
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0) return(NA_real_)
        return(quantile(x_clean, probs = probs, na.rm = na.rm, type = 7))
      }
      
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
      set.seed(2)
      bootP_result <- boot_popblock_wc_fis(
        as.matrix(hf_prepared),
        pop_col = 0L,
        missing_code = 0L,
        B = n_boot,
        return_locus_matrix = FALSE
      )
      
      overall_ci <- c(
        safe_quantile(bootP_result$overall, probs = alpha),
        safe_quantile(bootP_result$overall, probs = 1 - alpha)
      )
      
      # Construction du tableau final
      perm_tbl <- data.frame(
        Locus = loci_names,
        Observed_FIS = obs_fis_wc_by_locus,
        P_value = pval_fis_by_locus,
        stringsAsFactors = FALSE
      )
      
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
      
    }, error = function(e) {
      stop("Error in bootstrap analysis: ", e$message)
    })
  }
  
  # ===== BASIC STATISTICS =====

  observeEvent(input$run_basic_stats, {
    mydata_hierfstat_a <- mydata_hierfstat_reac()
    mydata_genind_a <- mydata_genind_reac()
    
    selected_stats <- c(
      "Ho" = input$ho_checkbox,
      "Hs" = input$hs_checkbox,
      "Ht" = input$ht_checkbox,
      "Fit (W&C)" = input$fit_wc_checkbox,
      "Fis (W&C)" = input$fis_wc_checkbox,
      "Fst (W&C)" = input$fst_wc_checkbox,
      "Fis (Nei)" = input$fis_n_checkbox,
      "Fst (Nei)" = input$fst_n_checkbox,
      "Fst-max" = input$fst_max_checkbox,
      "Fst'" = input$fst_prim_checkbox,
      "GST" = input$GST_checkbox,
      "GST''" = input$GST_sec_checkbox
    )
    
    if (!any(selected_stats)) {
      shinyalert("No Statistics Selected", "Please select at least one statistic to compute.", type = "warning")
      return(NULL)
    }
    
    # Calcul des statistiques de base
    result <- basic.stats(mydata_hierfstat_a)
    df_result_basic <- as.data.frame(result$perloc)
    
    # Calcul des F-statistiques de Weir & Cockerham
    data <- as.data.frame(as.loci(mydata_genind_a))
    result_f_stats <- Fst(as.loci(data))
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
    
    # Ajout des statistiques G
    df_result_basic <- df_result_basic %>% mutate(
      GST = 1 - Hs / Ht,
      `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
    )

    # Calcul FST-max si demandé
    fst_max_values <- NULL
    if (input$fst_max_checkbox || input$fst_prim_checkbox) {
      if ("Hs" %in% colnames(df_result_basic)) {
        fst_max_values <- 1 - df_result_basic$Hs
        # Éviter les valeurs non valides
        fst_max_values[fst_max_values <= 0 | is.na(fst_max_values)] <- NA_real_
        
        if (input$fst_max_checkbox) {
          df_result_basic$`Fst-max` <- fst_max_values
        }
      } else {
        showNotification("Hs not available: cannot compute Fst-max or Fst'.", type = "error")
        fst_max_values <- rep(NA, nrow(df_result_basic))
      }
    }

    # Calcul Fst' si demandé
    if (input$fst_prim_checkbox) {
      if (!is.null(fst_max_values) && "Fst (W&C)" %in% colnames(result_f_stats)) {
        fst_wc_values <- result_f_stats[, "Fst (W&C)"]
        fst_prime_values <- fst_wc_values / fst_max_values
        fst_prime_values <- pmin(pmax(fst_prime_values, 0, na.rm = TRUE), 1, na.rm = TRUE)
        df_result_basic$`Fst'` <- fst_prime_values
      } else {
        df_result_basic$`Fst'` <- NA
      }
    }
    
    # Fusion des résultats
    result_stats <- merge(result_f_stats, df_result_basic, by = "row.names", all.x = TRUE)
    
    # Renommage des colonnes Nei
    available_cols <- colnames(result_stats)
    if("Fst" %in% available_cols && "Fis" %in% available_cols) {
      fst_nei_col <- which(available_cols == "Fst" & !available_cols %in% c("Fst (W&C)"))
      fis_nei_col <- which(available_cols == "Fis" & !available_cols %in% c("Fis (W&C)"))
      
      if(length(fst_nei_col) > 0) colnames(result_stats)[fst_nei_col[1]] <- "Fst (Nei)"
      if(length(fis_nei_col) > 0) colnames(result_stats)[fis_nei_col[1]] <- "Fis (Nei)"
    }
    
    result_stats <- result_stats %>% column_to_rownames("Row.names")
    result_stats_reactive(result_stats)
    
    # Filtrer les statistiques sélectionnées
    available_stats <- intersect(names(selected_stats), colnames(result_stats))
    selected_available_stats <- available_stats[selected_stats[available_stats]]
    
    if (length(selected_available_stats) > 0) {
      result_stats_select <- result_stats[, selected_available_stats, drop = FALSE]
      result_stats_select$Locus <- rownames(result_stats_select)
      result_stats_select <- result_stats_select[, c("Locus", setdiff(names(result_stats_select), "Locus"))]
      
      result_stats_numeric_reactive(result_stats_select)
      result_stats_display <- format_numeric_cols(result_stats_select, digits = 3, exclude = "Locus")
      result_stats_download(result_stats_display)

      result_stats_select_reactive(result_stats_select)
      
      # Mise à jour des sorties
      output$basic_stats_table <- DT::renderDT({
        DT::datatable(result_stats_display, 
                     options = list(pageLength = 10, scrollX = TRUE),
                     rownames = FALSE)
      })
      
      output$basic_stats_overall <- renderTable({
        overall_stats <- data.frame(
          Statistic = c("Number of Loci", "Number of Populations", "Number of Individuals"),
          Value = c(nloci, n_pop, n_indv)
        )
        overall_stats
      }, rownames = FALSE)
      
      output$basic_stats_by_pop <- DT::renderDT({
        pop_stats <- data.frame(
          Population = pops,
          Sample_Size = as.numeric(table(formatted_data$Population))
        )
        DT::datatable(pop_stats, options = list(pageLength = 10))
      })
      
    } else {
      showNotification("No valid statistics to display", type = "warning")
      result_stats_download(NULL)
      result_stats_numeric_reactive(NULL)
    }
  })

  output$basic_stats_by_pop_selected <- DT::renderDT({
    req(input$selected_pop_overall, formatted_data)
    
    pop_name <- input$selected_pop_overall
    df <- calculate_pop_stats(pop_name, formatted_data)
    
    if (is.null(df)) {
      return(DT::datatable(data.frame(Message = paste("No data available for population:", pop_name))))
    }
    
    # Formater les nombres
    df_display <- df
    numeric_cols <- sapply(df, is.numeric)
    df_display[numeric_cols] <- lapply(df_display[numeric_cols], round, 5)
    
    DT::datatable(
      df_display,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Mettre à jour la liste des populations dans "By Population"
  observe({
    req(formatted_data)
    pops <- unique(formatted_data$Population)
    updateSelectInput(session, "selected_pop_overall", choices = pops, selected = pops[1])
  })

  # ===== SECTION 1: DOWNLOAD HANDLERS FOR BASIC STATS =====

  # 1. Population-specific stats
  output$download_pop_stats <- downloadHandler(
    filename = function() paste0("pop_stats_", input$selected_pop_overall, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$selected_pop_overall, mydata_genind_reac())
      df <- calculate_pop_stats(input$selected_pop_overall, mydata_genind_reac())
      if (is.null(df)) {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )

  output$download_pop_stats_txt <- downloadHandler(
    filename = function() paste0("pop_stats_", input$selected_pop_overall, "_", Sys.Date(), ".txt"),
    content = function(file) {
      req(input$selected_pop_overall, mydata_genind_reac())
      df <- calculate_pop_stats(input$selected_pop_overall, mydata_genind_reac())
      if (is.null(df)) {
        write.table(data.frame(Message = "No data available"), file, sep = "\t", row.names = FALSE, quote = FALSE)
      } else {
        write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    }
  )

  # 2. Global Averages
  output$download_global_avg <- downloadHandler(
    filename = function() paste0("global_averages_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_stats_select_reactive())
      df <- result_stats_select_reactive()
      numeric_cols <- setdiff(colnames(df), "Locus")
      global_avg <- colMeans(df[, numeric_cols, drop = FALSE], na.rm = TRUE)
      result_df <- data.frame(Statistic = names(global_avg), Value = global_avg)
      write.csv(result_df, file, row.names = FALSE)
    }
  )

  output$download_global_avg_txt <- downloadHandler(
    filename = function() paste0("global_averages_", Sys.Date(), ".txt"),
    content = function(file) {
      req(result_stats_select_reactive())
      df <- result_stats_select_reactive()
      numeric_cols <- setdiff(colnames(df), "Locus")
      global_avg <- colMeans(df[, numeric_cols, drop = FALSE], na.rm = TRUE)
      result_df <- data.frame(Statistic = names(global_avg), Value = global_avg)
      write.table(result_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # 3. Overall by Population
  output$download_overall_by_pop <- downloadHandler(
    filename = function() paste0("overall_by_population_", Sys.Date(), ".csv"),
    content = function(file) {
      req(formatted_data)
      pops <- unique(formatted_data$Population)
      haplo <- formatted_data$haplotype
      pop_vec <- formatted_data$Population
      results_list <- lapply(pops, function(pop_name) {
        idx <- which(pop_vec == pop_name)
        if (length(idx) == 0) return(NULL)
        pop_haplo <- haplo[idx, , drop = FALSE]
        n_loci <- ncol(pop_haplo)
        Ho_vals <- Hs_vals <- numeric(n_loci)
        for (j in 1:n_loci) {
          genotypes <- pop_haplo[, j]
          genotypes <- genotypes[!is.na(genotypes) & genotypes != formatted_data$missing_code]
          if (length(genotypes) == 0) {
            Ho_vals[j] <- Hs_vals[j] <- NA
            next
          }
          hom_count <- sum(grepl("^([0-9]+)/\\1$", genotypes))
          Ho_vals[j] <- hom_count / length(genotypes)
          alleles <- unlist(strsplit(genotypes, "/"))
          allele_freq <- table(alleles) / length(alleles)
          Hs_vals[j] <- 1 - sum(allele_freq^2)
        }
        Ho_mean <- mean(Ho_vals, na.rm = TRUE)
        Hs_mean <- mean(Hs_vals, na.rm = TRUE)
        Fis_Nei_mean <- if (Hs_mean > 0) 1 - Ho_mean / Hs_mean else NA_real_
        data.frame(
          Population = pop_name,
          Ho = round(Ho_mean, 5),
          Hs = round(Hs_mean, 5),
          `Fis (Nei)` = round(Fis_Nei_mean, 5),
          stringsAsFactors = FALSE
        )
      })
      results_df <- do.call(rbind, results_list)
      write.csv(results_df, file, row.names = FALSE)
    }
  )

  output$download_overall_by_pop_txt <- downloadHandler(
    filename = function() paste0("overall_by_population_", Sys.Date(), ".txt"),
    content = function(file) {
      req(formatted_data)
      pops <- unique(formatted_data$Population)
      haplo <- formatted_data$haplotype
      pop_vec <- formatted_data$Population
      results_list <- lapply(pops, function(pop_name) {
        idx <- which(pop_vec == pop_name)
        if (length(idx) == 0) return(NULL)
        pop_haplo <- haplo[idx, , drop = FALSE]
        n_loci <- ncol(pop_haplo)
        Ho_vals <- Hs_vals <- numeric(n_loci)
        for (j in 1:n_loci) {
          genotypes <- pop_haplo[, j]
          genotypes <- genotypes[!is.na(genotypes) & genotypes != formatted_data$missing_code]
          if (length(genotypes) == 0) {
            Ho_vals[j] <- Hs_vals[j] <- NA
            next
          }
          hom_count <- sum(grepl("^([0-9]+)/\\1$", genotypes))
          Ho_vals[j] <- hom_count / length(genotypes)
          alleles <- unlist(strsplit(genotypes, "/"))
          allele_freq <- table(alleles) / length(alleles)
          Hs_vals[j] <- 1 - sum(allele_freq^2)
        }
        Ho_mean <- mean(Ho_vals, na.rm = TRUE)
        Hs_mean <- mean(Hs_vals, na.rm = TRUE)
        Fis_Nei_mean <- if (Hs_mean > 0) 1 - Ho_mean / Hs_mean else NA_real_
        data.frame(
          Population = pop_name,
          Ho = round(Ho_mean, 5),
          Hs = round(Hs_mean, 5),
          `Fis (Nei)` = round(Fis_Nei_mean, 5),
          stringsAsFactors = FALSE
        )
      })
      results_df <- do.call(rbind, results_list)
      write.table(results_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # 4. Overall by Locus
  output$download_overall_by_locus <- downloadHandler(
    filename = function() paste0("overall_by_locus_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_stats_select_reactive())
      df <- result_stats_select_reactive()
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_overall_by_locus_txt <- downloadHandler(
    filename = function() paste0("overall_by_locus_", Sys.Date(), ".txt"),
    content = function(file) {
      req(result_stats_select_reactive())
      df <- result_stats_select_reactive()
      write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # ===== OVERALL STATISTICS

  output$overall_global_avg <- renderTable({
    req(result_stats_select_reactive())
    df <- result_stats_select_reactive()
    numeric_cols <- setdiff(colnames(df), "Locus")
    if (length(numeric_cols) == 0) return(NULL)
    global_avg <- colMeans(df[, numeric_cols, drop = FALSE], na.rm = TRUE)
    data.frame(
      Statistic = names(global_avg),
      `Global Mean` = round(global_avg, 5),
      row.names = NULL
    )
  }, rownames = FALSE)

  output$overall_by_locus <- renderTable({
    req(result_stats_select_reactive())
    df <- result_stats_select_reactive()
    numeric_cols <- setdiff(colnames(df), "Locus")
    if (length(numeric_cols) == 0) return(NULL)
    df_display <- df
    df_display[, numeric_cols] <- round(df_display[, numeric_cols], 5)
    df_display
  }, rownames = FALSE)

  # ===== Overall Statistics by Population

  output$overall_by_pop <- renderTable({
    req(formatted_data)
    
    pops <- unique(formatted_data$Population)
    haplo <- formatted_data$haplotype
    pop_vec <- formatted_data$Population
    
    results_list <- lapply(pops, function(pop_name) {
      # Indices des individus dans cette population
      idx <- which(pop_vec == pop_name)
      if (length(idx) == 0) return(NULL)
      
      # Extraire les génotypes de cette pop
      pop_haplo <- haplo[idx, , drop = FALSE]
      n_ind <- nrow(pop_haplo)
      n_loci <- ncol(pop_haplo)
      
      Ho_vals <- numeric(n_loci)
      Hs_vals <- numeric(n_loci)
      
      for (j in 1:n_loci) {
        genotypes <- pop_haplo[, j]
        # Retirer les manquants
        genotypes <- genotypes[!is.na(genotypes) & genotypes != formatted_data$missing_code]
        if (length(genotypes) == 0) {
          Ho_vals[j] <- NA
          Hs_vals[j] <- NA
          next
        }
        
        # Compter les homozygotes observés → Ho
        hom_count <- sum(grepl("^([0-9]+)/\\1$", genotypes))  # ex: "1/1", "2/2"
        Ho_vals[j] <- hom_count / length(genotypes)
        
        # Calculer les fréquences alléliques
        alleles <- unlist(strsplit(genotypes, "/"))
        allele_freq <- table(alleles) / length(alleles)
        # Hs = 1 - somme(pi^2)
        Hs_vals[j] <- 1 - sum(allele_freq^2)
      }
      
      Ho_mean <- mean(Ho_vals, na.rm = TRUE)
      Hs_mean <- mean(Hs_vals, na.rm = TRUE)
      Fis_Nei_mean <- if (Hs_mean > 0) 1 - Ho_mean / Hs_mean else NA_real_
      
      data.frame(
        Population = pop_name,
        Ho = round(Ho_mean, 5),
        Hs = round(Hs_mean, 5),
        `Fis (Nei)` = round(Fis_Nei_mean, 5),
        stringsAsFactors = FALSE
      )
    })
    
    if (length(results_list) == 0 || all(sapply(results_list, is.null))) {
      return(data.frame(Message = "No population data"))
    }
    
    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL
    results_df
  }, rownames = FALSE)
  
  # Téléchargement des statistiques de base
  output$download_basic_stats <- downloadHandler(
    filename = function() paste0("basic_stats_result_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_stats_numeric_reactive())
      df <- result_stats_numeric_reactive()
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- lapply(df[num_cols], round, 5)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Téléchargement des statistiques de base au format TXT
  output$download_basic_stats_txt <- downloadHandler(
    filename = function() paste0("basic_stats_result_", Sys.Date(), ".txt"),
    content = function(file) {
      req(result_stats_numeric_reactive())
      df <- result_stats_numeric_reactive()
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- lapply(df[num_cols], round, 5)
      write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # Download FIS Table as TXT
  output$download_fis_table_txt <- downloadHandler(
    filename = function() { paste("fis_results_", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      req(fis_boot_results())
      write.table(fis_boot_results()$final_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # Téléchargement des résultats de permutation au format TXT
  output$download_perm_table_txt <- downloadHandler(
    filename = function() paste("permutation_results_", Sys.Date(), ".txt"),
    content = function(file) {
      req(fis_boot_results(), perm_results())
      tryCatch({
        final_table <- fis_boot_results()$final_table %>%
          dplyr::filter(Locus != "Overall")
        if (is.matrix(perm_results())) {
          perm_stats <- data.frame(
            Locus = colnames(perm_results()),
            Perm_Mean = colMeans(perm_results(), na.rm = TRUE),
            Perm_SD = apply(perm_results(), 2, sd, na.rm = TRUE)
          )
          result_table <- final_table %>%
            dplyr::select(Locus, Observed_FIS, P_value) %>%
            dplyr::left_join(perm_stats, by = "Locus") %>%
            dplyr::mutate(
              Significant = ifelse(P_value < 0.05, "Yes", "No"),
              Deviation = Observed_FIS - Perm_Mean
            )
          write.table(result_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
        } else {
          write.table(final_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      }, error = function(e) {
        write.table(data.frame(Error = e$message), file, sep = "\t", row.names = FALSE, quote = FALSE)
      })
    }
  )

  # Téléchargement des résultats bootstrap au format TXT
  output$download_boot_table_txt <- downloadHandler(
    filename = function() paste("bootstrap_results_", Sys.Date(), ".txt"),
    content = function(file) {
      req(fis_boot_results())
      tryCatch({
        final_table <- fis_boot_results()$final_table %>%
          dplyr::filter(Locus != "Overall") %>%
          dplyr::mutate(
            CI_Width = CI_U - CI_L,
            Significant = ifelse(P_value < 0.05, "Yes", "No")
          )
        write.table(final_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }, error = function(e) {
        write.table(data.frame(Error = e$message), file, sep = "\t", row.names = FALSE, quote = FALSE)
      })
    }
  )

  output$download_perm_plot <- downloadHandler(
    filename = function() paste0("permutation_plot_", Sys.Date(), ".png"),
    content = function(file) {
      req(fis_boot_results(), perm_results())
      p <- generate_permutation_plot(fis_boot_results(), perm_results())
      ggsave(file, plot = p, device = "png", width = 10, height = 6, dpi = 300)
    }
  )

  output$download_boot_plot <- downloadHandler(
    filename = function() paste0("bootstrap_plot_", Sys.Date(), ".png"),
    content = function(file) {
      req(fis_boot_results())
      p <- generate_bootstrap_plot(fis_boot_results())
      ggsave(file, plot = p, device = "png", width = 10, height = 6, dpi = 300)
    }
  )

  # Fonction pour générer le plot de permutation (retourne un ggplot)
  generate_permutation_plot <- function(fis_boot_results_val, perm_results_val) {
    if (is.null(perm_results_val) || !is.matrix(perm_results_val)) {
      return(ggplot() + labs(title = "No permutation data available") + theme_minimal())
    }
    
    df_perm <- tryCatch({
      as.data.frame(perm_results_val)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(df_perm) || nrow(df_perm) == 0) {
      return(ggplot() + labs(title = "No permutation data to display") + theme_minimal())
    }
    
    df_perm$permutation <- 1:nrow(df_perm)
    df_long <- df_perm %>% 
      tidyr::pivot_longer(cols = -permutation, names_to = "Locus", values_to = "FIS")
    
    obs_fis <- fis_boot_results_val$final_table %>% 
      dplyr::filter(Locus != "Overall") %>%
      dplyr::select(Locus, Observed_FIS)
    
    ggplot(df_long, aes(x = FIS)) +
      geom_histogram(alpha = 0.6, bins = 30, fill = "lightblue") +
      geom_vline(
        data = obs_fis,
        aes(xintercept = Observed_FIS),
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      facet_wrap(~ Locus, scales = "free") +
      labs(
        title = "Permutation Test Results",
        x = "FIS",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  }

  # Fonction pour générer le plot bootstrap
  generate_bootstrap_plot <- function(fis_boot_results_val) {
    tryCatch({
      boot_data <- NULL
      if (!is.null(fis_boot_results_val$bootstrap_results$individual)) {
        boot_matrix <- fis_boot_results_val$bootstrap_results$individual
        if (is.matrix(boot_matrix) && ncol(boot_matrix) > 0 && nrow(boot_matrix) > 0) {
          final_table <- fis_boot_results_val$final_table %>%
            dplyr::filter(Locus != "Overall")
          locus_names <- final_table$Locus
          if (ncol(boot_matrix) != length(locus_names)) {
            boot_matrix <- boot_matrix[, 1:length(locus_names), drop = FALSE]
          }
          colnames(boot_matrix) <- locus_names
          boot_data <- as.data.frame(boot_matrix)
        }
      }
      
      if (is.null(boot_data) || nrow(boot_data) == 0) {
        return(ggplot() + labs(title = "No bootstrap data available") + theme_minimal())
      }
      
      boot_data$replicate <- 1:nrow(boot_data)
      df_long <- boot_data %>% 
        tidyr::pivot_longer(cols = -replicate, names_to = "Locus", values_to = "FIS")
      
      obs_fis <- fis_boot_results_val$final_table %>%
        dplyr::filter(Locus != "Overall") %>%
        dplyr::select(Locus, Observed_FIS) %>%
        dplyr::rename(FIS_Obs = Observed_FIS)
      
      ci_data <- fis_boot_results_val$final_table %>%
        dplyr::select(Locus, CI_L, CI_U)
      
      ggplot(df_long, aes(x = FIS, fill = Locus)) +
        geom_density(alpha = 0.5) +
        geom_vline(data = obs_fis, aes(xintercept = FIS_Obs), 
                  color = "red", linetype = "dashed", size = 1) +
        geom_vline(data = ci_data, aes(xintercept = CI_L), 
                  color = "blue", linetype = "dotted", size = 0.7) +
        geom_vline(data = ci_data, aes(xintercept = CI_U), 
                  color = "blue", linetype = "dotted", size = 0.7) +
        facet_wrap(~ Locus, scales = "free") +
        labs(title = "Bootstrap FIS Distribution",
            x = "FIS", y = "Density") +
        theme_minimal() +
        theme(legend.position = "none")
      
    }, error = function(e) {
      ggplot() + 
        labs(title = paste("Error in bootstrap plot:", e$message)) +
        theme_minimal()
    })
  }

  # Télécharger le plot de permutation pour un locus
  output$download_locus_perm_plot <- downloadHandler(
    filename = function() {
      req(input$selected_locus)
      paste0("locus_perm_", input$selected_locus, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(fis_boot_results(), input$selected_locus, perm_results())
      locus <- input$selected_locus
      if (!locus %in% colnames(perm_results())) {
        ggsave(file, plot = ggplot() + labs(title = "Locus not found in permutation data") + theme_minimal())
        return()
      }
      perm_data <- perm_results()[, locus]
      obs_fis <- fis_boot_results()$final_table %>% 
        dplyr::filter(Locus == locus) %>% 
        dplyr::pull(Observed_FIS)
      
      p <- ggplot(data.frame(FIS = perm_data), aes(x = FIS)) +
        geom_histogram(fill = "lightblue", bins = 30) +
        geom_vline(xintercept = obs_fis, color = "red", linetype = "dashed", size = 1.5) +
        labs(
          title = paste("Permutation Distribution -", locus),
          x = "FIS", y = "Frequency"
        ) +
        theme_minimal()
      
      ggsave(file, plot = p, device = "png", width = 8, height = 5, dpi = 300)
    }
  )

  # Télécharger le plot bootstrap pour un locus
  output$download_locus_boot_plot <- downloadHandler(
    filename = function() {
      req(input$selected_locus)
      paste0("locus_boot_", input$selected_locus, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(fis_boot_results(), input$selected_locus)
      locus <- input$selected_locus
      final_table_non_overall <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall")
      if (!locus %in% final_table_non_overall$Locus) {
        ggsave(file, plot = ggplot() + labs(title = "Locus not available") + theme_minimal())
        return()
      }
      locus_index <- which(final_table_non_overall$Locus == locus)
      boot_matrix <- fis_boot_results()$bootstrap_results$individual
      if (is.null(boot_matrix) || ncol(boot_matrix) < locus_index) {
        ggsave(file, plot = ggplot() + labs(title = "No bootstrap data for this locus") + theme_minimal())
        return()
      }
      boot_data <- boot_matrix[, locus_index]
      locus_row <- final_table_non_overall %>% dplyr::filter(Locus == locus)
      obs_fis <- locus_row$Observed_FIS
      ci_lower <- locus_row$CI_L
      ci_upper <- locus_row$CI_U

      p <- ggplot(data.frame(FIS = boot_data), aes(x = FIS)) +
        geom_density(fill = "lightgreen", alpha = 0.5) +
        geom_vline(xintercept = obs_fis, color = "red", linetype = "dashed", size = 1.5) +
        geom_vline(xintercept = c(ci_lower, ci_upper), color = "blue", linetype = "dotted", size = 1) +
        labs(
          title = paste("Bootstrap Distribution -", locus),
          x = "FIS", y = "Density"
        ) +
        theme_minimal()

      ggsave(file, plot = p, device = "png", width = 8, height = 5, dpi = 300)
    }
  )

  # ===== UNILATERAL P-VALUES TABLE =====

  output$unilateral_pvalues_table <- DT::renderDT({
    req(fis_boot_results())
    tryCatch({
      df <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall") %>%
        dplyr::select(Locus, Observed_FIS, P_value)
      
      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No locus-level results available")))
      }
      
      # Calcul des p-valeurs unilatérales
      df <- df %>%
        dplyr::mutate(
          # P(FIS > 0) : probabilité d'observer une valeur aussi grande ou plus grande que l'observée
          # Si Observed_FIS >= 0, p_uni_right = P_value / 2
          # Si Observed_FIS < 0, p_uni_right = 1 - (P_value / 2)
          `P (FIS > 0)` = ifelse(
            Observed_FIS >= 0,
            pmax(P_value / 2, .Machine$double.eps),
            pmin(1 - P_value / 2, 1 - .Machine$double.eps)
          ),
          # P(FIS < 0) : probabilité d'observer une valeur aussi petite ou plus petite
          # Si Observed_FIS <= 0, p_uni_left = P_value / 2
          # Si Observed_FIS > 0, p_uni_left = 1 - (P_value / 2)
          `P (FIS < 0)` = ifelse(
            Observed_FIS <= 0,
            pmax(P_value / 2, .Machine$double.eps),
            pmin(1 - P_value / 2, 1 - .Machine$double.eps)
          )
        ) %>%
        dplyr::select(Locus, Observed_FIS, `P (FIS > 0)`, `P (FIS < 0)`, P_value)
      
      # Formatage
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatRound(
          columns = c("Observed_FIS", "P (FIS > 0)", "P (FIS < 0)", "P_value"),
          digits = 5
        ) %>%
        DT::formatStyle(
          "P (FIS > 0)",
          backgroundColor = DT::styleInterval(c(0.001, 0.01, 0.05), c("#ffebee", "#ffcdd2", "#ef9a9a", "#ffffff"))
        ) %>%
        DT::formatStyle(
          "P (FIS < 0)",
          backgroundColor = DT::styleInterval(c(0.001, 0.01, 0.05), c("#e8f5e8", "#c8e6c9", "#a5d6a7", "#ffffff"))
        )
      
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  })

  # Téléchargement CSV
  output$download_unilateral_pvals <- downloadHandler(
    filename = function() paste0("unilateral_pvalues_", Sys.Date(), ".csv"),
    content = function(file) {
      req(fis_boot_results())
      df <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall") %>%
        dplyr::select(Locus, Observed_FIS, P_value) %>%
        dplyr::mutate(
          `P (FIS > 0)` = ifelse(Observed_FIS >= 0, P_value / 2, 1 - P_value / 2),
          `P (FIS < 0)` = ifelse(Observed_FIS <= 0, P_value / 2, 1 - P_value / 2)
        ) %>%
        dplyr::select(Locus, Observed_FIS, `P (FIS > 0)`, `P (FIS < 0)`, P_value)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Téléchargement TXT (tabulé)
  output$download_unilateral_pvals_txt <- downloadHandler(
    filename = function() paste0("unilateral_pvalues_", Sys.Date(), ".txt"),
    content = function(file) {
      req(fis_boot_results())
      df <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall") %>%
        dplyr::select(Locus, Observed_FIS, P_value) %>%
        dplyr::mutate(
          `P (FIS > 0)` = ifelse(Observed_FIS >= 0, P_value / 2, 1 - P_value / 2),
          `P (FIS < 0)` = ifelse(Observed_FIS <= 0, P_value / 2, 1 - P_value / 2)
        ) %>%
        dplyr::select(Locus, Observed_FIS, `P (FIS > 0)`, `P (FIS < 0)`, P_value)
      write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # ===== TABLEAUX PERMUTATION & BOOTSTRAP
  
  # Tableau des résultats de permutation par locus
  output$permutation_table <- DT::renderDT({
    req(fis_boot_results(), perm_results())
    
    tryCatch({
      final_table <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall")
      
      if (is.matrix(perm_results())) {
        perm_stats <- data.frame(
          Locus = colnames(perm_results()),
          Perm_Mean = colMeans(perm_results(), na.rm = TRUE),
          Perm_SD = apply(perm_results(), 2, sd, na.rm = TRUE)
        )
        
        result_table <- final_table %>%
          dplyr::select(Locus, Observed_FIS, P_value) %>%
          dplyr::left_join(perm_stats, by = "Locus") %>%
          dplyr::mutate(
            Significant = ifelse(P_value < 0.05, "***", 
                                ifelse(P_value < 0.01, "**", 
                                      ifelse(P_value < 0.001, "*", ""))),
            Deviation = Observed_FIS - Perm_Mean
          ) %>%
          dplyr::select(Locus, Observed_FIS, Perm_Mean, Deviation, Perm_SD, P_value, Significant)
        
        DT::datatable(
          result_table,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tlip'
          ),
          rownames = FALSE,
          colnames = c('Locus', 'FIS Observed', 'FIS Perm Mean', 'Deviation', 'Perm SD', 'P-value', 'Significance')
        ) %>%
          DT::formatRound(columns = c('Observed_FIS', 'Perm_Mean', 'Deviation', 'Perm_SD', 'P_value'), digits = 4) %>%
          DT::formatStyle(
            'P_value',
            backgroundColor = DT::styleInterval(
              c(0.001, 0.01, 0.05),
              c('#e6f3ff', '#b3d9ff', '#99ccff', '#ffffff')
            )
          )
      } else {
        DT::datatable(data.frame(Message = "No permutation data available"))
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)))
    })
  })

  # Tableau des résultats bootstrap par locus
  output$bootstrap_table <- DT::renderDT({
    req(fis_boot_results())
    tryCatch({
      final_table <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall")
      
      if (nrow(final_table) == 0) {
        return(DT::datatable(data.frame(Message = "No non-overall loci found")))
      }
      
      if (!is.null(fis_boot_results()$bootstrap_results$individual)) {
        boot_matrix <- fis_boot_results()$bootstrap_results$individual
        
        if (is.matrix(boot_matrix) && ncol(boot_matrix) > 0 && nrow(boot_matrix) > 0) {
          # Alignement par POSITION : on suppose que colonne j = locus j
          n_loci_final <- nrow(final_table)
          n_loci_boot <- ncol(boot_matrix)
          
          if (n_loci_boot != n_loci_final) {
            # Si les dimensions ne correspondent pas, on ne peut pas aligner → fallback
            result_table <- final_table %>%
              dplyr::mutate(
                Significant = ifelse(P_value < 0.05, "***", 
                                    ifelse(P_value < 0.01, "**", 
                                          ifelse(P_value < 0.001, "*", ""))),
                CI_Width = CI_U - CI_L
              ) %>%
              dplyr::select(Locus, Observed_FIS, CI_L, CI_U, CI_Width, P_value, Significant)
            
            return(
              DT::datatable(result_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
                DT::formatRound(columns = c('Observed_FIS', 'CI_L', 'CI_U', 'CI_Width', 'P_value'), digits = 4)
            )
          }
          
          # Calcul par position
          boot_means <- apply(boot_matrix, 2, mean, na.rm = TRUE)
          boot_sds   <- apply(boot_matrix, 2, sd,   na.rm = TRUE)
          
          # Ajouter directement aux lignes de final_table (même ordre !)
          result_table <- final_table %>%
            dplyr::mutate(
              Boot_Mean = boot_means,
              Boot_SD   = boot_sds,
              Significant = ifelse(P_value < 0.05, "***", 
                                  ifelse(P_value < 0.01, "**", 
                                        ifelse(P_value < 0.001, "*", ""))),
              CI_Width = CI_U - CI_L
            ) %>%
            dplyr::select(Locus, Observed_FIS, Boot_Mean, CI_L, CI_U, CI_Width, Boot_SD, P_value, Significant)
          
          # Affichage avec style de Script 2
          DT::datatable(
            result_table,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'tlip'
            ),
            rownames = FALSE,
            colnames = c('Locus', 'FIS Obs', 'Boot Mean', 'CI Lower', 'CI Upper', 'CI Width', 'Boot SD', 'P-value', 'Significance')
          ) %>%
            DT::formatRound(columns = c('Observed_FIS', 'Boot_Mean', 'CI_L', 'CI_U', 'CI_Width', 'Boot_SD', 'P_value'), digits = 4) %>%
            DT::formatStyle(
              'P_value',
              backgroundColor = DT::styleInterval(
                c(0.001, 0.01, 0.05),
                c('#e6f3ff', '#b3d9ff', '#99ccff', '#ffffff')
              )
            )
        } else {
          # Matrice vide
          result_table <- final_table %>%
            dplyr::mutate(
              Significant = ifelse(P_value < 0.05, "***", 
                                  ifelse(P_value < 0.01, "**", 
                                        ifelse(P_value < 0.001, "*", ""))),
              CI_Width = CI_U - CI_L
            ) %>%
            dplyr::select(Locus, Observed_FIS, CI_L, CI_U, CI_Width, P_value, Significant)
          
          DT::datatable(
            result_table,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'tlip'
            ),
            rownames = FALSE
          ) %>%
            DT::formatRound(columns = c('Observed_FIS', 'CI_L', 'CI_U', 'CI_Width', 'P_value'), digits = 4)
        }
      } else {
        # Pas de données bootstrap
        result_table <- final_table %>%
          dplyr::mutate(
            Significant = ifelse(P_value < 0.05, "***", 
                                ifelse(P_value < 0.01, "**", 
                                      ifelse(P_value < 0.001, "*", ""))),
            CI_Width = CI_U - CI_L
          ) %>%
          dplyr::select(Locus, Observed_FIS, CI_L, CI_U, CI_Width, P_value, Significant)
        
        DT::datatable(
          result_table,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'tlip'
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(columns = c('Observed_FIS', 'CI_L', 'CI_U', 'CI_Width', 'P_value'), digits = 4)
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Bootstrap table error:", e$message)))
    })
  })

  # Summary outputs
  output$permutation_summary <- renderPrint({
    req(fis_boot_results(), perm_results())
    
    tryCatch({
      final_table <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall")
      
      cat("Permutation Test Summary\n")
      cat("========================\n")
      cat("Total loci analyzed:", nrow(final_table), "\n")
      cat("Significant loci (p < 0.05):", sum(final_table$P_value < 0.05, na.rm = TRUE), "\n")
      cat("Highly significant (p < 0.01):", sum(final_table$P_value < 0.01, na.rm = TRUE), "\n")
      cat("Very highly significant (p < 0.001):", sum(final_table$P_value < 0.001, na.rm = TRUE), "\n")
      cat("Mean FIS observed:", round(mean(final_table$Observed_FIS, na.rm = TRUE), 4), "\n")
      
      if (is.matrix(perm_results())) {
        cat("Mean FIS permuted:", round(mean(colMeans(perm_results(), na.rm = TRUE), na.rm = TRUE), 4), "\n")
      }
    }, error = function(e) {
      cat("Error generating summary:", e$message)
    })
  })
  
  output$bootstrap_summary <- renderPrint({
    req(fis_boot_results())
    
    tryCatch({
      final_table <- fis_boot_results()$final_table %>%
        dplyr::filter(Locus != "Overall")
      
      cat("Bootstrap Analysis Summary\n")
      cat("==========================\n")
      cat("Total loci analyzed:", nrow(final_table), "\n")
      cat("Significant loci (p < 0.05):", sum(final_table$P_value < 0.05, na.rm = TRUE), "\n")
      cat("Mean FIS observed:", round(mean(final_table$Observed_FIS, na.rm = TRUE), 4), "\n")
      
      if (all(c("CI_L", "CI_U") %in% names(final_table))) {
        ci_widths <- final_table$CI_U - final_table$CI_L
        cat("Mean 95% CI width:", round(mean(ci_widths, na.rm = TRUE), 4), "\n")
        cat("Min CI width:", round(min(ci_widths, na.rm = TRUE), 4), "\n")
        cat("Max CI width:", round(max(ci_widths, na.rm = TRUE), 4), "\n")
      }
    }, error = function(e) {
      cat("Error generating summary:", e$message)
    })
  })
  
  # Téléchargement des tableaux
  output$download_perm_table <- downloadHandler(
    filename = function() paste("permutation_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(fis_boot_results(), perm_results())
      
      tryCatch({
        final_table <- fis_boot_results()$final_table %>%
          dplyr::filter(Locus != "Overall")
        
        if (is.matrix(perm_results())) {
          perm_stats <- data.frame(
            Locus = colnames(perm_results()),
            Perm_Mean = colMeans(perm_results(), na.rm = TRUE),
            Perm_SD = apply(perm_results(), 2, sd, na.rm = TRUE)
          )
          
          result_table <- final_table %>%
            dplyr::select(Locus, Observed_FIS, P_value) %>%
            dplyr::left_join(perm_stats, by = "Locus") %>%
            dplyr::mutate(
              Significant = ifelse(P_value < 0.05, "Yes", "No"),
              Deviation = Observed_FIS - Perm_Mean
            )
          
          write.csv(result_table, file, row.names = FALSE)
        } else {
          write.csv(final_table, file, row.names = FALSE)
        }
      }, error = function(e) {
        # Fallback
        write.csv(data.frame(Error = e$message), file, row.names = FALSE)
      })
    }
  )
  
  output$download_boot_table <- downloadHandler(
    filename = function() paste("bootstrap_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(fis_boot_results())
      
      tryCatch({
        final_table <- fis_boot_results()$final_table %>%
          dplyr::filter(Locus != "Overall") %>%
          dplyr::mutate(
            CI_Width = CI_U - CI_L,
            Significant = ifelse(P_value < 0.05, "Yes", "No")
          )
        
        write.csv(final_table, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = e$message), file, row.names = FALSE)
      })
    }
  )

  # ===== BOOTSTRAP FIS ANALYSIS =====

  observeEvent(input$run_bootstrap_fis, {
    if (is.null(formatted_data)) {
      showNotification("No genetic data available", type = "error")
      return(NULL)
    }
    
    if (input$n_perm < 10 || input$n_boot < 10) {
      showNotification("Number of permutations and bootstrap replicates should be at least 10", type = "warning")
    }
    
    waiter <- Waiter$new(
      id = c("fis_results_table", "permutation_plot", "bootstrap_plot"),
      html = spin_3(), 
      color = transparent(0.7)
    )
    waiter$show()
    
    on.exit(waiter$hide())
    
    tryCatch({
      start_time <- Sys.time()
      
      results <- run_bootstrap_fis_analysis(
        formatted_data = formatted_data,
        n_perm = input$n_perm,
        n_boot = input$n_boot,
        conf_level = input$conf_level
      )
      
      if (is.null(results) || is.null(results$final_table)) {
        stop("Invalid results from bootstrap analysis")
      }
      
      end_time <- Sys.time()
      duration <- round(difftime(end_time, start_time, units = "secs"), 1)
      fis_boot_timing(duration)
      fis_boot_results(results)
      perm_results(results$permutation_results)
      
      showNotification("Bootstrap FIS analysis completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in bootstrap analysis:", e$message), type = "error")
      fis_boot_results(NULL)
      perm_results(NULL)
    })
  })

  # Global FIS Value Box
  output$global_fis_box <- renderValueBox({
    req(fis_boot_results())
    fis_val <- fis_boot_results()$final_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(Observed_FIS)
    fis_display <- ifelse(abs(fis_val) < 0.0001, "≈ 0.0000", format(round(fis_val, 4), nsmall = 4))
    color <- ifelse(fis_val > 0.1, "red", ifelse(fis_val > 0.05, "yellow", "green"))
    subtitle_html <- ifelse(fis_val > 0.1, "Strong inbreeding", ifelse(fis_val > 0.05, "Moderate inbreeding", ifelse(fis_val < -0.05, "Heterozygote excess", "Near equilibrium")))
    valueBox(
      value = fis_display,
      subtitle = HTML(paste0("<small>Global F<sub>IS</sub><br>", subtitle_html, "</small>")),
      color = color,
      icon = icon("dna"),
      width = NULL
    )
  })

  # Global P-value
  output$global_pvalue_box <- renderValueBox({
    req(fis_boot_results())
    p_val <- fis_boot_results()$final_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(P_value)
    p_display <- ifelse(p_val < 0.0001, "< 0.0001", ifelse(p_val < 0.001, "< 0.001", format(round(p_val, 4), nsmall = 4)))
    color <- ifelse(p_val < 0.001, "red", ifelse(p_val < 0.05, "yellow", "green"))
    valueBox(
      value = p_display,
      subtitle = HTML("<small>Global <i>p</i>-value<br>Bilateral test</small>"),
      color = color,
      icon = icon("balance-scale"),
      width = NULL
    )
  })

  # Significant Loci
  output$significant_loci_box <- renderValueBox({
    req(fis_boot_results())
    sig <- fis_boot_results()$final_table %>%
      dplyr::filter(Locus != "Overall", P_value < 0.05) %>%
      nrow()
    total <- fis_boot_results()$final_table %>%
      dplyr::filter(Locus != "Overall") %>%
      nrow()
    pct <- ifelse(total > 0, round(100 * sig / total, 1), 0)
    color <- ifelse(sig > 0, "yellow", "aqua")
    valueBox(
      value = paste0(sig, " / ", total),
      subtitle = HTML(paste0("<small>Significant Loci<br><small>", pct, "% of total</small></small>")),
      color = color,
      icon = icon("vial"),
      width = NULL
    )
  })

  # Analysis Time
  output$analysis_time_box <- renderValueBox({
    req(fis_boot_timing())
    time_sec <- fis_boot_timing()
    if (time_sec < 60) {
      time_display <- paste0(time_sec, " s")
    } else {
      time_display <- paste0(round(time_sec / 60, 1), " min")
    }
    valueBox(
      value = time_display,
      subtitle = HTML("<small>Computation Time<br>Permutation + Bootstrap</small>"),
      color = "purple",
      icon = icon("hourglass-half"),
      width = NULL
    )
  })

  # FIS Results Table
  output$fis_results_table <- DT::renderDataTable({
    req(fis_boot_results())
    
    df <- fis_boot_results()$final_table
    
    if (input$results_view == "Significant Only") {
      df <- df %>% filter(P_value < 0.05 | Locus == "Overall")
    }
    
    dt <- DT::datatable(
      df,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c('Observed_FIS', 'P_value', 'CI_L', 'CI_U'), digits = 4)
    
    return(dt)
  })
  
  # Permutation Plot
  output$permutation_plot <- renderPlot({
  req(fis_boot_results(), perm_results())
  
    if (is.null(perm_results()) || !is.matrix(perm_results())) {
      return(ggplot() + 
              labs(title = "No permutation data available") +
              theme_minimal())
    }
    
    df_perm <- tryCatch({
      as.data.frame(perm_results())
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(df_perm) || nrow(df_perm) == 0) {
      return(ggplot() + 
              labs(title = "No permutation data to display") +
              theme_minimal())
    }
    
    # Ajouter un identifiant de réplication
    df_perm$permutation <- 1:nrow(df_perm)
    
    # Pivoter en format long
    df_long <- df_perm %>% 
      tidyr::pivot_longer(cols = -permutation, names_to = "Locus", values_to = "FIS")
    
    # Récupérer les valeurs observées (une seule valeur par locus)
    obs_fis <- fis_boot_results()$final_table %>% 
      dplyr::filter(Locus != "Overall") %>%
      dplyr::select(Locus, Observed_FIS)
    
    # Créer le plot avec le style de Script 2
    ggplot(df_long, aes(x = FIS)) +
      geom_histogram(alpha = 0.6, bins = 30, fill = "lightblue") +
      geom_vline(
        data = obs_fis,
        aes(xintercept = Observed_FIS),
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      facet_wrap(~ Locus, scales = "free") +
      labs(
        title = "Permutation Test Results",
        x = "FIS",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bootstrap Plot
  output$bootstrap_plot <- renderPlot({
    req(fis_boot_results())
    tryCatch({
      boot_data <- NULL
      if (!is.null(fis_boot_results()$bootstrap_results$individual)) {
        boot_matrix <- fis_boot_results()$bootstrap_results$individual
        
        # Vérifier que c'est bien une matrice avec des données
        if (is.matrix(boot_matrix) && ncol(boot_matrix) > 0 && nrow(boot_matrix) > 0) {
          # Récupérer les noms de loci depuis final_table (source fiable)
          final_table <- fis_boot_results()$final_table %>%
            dplyr::filter(Locus != "Overall")
          locus_names <- final_table$Locus
          
          # Vérifier que les dimensions correspondent
          if (ncol(boot_matrix) != length(locus_names)) {
            showNotification("Warning: Number of bootstrap columns doesn't match number of loci.", type = "warning")
            # Tronquer ou étendre si nécessaire
            boot_matrix <- boot_matrix[, 1:length(locus_names), drop = FALSE]
          }
          
          # Forcer les bons noms de loci
          colnames(boot_matrix) <- locus_names
          
          boot_data <- as.data.frame(boot_matrix)
        }
      }
      
      if (is.null(boot_data) || nrow(boot_data) == 0) {
        return(ggplot() + 
                 labs(title = "No bootstrap data available") +
                 theme_minimal())
      }
      
      # Préparer les données pour le plot
      boot_data$replicate <- 1:nrow(boot_data)
      df_long <- boot_data %>% 
        tidyr::pivot_longer(cols = -replicate, names_to = "Locus", values_to = "FIS")
      
      # Récupérer les Observed_FIS et CI pour chaque locus
      obs_fis <- final_table %>%
        dplyr::select(Locus, Observed_FIS) %>%
        dplyr::rename(FIS_Obs = Observed_FIS)
      
      ci_data <- final_table %>%
        dplyr::select(Locus, CI_L, CI_U)
      
      # Créer le plot
      ggplot(df_long, aes(x = FIS, fill = Locus)) +
        geom_density(alpha = 0.5) +
        geom_vline(data = obs_fis, aes(xintercept = FIS_Obs), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(data = ci_data, aes(xintercept = CI_L), 
                   color = "blue", linetype = "dotted", size = 0.7) +
        geom_vline(data = ci_data, aes(xintercept = CI_U), 
                   color = "blue", linetype = "dotted", size = 0.7) +
        facet_wrap(~ Locus, scales = "free") +
        labs(title = "Bootstrap FIS Distribution",
             x = "FIS", y = "Density") +
        theme_minimal() +
        theme(legend.position = "none")
    }, error = function(e) {
      ggplot() + 
        labs(title = paste("Error in bootstrap plot:", e$message)) +
        theme_minimal()
    })
  })
  
  # Download FIS Table
  output$download_fis_table <- downloadHandler(
    filename = function() { paste("fis_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      req(fis_boot_results())
      write.csv(fis_boot_results()$final_table, file, row.names = FALSE)
    }
  )
  
  # Locus Bootstrap Plot
  output$locus_boot_plot <- renderPlot({
    req(fis_boot_results(), input$selected_locus)
    locus <- input$selected_locus
    
    # Récupérer les loci non-"Overall"
    final_table_non_overall <- fis_boot_results()$final_table %>%
      dplyr::filter(Locus != "Overall")
    
    if (!locus %in% final_table_non_overall$Locus) {
      return(ggplot() + 
              labs(title = paste("No data for locus:", locus)) +
              theme_minimal())
    }
    
    # Trouver l'index du locus (par position, pas par nom)
    locus_index <- which(final_table_non_overall$Locus == locus)
    
    boot_data <- NULL
    if (!is.null(fis_boot_results()$bootstrap_results$individual)) {
      boot_matrix <- fis_boot_results()$bootstrap_results$individual
      if (is.matrix(boot_matrix) && ncol(boot_matrix) >= locus_index) {
        boot_data <- boot_matrix[, locus_index]
      }
    }
    
    if (is.null(boot_data) || length(boot_data) == 0) {
      return(ggplot() + 
              labs(title = paste("No bootstrap data for", locus)) +
              theme_minimal())
    }
    
    # Récupérer les valeurs depuis final_table
    locus_row <- final_table_non_overall %>% dplyr::filter(Locus == locus)
    obs_fis <- locus_row$Observed_FIS
    ci_lower <- locus_row$CI_L
    ci_upper <- locus_row$CI_U
    
    ggplot(data.frame(FIS = boot_data), aes(x = FIS)) +
      geom_density(fill = "lightgreen", alpha = 0.5) +
      geom_vline(xintercept = obs_fis, color = "red", linetype = "dashed", size = 1.5) +
      geom_vline(xintercept = c(ci_lower, ci_upper), color = "blue", linetype = "dotted", size = 1) +
      labs(title = paste("Bootstrap Distribution -", locus),
          x = "FIS", y = "Density") +
      theme_minimal()
  })
  
  # Locus Permutation Plot
  output$locus_perm_plot <- renderPlot({
    req(fis_boot_results(), input$selected_locus, perm_results())
    
    locus <- input$selected_locus
    
    if (!locus %in% fis_boot_results()$final_table$Locus) {
      return(ggplot() + 
               labs(title = paste("No data for locus:", locus)) +
               theme_minimal())
    }
    
    perm_data <- tryCatch({
      if (!is.null(perm_results()) && locus %in% colnames(perm_results())) {
        perm_results()[, locus]
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    
    if (is.null(perm_data) || length(perm_data) == 0) {
      return(ggplot() + 
               labs(title = paste("No permutation data for", locus)) +
               theme_minimal())
    }
    
    obs_fis <- fis_boot_results()$final_table %>% 
      filter(Locus == locus) %>% 
      pull(Observed_FIS)
    
    ggplot(data.frame(FIS = perm_data), aes(x = FIS)) +
      geom_histogram(fill = "lightblue", bins = 30) +
      geom_vline(xintercept = obs_fis, color = "red", linetype = "dashed", size = 1.5) +
      labs(title = paste("Permutation Distribution -", locus),
           x = "FIS", y = "Frequency") +
      theme_minimal()
  })
  
  # Locus Statistical Report
  output$locus_statistical_report <- renderPrint({
    req(fis_boot_results(), input$selected_locus)
    
    tryCatch({
      locus <- input$selected_locus
      locus_data <- fis_boot_results()$final_table %>% dplyr::filter(Locus == locus)
      
      if (nrow(locus_data) > 0) {
        cat("Statistical Report for", locus, "\n")
        cat("==============================\n")
        cat("Observed FIS:", round(locus_data$Observed_FIS, 4), "\n")
        cat("P-value:", ifelse(locus_data$P_value < 0.001, "<0.001", round(locus_data$P_value, 4)), "\n")
        cat("95% CI: [", round(locus_data$CI_L, 4), ", ", round(locus_data$CI_U, 4), "]\n")
        cat("Significance:", ifelse(locus_data$P_value < 0.05, "SIGNIFICANT", "Not significant"), "\n")
      } else {
        cat("No data available for locus:", locus)
      }
    }, error = function(e) {
      cat("Error generating report:", e$message)
    })
  })
  
  # Update locus and population choices
  observe({
    if (!is.null(fis_boot_results()) && !is.null(fis_boot_results()$final_table)) {
      loci <- fis_boot_results()$final_table %>% 
        dplyr::filter(Locus != "Overall") %>% 
        dplyr::pull(Locus)
      
      if (length(loci) > 0) {
        updateSelectInput(session, "selected_locus", choices = loci, selected = loci[1])
      }
    }
  })
  
  observe({
    if (!is.null(formatted_data)) {
      pops <- unique(formatted_data$Population)
      updateSelectInput(session, "selected_population", choices = pops, selected = pops[1])
    }
  })

  # Quality Control Plot
  output$qc_plot <- renderPlot({
    req(fis_boot_results())
    tryCatch({
      if (!is.null(fis_boot_results()$bootstrap_results$individual)) {
        boot_matrix <- fis_boot_results()$bootstrap_results$individual
        # Vérifier que c'est bien une matrice avec des données
        if (is.matrix(boot_matrix) && ncol(boot_matrix) > 0 && nrow(boot_matrix) > 0) {
          # Récupérer les noms de loci depuis final_table (source fiable)
          final_table <- fis_boot_results()$final_table %>%
            dplyr::filter(Locus != "Overall")
          locus_names <- final_table$Locus
          # Vérifier que les dimensions correspondent
          if (ncol(boot_matrix) != length(locus_names)) {
            showNotification("Warning: Number of bootstrap columns doesn't match number of loci.", type = "warning")
            # Tronquer ou étendre si nécessaire
            boot_matrix <- boot_matrix[, 1:length(locus_names), drop = FALSE]
          }
          # Forcer les bons noms de loci
          colnames(boot_matrix) <- locus_names
          valid_bootstraps <- colSums(!is.na(boot_matrix))
          df <- data.frame(
            Locus = colnames(boot_matrix),
            Valid_Bootstraps = valid_bootstraps,
            Total = nrow(boot_matrix)
          )
          ggplot(df, aes(x = Locus, y = Valid_Bootstraps)) +
            geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
            geom_hline(yintercept = nrow(boot_matrix) * 0.9, linetype = "dashed", color = "red") +
            labs(title = "Bootstrap Quality by Locus",
                 x = "Locus", y = "Number of Valid Bootstraps") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          ggplot() + 
            labs(title = "No bootstrap data available") + 
            theme_minimal()
        }
      } else {
        ggplot() + 
          labs(title = "No bootstrap data available") + 
          theme_minimal()
      }
    }, error = function(e) {
      ggplot() + 
        labs(title = "Error generating QC plot") + 
        theme_minimal()
    })
  })
  
  # Data Completeness Plot
  output$completeness_plot <- renderPlot({
    req(missing_data_reac())
    
    tryCatch({
      missing_data <- missing_data_reac()
      completeness <- 100 - colMeans(missing_data, na.rm = TRUE)
      
      df <- data.frame(
        Locus = names(completeness),
        Completeness = completeness
      )
      
      ggplot(df, aes(x = Locus, y = Completeness)) +
        geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.8) +
        labs(title = "Data Completeness by Locus",
             x = "Locus", y = "Completeness (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    }, error = function(e) {
      ggplot() + 
        labs(title = "Error generating completeness plot") + 
        theme_minimal()
    })
  })
  
  # QC Summary
  output$qc_summary <- renderPrint({
    req(fis_boot_results())
    
    tryCatch({
      cat("Quality Control Summary\n")
      cat("=======================\n")
      
      if (!is.null(fis_boot_results()$bootstrap_results$individual)) {
        boot_matrix <- fis_boot_results()$bootstrap_results$individual
        valid_bootstraps <- colSums(!is.na(boot_matrix))
        
        cat("Total bootstrap replicates:", nrow(boot_matrix), "\n")
        cat("Average valid bootstraps per locus:", round(mean(valid_bootstraps), 1), "\n")
        cat("Minimum valid bootstraps:", min(valid_bootstraps), "\n")
        cat("Maximum valid bootstraps:", max(valid_bootstraps), "\n")
        
        low_quality <- sum(valid_bootstraps < nrow(boot_matrix) * 0.9)
        cat("Loci with <90% valid bootstraps:", low_quality, "\n")
      } else {
        cat("No bootstrap data available\n")
      }
    }, error = function(e) {
      cat("Error generating QC summary:", e$message)
    })
  })
  
  # Session Info
  output$session_info <- renderPrint({
    tryCatch({
      cat("R version:", R.version.string, "\n")
      cat("Platform:", R.version$platform, "\n")
      cat("Shiny version:", as.character(packageVersion("shiny")), "\n")
      cat("adegenet version:", as.character(packageVersion("adegenet")), "\n")
      cat("hierfstat version:", as.character(packageVersion("hierfstat")), "\n")
      cat("Analysis date:", as.character(Sys.Date()), "\n")
      cat("Analysis time:", format(Sys.time(), "%H:%M:%S"), "\n")
      
      if (!is.null(formatted_data)) {
        cat("Dataset info:\n")
        cat("- Individuals:", nrow(formatted_data$haplotype), "\n")
        cat("- Populations:", length(unique(formatted_data$Population)), "\n")
        cat("- Loci:", ncol(formatted_data$haplotype), "\n")
      }
    }, error = function(e) {
      cat("Error generating session info:", e$message)
    })
  })
  
  # Report generation placeholder
  observeEvent(input$generate_report, {
    showNotification("Report generation feature is under development. Use download buttons for individual results.", 
                    type = "message", duration = 5)
  })
  
  # Export Data
  output$export_all_data <- downloadHandler(
    filename = function() { 
      paste("genetic_analysis_export_", Sys.Date(), ".zip", sep = "") 
    },
    content = function(file) {
      tryCatch({
        # Create temporary directory
        temp_dir <- tempdir()
        
        # List of files to include in export
        files_to_zip <- c()
        
        # Export basic stats if available
        if (!is.null(result_stats_numeric_reactive())) {
          basic_stats_file <- file.path(temp_dir, "basic_statistics.csv")
          write.csv(result_stats_numeric_reactive(), basic_stats_file, row.names = FALSE)
          files_to_zip <- c(files_to_zip, basic_stats_file)
        }
        
        # Export FIS results if available
        if (!is.null(fis_boot_results())) {
          fis_results_file <- file.path(temp_dir, "fis_bootstrap_results.csv")
          write.csv(fis_boot_results()$final_table, fis_results_file, row.names = FALSE)
          files_to_zip <- c(files_to_zip, fis_results_file)
        }
        
        # Export session info
        session_file <- file.path(temp_dir, "session_info.txt")
        sink(session_file)
        cat("Genetic Analysis Session Information\n")
        cat("====================================\n")
        cat("Date:", as.character(Sys.Date()), "\n")
        cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n")
        cat("R version:", R.version.string, "\n")
        sink()
        files_to_zip <- c(files_to_zip, session_file)
        
        # Create the zip file
        if (length(files_to_zip) > 0) {
          zip(file, files_to_zip, flags = "-j")
          showNotification("Data exported successfully!", type = "message")
        } else {
          # Create empty file with message
          writeLines("No analysis results available for export. Please run analyses first.", file)
          showNotification("No data available for export", type = "warning")
        }
        
      }, error = function(e) {
        # Fallback: create a simple text file
        writeLines(paste("Export failed:", e$message), file)
        showNotification("Error during export", type = "error")
      })
    }
  )
  
  # Save Workspace
  observeEvent(input$save_workspace, {
    tryCatch({
      # Create a simple workspace summary
      workspace_data <- list(
        timestamp = Sys.time(),
        basic_stats_available = !is.null(result_stats_reactive()),
        bootstrap_results_available = !is.null(fis_boot_results()),
        dataset_info = if (!is.null(formatted_data)) {
          list(
            n_individuals = nrow(formatted_data$haplotype),
            n_populations = length(unique(formatted_data$Population)),
            n_loci = ncol(formatted_data$haplotype)
          )
        } else {
          NULL
        }
      )
      
      # Save to temporary file (in real app, this would save to user-specified location)
      temp_file <- tempfile(fileext = ".rds")
      saveRDS(workspace_data, temp_file)
      
      showNotification("Workspace information saved (feature under development)", 
                      type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification("Error saving workspace", type = "error")
    })
  })

  # Additional error handling for reactive expressions
  observe({
    # Monitor for errors in reactive values and provide user feedback
    if (!is.null(mydata_genind_reac()) && !is.null(mydata_hierfstat_reac())) {
      showNotification("Genetic data objects ready for analysis", type = "message", duration = 2)
    }
  })
  
  # Cleanup function when session ends
  session$onSessionEnded(function() {
    # Clean up any temporary files or connections
    tryCatch({
      # Any cleanup code would go here
      message("Session ended, cleanup completed")
    }, error = function(e) {
      message("Error during cleanup: ", e$message)
    })
  })

  ###

  # Fonction pour calculer FIT à partir de FST et FIS
  calculate_fit_from_fst_fis <- function(fst, fis) {
    # Formule: FIT = 1 - (1 - FIS) * (1 - FST)
    # Ou: FIT = FIS + FST - FIS*FST
    fit <- fst + fis - (fst * fis)
    
    # Gérer les valeurs NA
    fit[is.na(fst) | is.na(fis)] <- NA
    
    # S'assurer que FIT est dans l'intervalle [-1, 1]
    fit <- pmin(pmax(fit, -1), 1)
    
    return(fit)
  }

  # ===== FONCTION BOOTSTRAP FIT COMPLÈTE =====

  run_bootstrap_fit_analysis <- function(formatted_data, n_perm, n_boot, conf_level, bootstrap_method, analysis_level) {
    set.seed(123)
    RNGkind("Mersenne-Twister")
    
    if (is.null(formatted_data) || is.null(formatted_data$haplotype)) {
      stop("Invalid formatted_data structure")
    }
    
    tryCatch({
      # Afficher les informations de débogage
      cat("=== STARTING FIT BOOTSTRAP ANALYSIS ===\n")
      cat("Bootstrap method:", bootstrap_method, "\n")
      cat("Number of bootstrap replicates:", n_boot, "\n")
      cat("Confidence level:", conf_level, "\n")
      
      # Création des objets génétiques
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
      
      if (length(loci_names) == 0) {
        stop("No loci found in the data")
      }
      
      cat("Number of loci:", length(loci_names), "\n")
      cat("Loci names:", paste(loci_names, collapse = ", "), "\n")
      
      # Obtenir FST et FIS depuis hierfstat
      obs_wc_hier <- hierfstat::wc(mydata_hierfstat)
      
      # Vérifier les colonnes disponibles
      available_cols <- colnames(obs_wc_hier$per.loc)
      cat("Available columns in wc results:", paste(available_cols, collapse = ", "), "\n")
      
      # Extraire FST et FIS
      if ("FST" %in% available_cols && "FIS" %in% available_cols) {
        obs_fst_by_locus <- as.numeric(obs_wc_hier$per.loc[, "FST"])
        obs_fis_by_locus <- as.numeric(obs_wc_hier$per.loc[, "FIS"])
        
        # CALCULER FIT à partir de FST et FIS
        obs_fit_by_locus <- calculate_fit_from_fst_fis(obs_fst_by_locus, obs_fis_by_locus)
        obs_fit_overall <- mean(obs_fit_by_locus, na.rm = TRUE)
        
        cat("FIT calculation successful:\n")
        cat("  Mean FST:", round(mean(obs_fst_by_locus, na.rm = TRUE), 4), "\n")
        cat("  Mean FIS:", round(mean(obs_fis_by_locus, na.rm = TRUE), 4), "\n")
        cat("  Mean FIT:", round(obs_fit_overall, 4), "\n")
        
      } else {
        stop("Required columns FST and FIS not found in wc results. Available: ", paste(available_cols, collapse = ", "))
      }
      
      # Préparation des données pour C++
      hf <- as.data.frame(mydata_hierfstat)
      mat_int <- prepare_matrix_for_cpp_corrected(hf)
      
      cat("Data matrix prepared. Dimensions:", dim(mat_int), "\n")
      
      # Initialiser les résultats bootstrap
      boot_two_stage <- NULL
      boot_pop_only <- NULL
      
      # Bootstrap selon la méthode choisie
      if (bootstrap_method %in% c("two_stage", "both")) {
        cat("Running two-stage bootstrap...\n")
        boot_two_stage <- safe_boot_wc_two_stage_fast_cpp(
          mat_int = mat_int,
          colnames = names(hf),
          pop_col = 0L,
          missing_code = 0L,
          B = as.integer(n_boot),
          probs = c((1-conf_level)/2, 0.5, 1-(1-conf_level)/2),
          seed = as.integer(123),
          n_threads = 1L
        )
        cat("Two-stage bootstrap completed\n")
      }
      
      if (bootstrap_method %in% c("pop_only", "both")) {
        cat("Running population-only bootstrap...\n")
        boot_pop_only <- safe_boot_wc_pop_only_fast_cpp(
          mat_int = mat_int,
          colnames = names(hf),
          pop_col = 0L,
          missing_code = 0L,
          B = as.integer(n_boot),
          probs = c((1-conf_level)/2, 0.5, 1-(1-conf_level)/2),
          seed = as.integer(456),
          n_threads = 1L
        )
        cat("Population-only bootstrap completed\n")
      }
      
      # Construction des résultats
      results <- list(
        observed_fit = obs_fit_by_locus,
        observed_fit_overall = obs_fit_overall,
        observed_fst = obs_fst_by_locus,
        observed_fis = obs_fis_by_locus,
        loci_names = loci_names,
        bootstrap_method = bootstrap_method,
        conf_level = conf_level,
        n_boot = n_boot
      )
      
      if (bootstrap_method == "two_stage" || bootstrap_method == "both") {
        results$two_stage <- boot_two_stage
        if (!is.null(boot_two_stage)) {
          cat("Two-stage results structure:\n")
          cat("  FST dimensions:", if(!is.null(boot_two_stage$FST)) dim(boot_two_stage$FST) else "NULL", "\n")
          cat("  FIT dimensions:", if(!is.null(boot_two_stage$FIT)) dim(boot_two_stage$FIT) else "NULL", "\n")
        }
      }
      
      if (bootstrap_method == "pop_only" || bootstrap_method == "both") {
        results$pop_only <- boot_pop_only
        if (!is.null(boot_pop_only)) {
          cat("Pop-only results structure:\n")
          cat("  FST dimensions:", if(!is.null(boot_pop_only$FST)) dim(boot_pop_only$FST) else "NULL", "\n")
          cat("  FIT dimensions:", if(!is.null(boot_pop_only$FIT)) dim(boot_pop_only$FIT) else "NULL", "\n")
        }
      }
      
      cat("=== FIT BOOTSTRAP ANALYSIS COMPLETED SUCCESSFULLY ===\n")
      
      return(results)
      
    }, error = function(e) {
      cat("=== ERROR IN FIT BOOTSTRAP ANALYSIS ===\n")
      cat("Error message:", e$message, "\n")
      stop("Error in FIT bootstrap analysis: ", e$message)
    })
  }

  # ===== FONCTION BOOTSTRAP FST COMPLÈTE =====

  run_bootstrap_fst_analysis <- function(formatted_data, n_perm, n_boot, conf_level, bootstrap_method, fst_estimator, analysis_level) {
    set.seed(123)
    RNGkind("Mersenne-Twister")
    
    if (is.null(formatted_data) || is.null(formatted_data$haplotype)) {
      stop("Invalid formatted_data structure")
    }
    
    tryCatch({
      # Afficher les informations de débogage
      cat("=== STARTING FST BOOTSTRAP ANALYSIS ===\n")
      cat("Bootstrap method:", bootstrap_method, "\n")
      cat("FST estimator:", fst_estimator, "\n")
      cat("Number of bootstrap replicates:", n_boot, "\n")
      cat("Confidence level:", conf_level, "\n")
      
      # Création des objets génétiques
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
      pop_names <- unique(mydata_hierfstat[,1])
      
      if (length(loci_names) == 0) {
        stop("No loci found in the data")
      }
      
      cat("Number of loci:", length(loci_names), "\n")
      cat("Number of populations:", length(pop_names), "\n")
      cat("Loci names:", paste(loci_names, collapse = ", "), "\n")
      
      # FST observés
      obs_wc_hier <- hierfstat::wc(mydata_hierfstat)
      
      # Vérifier les colonnes disponibles
      available_cols <- colnames(obs_wc_hier$per.loc)
      cat("Available columns in wc results:", paste(available_cols, collapse = ", "), "\n")
      
      # Extraire FST de manière robuste
      if ("FST" %in% available_cols) {
        obs_fst_by_locus <- as.numeric(obs_wc_hier$per.loc[, "FST"])
        obs_fst_overall <- mean(obs_fst_by_locus, na.rm = TRUE)
        
        cat("FST extraction successful:\n")
        cat("  Mean FST:", round(obs_fst_overall, 4), "\n")
        cat("  FST range:", round(range(obs_fst_by_locus, na.rm = TRUE), 4), "\n")
        
      } else {
        stop("No FST column found in wc results. Available: ", paste(available_cols, collapse = ", "))
      }
      
      # Calcul des FST pairwise si demandé
      pairwise_fst <- NULL
      if (analysis_level %in% c("pairwise", "all")) {
        cat("Calculating pairwise FST...\n")
        pairwise_fst <- hierfstat::pairwise.WCfst(mydata_hierfstat)
        cat("Pairwise FST matrix dimensions:", dim(pairwise_fst), "\n")
      }
      
      # Préparation des données pour C++
      hf <- as.data.frame(mydata_hierfstat)
      mat_int <- prepare_matrix_for_cpp_corrected(hf)
      
      cat("Data matrix prepared. Dimensions:", dim(mat_int), "\n")
      
      # Initialiser les résultats bootstrap
      boot_two_stage <- NULL
      boot_pop_only <- NULL
      
      # Bootstrap selon la méthode choisie
      if (bootstrap_method %in% c("two_stage", "both")) {
        cat("Running two-stage bootstrap...\n")
        boot_two_stage <- safe_boot_wc_two_stage_fast_cpp(
          mat_int = mat_int,
          colnames = names(hf),
          pop_col = 0L,
          missing_code = 0L,
          B = as.integer(n_boot),
          probs = c((1-conf_level)/2, 0.5, 1-(1-conf_level)/2),
          seed = as.integer(123),
          n_threads = 1L
        )
        cat("Two-stage bootstrap completed\n")
      }
      
      if (bootstrap_method %in% c("pop_only", "both")) {
        cat("Running population-only bootstrap...\n")
        boot_pop_only <- safe_boot_wc_pop_only_fast_cpp(
          mat_int = mat_int,
          colnames = names(hf),
          pop_col = 0L,
          missing_code = 0L,
          B = as.integer(n_boot),
          probs = c((1-conf_level)/2, 0.5, 1-(1-conf_level)/2),
          seed = as.integer(456),
          n_threads = 1L
        )
        cat("Population-only bootstrap completed\n")
      }
      
      # Construction des résultats
      results <- list(
        observed_fst = obs_fst_by_locus,
        observed_fst_overall = obs_fst_overall,
        loci_names = loci_names,
        pop_names = pop_names,
        pairwise_fst = pairwise_fst,
        bootstrap_method = bootstrap_method,
        fst_estimator = fst_estimator,
        conf_level = conf_level,
        n_boot = n_boot
      )
      
      if (bootstrap_method == "two_stage" || bootstrap_method == "both") {
        results$two_stage <- boot_two_stage
        if (!is.null(boot_two_stage)) {
          cat("Two-stage results structure:\n")
          cat("  FST dimensions:", if(!is.null(boot_two_stage$FST)) dim(boot_two_stage$FST) else "NULL", "\n")
          cat("  FST overall length:", if(!is.null(boot_two_stage$FST_overall)) length(boot_two_stage$FST_overall) else "NULL", "\n")
        }
      }
      
      if (bootstrap_method == "pop_only" || bootstrap_method == "both") {
        results$pop_only <- boot_pop_only
        if (!is.null(boot_pop_only)) {
          cat("Pop-only results structure:\n")
          cat("  FST dimensions:", if(!is.null(boot_pop_only$FST)) dim(boot_pop_only$FST) else "NULL", "\n")
          cat("  FST overall length:", if(!is.null(boot_pop_only$FST_overall)) length(boot_pop_only$FST_overall) else "NULL", "\n")
        }
      }
      
      cat("=== FST BOOTSTRAP ANALYSIS COMPLETED SUCCESSFULLY ===\n")
      
      return(results)
      
    }, error = function(e) {
      cat("=== ERROR IN FST BOOTSTRAP ANALYSIS ===\n")
      cat("Error message:", e$message, "\n")
      stop("Error in FST bootstrap analysis: ", e$message)
    })
  }

  # Fonction corrigée pour préparer la matrice
  prepare_matrix_for_cpp_corrected <- function(hierfstat_data) {
    hf <- as.data.frame(hierfstat_data)
    
    cat("Original data dimensions:", dim(hf), "\n")
    cat("Column names:", colnames(hf), "\n")
    cat("First column type:", class(hf[[1]]), "\n")
    cat("First column values (sample):", head(hf[[1]]), "\n")
    
    # Conversion robuste de la colonne population
    if (!is.integer(hf[[1]])) {
      # Convertir en facteur puis en entier
      pop_factor <- as.factor(hf[[1]])
      hf[[1]] <- as.integer(pop_factor)
      cat("Converted population to integer. Unique values:", unique(hf[[1]]), "\n")
    }
    
    # Vérifier qu'on a bien des populations
    if (length(unique(hf[[1]])) == 0) {
      stop("No populations found after conversion!")
    }
    
    # Conversion robuste des génotypes
    for (j in 2:ncol(hf)) {
      if (!is.integer(hf[[j]])) {
        # Nettoyer et convertir en numérique d'abord
        geno_char <- as.character(hf[[j]])
        geno_numeric <- suppressWarnings(as.numeric(geno_char))
        # Remplacer les NA par 0 (code manquant)
        geno_numeric[is.na(geno_numeric)] <- 0L
        hf[[j]] <- as.integer(geno_numeric)
      }
    }
    
    colnames(hf)[1] <- "pop"
    
    # Vérifier que toutes les colonnes sont bien des entiers
    for (j in 1:ncol(hf)) {
      if (!is.integer(hf[[j]])) {
        hf[[j]] <- as.integer(hf[[j]])
      }
    }
    
    mat_int <- as.matrix(hf)
    storage.mode(mat_int) <- "integer"
    
    cat("Final matrix - Populations:", length(unique(mat_int[,1])), "\n")
    cat("Final matrix - Loci:", ncol(mat_int) - 1, "\n")
    cat("Final matrix dimensions:", dim(mat_int), "\n")
    
    return(mat_int)
  }

  ###

  # ===== CORRECTION DES FONCTIONS FALLBACK - PROBLÈME D'INDEX =====

  # Version corrigée du bootstrap two-stage - CORRECTION DES INDICES
  boot_wc_two_stage_fallback_r_corrected <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting CORRECTED R two-stage bootstrap fallback with", B, "replicates\n")
    
    # CORRECTION: Convertir l'index C++ (0-based) en index R (1-based)
    pop_col_r <- pop_col + 1
    cat("Population column - C++ index:", pop_col, "R index:", pop_col_r, "\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col_r]  # CORRECTION: utiliser pop_col_r
    L <- length(loci_names)
    
    # Obtenir les populations - CORRECTION: utiliser pop_col_r
    pop_ids <- unique(mat_int[, pop_col_r])
    R <- length(pop_ids)
    
    cat("  Populations found:", R, "-", paste(pop_ids, collapse = ", "), "\n")
    cat("  Loci:", L, "-", paste(loci_names, collapse = ", "), "\n")
    
    if (R == 0) {
      stop("No populations found in the data! Check pop_col conversion.")
    }
    
    # Stockage des résultats
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    # Donner des noms aux colonnes
    colnames(boot_FST) <- colnames(boot_FIT) <- colnames(boot_HI) <- colnames(boot_HS) <- loci_names
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Étape 1: Rééchantillonnage des populations
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Étape 2: Rééchantillonnage des individus dans chaque population
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        # CORRECTION: utiliser pop_col_r
        pop_rows <- which(mat_int[, pop_col_r] == pop_id)
        if (length(pop_rows) > 0) {
          # Rééchantillonnage avec remise dans cette population
          boot_pop_rows <- sample(pop_rows, length(pop_rows), replace = TRUE)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[boot_pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            # CORRECTION: utiliser pop_col_r pour la colonne population
            boot_df[,pop_col_r] <- as.factor(boot_df[,pop_col_r])
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                # Vérifier que boot_FST[b, ] n'est pas NA avant de calculer FIT
                if (all(!is.na(boot_FST[b, ]))) {
                  boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
                }
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Vérifier qu'on a des données
    cat("Bootstrap completion summary:\n")
    cat("  FST - non-NA values:", sum(!is.na(boot_FST)), "/", length(boot_FST), "\n")
    cat("  FIT - non-NA values:", sum(!is.na(boot_FIT)), "/", length(boot_FIT), "\n")
    
    # Calcul des quantiles avec gestion des cas vides
    calculate_quantiles <- function(boot_matrix, probs, loci_names) {
      # Vérifier si la matrice a des données
      if (all(is.na(boot_matrix))) {
        warning("All bootstrap values are NA for this statistic")
        result <- matrix(NA, nrow = length(probs), ncol = length(loci_names))
      } else {
        result <- apply(boot_matrix, 2, function(x) {
          x_clean <- x[!is.na(x)]
          if (length(x_clean) > 0) {
            quantile(x_clean, probs = probs, na.rm = TRUE)
          } else {
            rep(NA, length(probs))
          }
        })
      }
      
      # S'assurer que c'est une matrice avec des dimensions
      if (!is.matrix(result)) {
        result <- matrix(result, nrow = length(probs), ncol = length(loci_names))
      }
      
      # Définir les noms de ligne et colonne
      rownames(result) <- as.character(probs)
      colnames(result) <- loci_names
      
      return(result)
    }
    
    # Calcul des quantiles avec vérification
    Q_FST <- calculate_quantiles(boot_FST, probs, loci_names)
    Q_FIT <- calculate_quantiles(boot_FIT, probs, loci_names)
    Q_HI <- calculate_quantiles(boot_HI, probs, loci_names)
    Q_HS <- calculate_quantiles(boot_HS, probs, loci_names)
    
    # Quantiles globaux avec vérification
    safe_quantile <- function(x, probs) {
      x_clean <- as.vector(x)[!is.na(as.vector(x))]
      if (length(x_clean) > 0) {
        result <- quantile(x_clean, probs = probs, na.rm = TRUE)
        names(result) <- as.character(probs)
        return(result)
      } else {
        result <- rep(NA, length(probs))
        names(result) <- as.character(probs)
        return(result)
      }
    }
    
    Q_FST_overall <- safe_quantile(boot_FST, probs)
    Q_FIT_overall <- safe_quantile(boot_FIT, probs)
    Q_HI_overall <- safe_quantile(boot_HI, probs)
    Q_HS_overall <- safe_quantile(boot_HS, probs)
    
    cat("R two-stage bootstrap completed successfully\n")
    cat("Result dimensions - FST:", dim(Q_FST), "FIT:", dim(Q_FIT), "\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # Version corrigée du bootstrap population-only - CORRECTION DES INDICES
  boot_wc_pop_only_fallback_r_corrected <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting CORRECTED R population-only bootstrap fallback with", B, "replicates\n")
    
    # CORRECTION: Convertir l'index C++ (0-based) en index R (1-based)
    pop_col_r <- pop_col + 1
    cat("Population column - C++ index:", pop_col, "R index:", pop_col_r, "\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col_r]  # CORRECTION: utiliser pop_col_r
    L <- length(loci_names)
    
    # Obtenir les populations - CORRECTION: utiliser pop_col_r
    pop_ids <- unique(mat_int[, pop_col_r])
    R <- length(pop_ids)
    
    cat("  Populations found:", R, "-", paste(pop_ids, collapse = ", "), "\n")
    cat("  Loci:", L, "-", paste(loci_names, collapse = ", "), "\n")
    
    if (R == 0) {
      stop("No populations found in the data! Check pop_col conversion.")
    }
    
    # Stockage des résultats
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    # Donner des noms aux colonnes
    colnames(boot_FST) <- colnames(boot_FIT) <- colnames(boot_HI) <- colnames(boot_HS) <- loci_names
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Rééchantillonnage des populations seulement
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Construire l'échantillon bootstrap
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        # CORRECTION: utiliser pop_col_r
        pop_rows <- which(mat_int[, pop_col_r] == pop_id)
        if (length(pop_rows) > 0) {
          # Prendre tous les individus de cette population (sans rééchantillonnage)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            # CORRECTION: utiliser pop_col_r pour la colonne population
            boot_df[,pop_col_r] <- as.factor(boot_df[,pop_col_r])
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                # Vérifier que boot_FST[b, ] n'est pas NA avant de calculer FIT
                if (all(!is.na(boot_FST[b, ]))) {
                  boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
                }
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Vérifier qu'on a des données
    cat("Bootstrap completion summary:\n")
    cat("  FST - non-NA values:", sum(!is.na(boot_FST)), "/", length(boot_FST), "\n")
    cat("  FIT - non-NA values:", sum(!is.na(boot_FIT)), "/", length(boot_FIT), "\n")
    
    # Calcul des quantiles (même fonction que pour two-stage)
    calculate_quantiles <- function(boot_matrix, probs, loci_names) {
      if (all(is.na(boot_matrix))) {
        warning("All bootstrap values are NA for this statistic")
        result <- matrix(NA, nrow = length(probs), ncol = length(loci_names))
      } else {
        result <- apply(boot_matrix, 2, function(x) {
          x_clean <- x[!is.na(x)]
          if (length(x_clean) > 0) {
            quantile(x_clean, probs = probs, na.rm = TRUE)
          } else {
            rep(NA, length(probs))
          }
        })
      }
      
      if (!is.matrix(result)) {
        result <- matrix(result, nrow = length(probs), ncol = length(loci_names))
      }
      
      rownames(result) <- as.character(probs)
      colnames(result) <- loci_names
      
      return(result)
    }
    
    Q_FST <- calculate_quantiles(boot_FST, probs, loci_names)
    Q_FIT <- calculate_quantiles(boot_FIT, probs, loci_names)
    Q_HI <- calculate_quantiles(boot_HI, probs, loci_names)
    Q_HS <- calculate_quantiles(boot_HS, probs, loci_names)
    
    # Quantiles globaux
    safe_quantile <- function(x, probs) {
      x_clean <- as.vector(x)[!is.na(as.vector(x))]
      if (length(x_clean) > 0) {
        result <- quantile(x_clean, probs = probs, na.rm = TRUE)
        names(result) <- as.character(probs)
        return(result)
      } else {
        result <- rep(NA, length(probs))
        names(result) <- as.character(probs)
        return(result)
      }
    }
    
    Q_FST_overall <- safe_quantile(boot_FST, probs)
    Q_FIT_overall <- safe_quantile(boot_FIT, probs)
    Q_HI_overall <- safe_quantile(boot_HI, probs)
    Q_HS_overall <- safe_quantile(boot_HS, probs)
    
    cat("R population-only bootstrap completed successfully\n")
    cat("Result dimensions - FST:", dim(Q_FST), "FIT:", dim(Q_FIT), "\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # ===== FONCTIONS SAFE POUR BOOTSTRAP =====

  safe_boot_wc_two_stage_fast_cpp <- function(mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads) {
    cat("Checking for two-stage C++ function...\n")
    
    # Diagnostic de la structure de la matrice
    diagnose_matrix_structure(mat_int, pop_col)
    
    if (exists("boot_wc_two_stage_fast_cpp")) {
      cat("Using C++ two-stage bootstrap function\n")
      return(boot_wc_two_stage_fast_cpp(
        mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads
      ))
    } else {
      cat("Using CORRECTED R fallback for two-stage bootstrap\n")
      return(boot_wc_two_stage_fallback_r_corrected(mat_int, pop_col, missing_code, B, probs))
    }
  }

  safe_boot_wc_pop_only_fast_cpp <- function(mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads) {
    cat("Checking for pop-only C++ function...\n")
    
    # Diagnostic de la structure de la matrice
    diagnose_matrix_structure(mat_int, pop_col)
    
    if (exists("boot_wc_pop_only_fast_cpp")) {
      cat("Using C++ population-only bootstrap function\n")
      return(boot_wc_pop_only_fast_cpp(
        mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads
      ))
    } else {
      cat("Using CORRECTED R fallback for population-only bootstrap\n")
      return(boot_wc_pop_only_fallback_r_corrected(mat_int, pop_col, missing_code, B, probs))
    }
  }

  # ===== FONCTION DE DIAGNOSTIC =====

  diagnose_matrix_structure <- function(mat_int, pop_col_cpp) {
    cat("=== MATRIX DIAGNOSIS ===\n")
    cat("Matrix dimensions:", dim(mat_int), "\n")
    cat("C++ pop_col:", pop_col_cpp, "\n")
    cat("R pop_col:", pop_col_cpp + 1, "\n")
    cat("Column names:", colnames(mat_int), "\n")
    
    # Vérifier la colonne population
    pop_col_r <- pop_col_cpp + 1
    if (pop_col_r <= ncol(mat_int)) {
      pop_values <- unique(mat_int[, pop_col_r])
      cat("Population values:", paste(pop_values, collapse = ", "), "\n")
      cat("Number of populations:", length(pop_values), "\n")
    } else {
      cat("ERROR: pop_col_r", pop_col_r, "exceeds matrix columns", ncol(mat_int), "\n")
    }
    
    # Vérifier les loci
    if (pop_col_r <= ncol(mat_int)) {
      loci_names <- colnames(mat_int)[-pop_col_r]
      cat("Loci names:", paste(loci_names, collapse = ", "), "\n")
      cat("Number of loci:", length(loci_names), "\n")
    }
    cat("=====================\n")
  }

  # ===== FONCTIONS FALLBACK EN R =====

  # Version R de secours pour le bootstrap two-stage
  boot_wc_two_stage_fallback_r <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting R two-stage bootstrap fallback with", B, "replicates\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col]
    L <- length(loci_names)
    
    # Obtenir les populations
    pop_ids <- unique(mat_int[, pop_col])
    R <- length(pop_ids)
    
    cat("  Populations:", R, "\n")
    cat("  Loci:", L, "\n")
    
    # Stockage des résultats
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Étape 1: Rééchantillonnage des populations
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Étape 2: Rééchantillonnage des individus dans chaque population
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        pop_rows <- which(mat_int[, pop_col] == pop_id)
        if (length(pop_rows) > 0) {
          # Rééchantillonnage avec remise dans cette population
          boot_pop_rows <- sample(pop_rows, length(pop_rows), replace = TRUE)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[boot_pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            boot_df[,1] <- as.factor(boot_df[,1])  # colonne population
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Calcul des quantiles
    calculate_quantiles <- function(boot_matrix, probs) {
      result <- apply(boot_matrix, 2, function(x) {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) > 0) {
          quantile(x_clean, probs = probs, na.rm = TRUE)
        } else {
          rep(NA, length(probs))
        }
      })
      return(result)
    }
    
    Q_FST <- calculate_quantiles(boot_FST, probs)
    Q_FIT <- calculate_quantiles(boot_FIT, probs)
    Q_HI <- calculate_quantiles(boot_HI, probs)
    Q_HS <- calculate_quantiles(boot_HS, probs)
    
    # Quantiles globaux
    Q_FST_overall <- quantile(as.vector(boot_FST), probs = probs, na.rm = TRUE)
    Q_FIT_overall <- quantile(as.vector(boot_FIT), probs = probs, na.rm = TRUE)
    Q_HI_overall <- quantile(as.vector(boot_HI), probs = probs, na.rm = TRUE)
    Q_HS_overall <- quantile(as.vector(boot_HS), probs = probs, na.rm = TRUE)
    
    # Préparer les matrices de sortie
    rownames(Q_FST) <- rownames(Q_FIT) <- rownames(Q_HI) <- rownames(Q_HS) <- as.character(probs)
    colnames(Q_FST) <- colnames(Q_FIT) <- colnames(Q_HI) <- colnames(Q_HS) <- loci_names
    
    cat("R two-stage bootstrap completed successfully\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # Version R de secours pour le bootstrap population-only
  boot_wc_pop_only_fallback_r <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting R population-only bootstrap fallback with", B, "replicates\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col]
    L <- length(loci_names)
    
    # Obtenir les populations
    pop_ids <- unique(mat_int[, pop_col])
    R <- length(pop_ids)
    
    cat("  Populations:", R, "\n")
    cat("  Loci:", L, "\n")
    
    # Stockage des résultats
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Rééchantillonnage des populations seulement (sans rééchantillonnage des individus)
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Construire l'échantillon bootstrap en prenant toutes les données de chaque population échantillonnée
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        pop_rows <- which(mat_int[, pop_col] == pop_id)
        if (length(pop_rows) > 0) {
          # Prendre tous les individus de cette population (sans rééchantillonnage)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            boot_df[,1] <- as.factor(boot_df[,1])  # colonne population
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Calcul des quantiles
    calculate_quantiles <- function(boot_matrix, probs) {
      result <- apply(boot_matrix, 2, function(x) {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) > 0) {
          quantile(x_clean, probs = probs, na.rm = TRUE)
        } else {
          rep(NA, length(probs))
        }
      })
      return(result)
    }
    
    Q_FST <- calculate_quantiles(boot_FST, probs)
    Q_FIT <- calculate_quantiles(boot_FIT, probs)
    Q_HI <- calculate_quantiles(boot_HI, probs)
    Q_HS <- calculate_quantiles(boot_HS, probs)
    
    # Quantiles globaux
    Q_FST_overall <- quantile(as.vector(boot_FST), probs = probs, na.rm = TRUE)
    Q_FIT_overall <- quantile(as.vector(boot_FIT), probs = probs, na.rm = TRUE)
    Q_HI_overall <- quantile(as.vector(boot_HI), probs = probs, na.rm = TRUE)
    Q_HS_overall <- quantile(as.vector(boot_HS), probs = probs, na.rm = TRUE)
    
    # Préparer les matrices de sortie
    rownames(Q_FST) <- rownames(Q_FIT) <- rownames(Q_HI) <- rownames(Q_HS) <- as.character(probs)
    colnames(Q_FST) <- colnames(Q_FIT) <- colnames(Q_HI) <- colnames(Q_HS) <- loci_names
    
    cat("R population-only bootstrap completed successfully\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # ===== REACTIVE VALUES POUR FIT ET FST =====

  # FIT Analysis
  fit_boot_results <- reactiveVal(NULL)
  fit_boot_timing <- reactiveVal(NULL)
  fit_perm_results <- reactiveVal(NULL)

  # FST Analysis  
  fst_boot_results <- reactiveVal(NULL)
  fst_boot_timing <- reactiveVal(NULL)
  fst_perm_results <- reactiveVal(NULL)

  # ===== OBSERVE EVENTS POUR FIT ET FST =====

  # FIT Analysis Trigger
  observeEvent(input$run_bootstrap_fit, {
    if (is.null(formatted_data)) {
      showNotification("No genetic data available", type = "error")
      return(NULL)
    }
    
    # Validation des paramètres
    if (input$n_boot_fit < 10) {
      showNotification("Number of bootstrap replicates should be at least 10", type = "warning")
      return(NULL)
    }
    
    waiter <- Waiter$new(
      id = c("fit_results_table", "permutation_plot_fit", "bootstrap_plot_fit"),
      html = spin_3(), 
      color = transparent(0.7)
    )
    waiter$show()
    
    on.exit(waiter$hide())
    
    tryCatch({
      start_time <- Sys.time()
      
      results <- run_bootstrap_fit_analysis(
        formatted_data = formatted_data,
        n_perm = input$n_perm_fit,
        n_boot = input$n_boot_fit,
        conf_level = input$conf_level_fit,
        bootstrap_method = input$bootstrap_method_fit,
        analysis_level = input$analysis_level_fit
      )
      
      formatted_results <- format_fit_results(results)
      
      end_time <- Sys.time()
      duration <- round(difftime(end_time, start_time, units = "secs"), 1)
      fit_boot_timing(duration)
      fit_boot_results(list(
        raw_results = results,
        formatted_table = formatted_results
      ))
      
      showNotification("FIT bootstrap analysis completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in FIT analysis:", e$message), type = "error")
      fit_boot_results(NULL)
    })
  })

  # FST Analysis Trigger
  observeEvent(input$run_bootstrap_fst, {
    if (is.null(formatted_data)) {
      showNotification("No genetic data available", type = "error")
      return(NULL)
    }
    
    # Validation des paramètres
    if (input$n_boot_fst < 10) {
      showNotification("Number of bootstrap replicates should be at least 10", type = "warning")
      return(NULL)
    }
    
    waiter <- Waiter$new(
      id = c("fst_results_table", "permutation_plot_fst", "bootstrap_plot_fst"),
      html = spin_3(), 
      color = transparent(0.7)
    )
    waiter$show()
    
    on.exit(waiter$hide())
    
    tryCatch({
      start_time <- Sys.time()
      
      results <- run_bootstrap_fst_analysis(
        formatted_data = formatted_data,
        n_perm = input$n_perm_fst,
        n_boot = input$n_boot_fst,
        conf_level = input$conf_level_fst,
        bootstrap_method = input$bootstrap_method_fst,
        fst_estimator = input$fst_estimator,
        analysis_level = input$analysis_level_fst
      )
      
      formatted_results <- format_fst_results(results)
      
      end_time <- Sys.time()
      duration <- round(difftime(end_time, start_time, units = "secs"), 1)
      fst_boot_timing(duration)
      fst_boot_results(list(
        raw_results = results,
        formatted_results = formatted_results
      ))
      
      showNotification("FST bootstrap analysis completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in FST analysis:", e$message), type = "error")
      fst_boot_results(NULL)
    })
  })

  # ===== VALUE BOXES POUR FIT =====

  output$global_fit_box <- renderValueBox({
    req(fit_boot_results())
    fit_val <- fit_boot_results()$formatted_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(Observed_FIT)
    fit_display <- ifelse(abs(fit_val) < 0.0001, "≈ 0.0000", format(round(fit_val, 4), nsmall = 4))
    color <- ifelse(fit_val > 0.2, "red", ifelse(fit_val > 0.1, "yellow", "green"))
    valueBox(
      value = fit_display,
      subtitle = HTML("<small>Global F<sub>IT</sub><br>Total inbreeding</small>"),
      color = color,
      icon = icon("dna")
    )
  })

  output$global_fit_pvalue_box <- renderValueBox({
    req(fit_boot_results())
    # Pour FIT, on peut calculer une p-value basée sur la déviation de zéro
    fit_val <- fit_boot_results()$formatted_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(Observed_FIT)
    # P-value approximative basée sur la magnitude
    p_val <- ifelse(abs(fit_val) < 0.01, 1.0, 
                  ifelse(abs(fit_val) < 0.05, 0.5, 0.01))
    p_display <- ifelse(p_val < 0.001, "< 0.001", format(round(p_val, 4), nsmall = 4))
    color <- ifelse(p_val < 0.05, "yellow", "green")
    valueBox(
      value = p_display,
      subtitle = HTML("<small>Significance<br>Deviation from zero</small>"),
      color = color,
      icon = icon("chart-line")
    )
  })

  output$fit_ci_width_box <- renderValueBox({
    req(fit_boot_results())
    ci_data <- fit_boot_results()$formatted_table %>%
      dplyr::filter(Locus != "Overall")
    if ("TwoStage_CI_U" %in% colnames(ci_data) && "TwoStage_CI_L" %in% colnames(ci_data)) {
      ci_widths <- ci_data$TwoStage_CI_U - ci_data$TwoStage_CI_L
      avg_width <- mean(ci_widths, na.rm = TRUE)
    } else if ("PopOnly_CI_U" %in% colnames(ci_data) && "PopOnly_CI_L" %in% colnames(ci_data)) {
      ci_widths <- ci_data$PopOnly_CI_U - ci_data$PopOnly_CI_L
      avg_width <- mean(ci_widths, na.rm = TRUE)
    } else {
      avg_width <- NA
    }
    width_display <- ifelse(is.na(avg_width), "N/A", format(round(avg_width, 4), nsmall = 4))
    color <- ifelse(is.na(avg_width), "red", ifelse(avg_width > 0.2, "yellow", "green"))
    valueBox(
      value = width_display,
      subtitle = HTML("<small>Avg CI Width<br>Precision measure</small>"),
      color = color,
      icon = icon("arrows-alt-h")
    )
  })

  output$analysis_time_fit_box <- renderValueBox({
    req(fit_boot_timing())
    time_sec <- fit_boot_timing()
    time_display <- ifelse(time_sec < 60, 
                          paste0(time_sec, " s"), 
                          paste0(round(time_sec / 60, 1), " min"))
    valueBox(
      value = time_display,
      subtitle = HTML("<small>Computation Time<br>FIT Analysis</small>"),
      color = "purple",
      icon = icon("clock")
    )
  })

  # ===== VALUE BOXES POUR FST =====

  output$global_fst_box <- renderValueBox({
    req(fst_boot_results())
    fst_val <- fst_boot_results()$formatted_results$locus_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(Observed_FST)
    fst_display <- ifelse(abs(fst_val) < 0.0001, "≈ 0.0000", format(round(fst_val, 4), nsmall = 4))
    color <- ifelse(fst_val > 0.15, "red", ifelse(fst_val > 0.05, "yellow", "green"))
    valueBox(
      value = fst_display,
      subtitle = HTML("<small>Global F<sub>ST</sub><br>Population differentiation</small>"),
      color = color,
      icon = icon("globe-americas")
    )
  })

  output$global_fst_pvalue_box <- renderValueBox({
    req(fst_boot_results())
    fst_val <- fst_boot_results()$formatted_results$locus_table %>%
      dplyr::filter(Locus == "Overall") %>%
      dplyr::pull(Observed_FST)
    # P-value approximative basée sur la magnitude de FST
    p_val <- ifelse(fst_val < 0.01, 1.0, 
                  ifelse(fst_val < 0.05, 0.1, 0.001))
    p_display <- ifelse(p_val < 0.001, "< 0.001", format(round(p_val, 4), nsmall = 4))
    color <- ifelse(p_val < 0.05, "yellow", "green")
    valueBox(
      value = p_display,
      subtitle = HTML("<small>Significance<br>Differentiation test</small>"),
      color = color,
      icon = icon("signature")
    )
  })

  output$fst_ci_width_box <- renderValueBox({
    req(fst_boot_results())
    ci_data <- fst_boot_results()$formatted_results$locus_table %>%
      dplyr::filter(Locus != "Overall")
    if ("TwoStage_CI_U" %in% colnames(ci_data) && "TwoStage_CI_L" %in% colnames(ci_data)) {
      ci_widths <- ci_data$TwoStage_CI_U - ci_data$TwoStage_CI_L
      avg_width <- mean(ci_widths, na.rm = TRUE)
    } else if ("PopOnly_CI_U" %in% colnames(ci_data) && "PopOnly_CI_L" %in% colnames(ci_data)) {
      ci_widths <- ci_data$PopOnly_CI_U - ci_data$PopOnly_CI_L
      avg_width <- mean(ci_widths, na.rm = TRUE)
    } else {
      avg_width <- NA
    }
    width_display <- ifelse(is.na(avg_width), "N/A", format(round(avg_width, 4), nsmall = 4))
    color <- ifelse(is.na(avg_width), "red", ifelse(avg_width > 0.15, "yellow", "green"))
    valueBox(
      value = width_display,
      subtitle = HTML("<small>Avg CI Width<br>Precision measure</small>"),
      color = color,
      icon = icon("expand-arrows-alt")
    )
  })

  output$analysis_time_fst_box <- renderValueBox({
    req(fst_boot_timing())
    time_sec <- fst_boot_timing()
    time_display <- ifelse(time_sec < 60, 
                          paste0(time_sec, " s"), 
                          paste0(round(time_sec / 60, 1), " min"))
    valueBox(
      value = time_display,
      subtitle = HTML("<small>Computation Time<br>FST Analysis</small>"),
      color = "purple",
      icon = icon("stopwatch")
    )
  })

  # ===== TABLES DE RESULTATS =====

  # Table FIT
  output$fit_results_table <- DT::renderDT({
    req(fit_boot_results())
    
    df <- fit_boot_results()$formatted_table
    
    # Séparer Overall des autres loci
    df_overall <- df %>% dplyr::filter(Locus == "Overall")
    df_loci <- df %>% dplyr::filter(Locus != "Overall")

    # Appliquer les filtres selon la vue sélectionnée
    if (input$results_view_fit == "significant") {
      # Pour FIT, on peut filtrer basé sur la magnitude
      df_loci <- df_loci %>% dplyr::filter(abs(Observed_FIT) > 0.05)
      df <- rbind(df_loci, df_overall)
    } else if (input$results_view_fit == "by_pop") {
      # FIT est global, donc on montre tous les loci (sans Overall)
      df <- df_loci
    } else if (input$results_view_fit == "global") {
      df <- df_overall
    } else {
      # Trier les loci selon le critère choisi, puis ajouter Overall à la fin
      if (input$sort_by_fit == "fit") {
        df_loci <- df_loci %>% dplyr::arrange(desc(Observed_FIT))
      } else if (input$sort_by_fit == "pvalue") {
        # Pour FIT, on utilise la magnitude comme proxy de p-value
        df_loci <- df_loci %>% dplyr::arrange(desc(abs(Observed_FIT)))
      } else if (input$sort_by_fit == "ci_width") {
        if ("TwoStage_CI_U" %in% colnames(df_loci)) {
          df_loci$CI_Width <- df_loci$TwoStage_CI_U - df_loci$TwoStage_CI_L
          df_loci <- df_loci %>% dplyr::arrange(desc(CI_Width))
        }
      } else if (input$sort_by_fit == "locus") {
        df_loci <- df_loci %>% dplyr::arrange(Locus)
      }
      df <- rbind(df_loci, df_overall)
    }

    DT::datatable(
      df,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = sapply(df, is.numeric), digits = 4)
  })

  # Table FST
  output$fst_results_table <- DT::renderDT({
    req(fst_boot_results())
    
    df <- fst_boot_results()$formatted_results$locus_table
    
    # Séparer Overall des autres loci
    df_overall <- df %>% dplyr::filter(Locus == "Overall")
    df_loci <- df %>% dplyr::filter(Locus != "Overall")

    # Appliquer les filtres
    if (input$results_view_fst == "significant") {
      df_loci <- df_loci %>% dplyr::filter(Observed_FST > 0.05)
      df <- rbind(df_loci, df_overall)
    } else if (input$results_view_fst == "by_pop") {
      df <- df_loci
    } else if (input$results_view_fst == "global") {
      df <- df_overall
    } else if (input$results_view_fst == "pairwise") {
      # Pour pairwise, on retourne un message
      return(DT::datatable(
        data.frame(Message = "Select 'Pairwise Comparisons' tab for pairwise FST results"),
        options = list(dom = 't')
      ))
    } else {
      # Trier les loci selon le critère choisi, puis ajouter Overall à la fin
      if (input$sort_by_fst == "fst") {
        df_loci <- df_loci %>% dplyr::arrange(desc(Observed_FST))
      } else if (input$sort_by_fst == "pvalue") {
        df_loci <- df_loci %>% dplyr::arrange(desc(Observed_FST))  # Utilise FST comme proxy
      } else if (input$sort_by_fst == "ci_width") {
        if ("TwoStage_CI_U" %in% colnames(df_loci)) {
          df_loci$CI_Width <- df_loci$TwoStage_CI_U - df_loci$TwoStage_CI_L
          df_loci <- df_loci %>% dplyr::arrange(desc(CI_Width))
        }
      } else if (input$sort_by_fst == "differentiation") {
        df_loci <- df_loci %>% dplyr::arrange(desc(Observed_FST))
      } else if (input$sort_by_fst == "locus") {
        df_loci <- df_loci %>% dplyr::arrange(Locus)
      }
      df <- rbind(df_loci, df_overall)
    }

    DT::datatable(
      df,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = sapply(df, is.numeric), digits = 4)
  })

  # Table FST Pairwise
  output$pairwise_fst_table <- DT::renderDT({
    req(fst_boot_results(), fst_boot_results()$formatted_results$pairwise_fst)
    
    pairwise_matrix <- fst_boot_results()$formatted_results$pairwise_fst
    
    DT::datatable(
      as.data.frame(pairwise_matrix),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10,
        scrollX = TRUE
      )
    ) %>%
      DT::formatRound(columns = 1:ncol(pairwise_matrix), digits = 4)
  })

  # ===== GRAPHIQUES FIT =====

  output$permutation_plot_fit <- renderPlot({
    req(fit_boot_results())
    
    # Pour FIT, on peut créer un graphique de distribution des valeurs observées
    df <- fit_boot_results()$formatted_table %>%
      dplyr::filter(Locus != "Overall")
    
    ggplot(df, aes(x = Observed_FIT)) +
      geom_histogram(fill = "lightblue", alpha = 0.7, bins = input$perm_bins_fit) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Distribution of FIT Values Across Loci",
        x = "FIT",
        y = "Number of Loci"
      ) +
      theme_minimal()
  })

  output$bootstrap_plot_fit <- renderPlot({
    req(fit_boot_results())
    
    df <- fit_boot_results()$formatted_table %>%
      dplyr::filter(Locus != "Overall")
    
    # Determine which columns are available
    available_cols <- c("Observed_FIT")
    if ("TwoStage_Median" %in% colnames(df)) {
      available_cols <- c(available_cols, "TwoStage_Median")
    }
    if ("PopOnly_Median" %in% colnames(df)) {
      available_cols <- c(available_cols, "PopOnly_Median")
    }

    # Create long format with only available columns
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = all_of(available_cols),
        names_to = "Type",
        values_to = "Value"
      ) %>%
      dplyr::filter(!is.na(Value))
    
    ggplot(df_long, aes(x = Locus, y = Value, fill = Type)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = "FIT Values by Locus and Method",
        x = "Locus",
        y = "FIT"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # output$bootstrap_plot_fit <- renderPlot({
  #   req(fit_boot_results())
    
  #   df <- fit_boot_results()$formatted_table %>%
  #     dplyr::filter(Locus != "Overall")
    
  #   # Créer un boxplot des FIT par locus
  #   df_long <- df %>%
  #     tidyr::pivot_longer(
  #       cols = c(Observed_FIT, TwoStage_Median, PopOnly_Median),
  #       names_to = "Type",
  #       values_to = "Value"
  #     ) %>%
  #     dplyr::filter(!is.na(Value))
    
  #   ggplot(df_long, aes(x = Locus, y = Value, fill = Type)) +
  #     geom_boxplot(alpha = 0.7) +
  #     labs(
  #       title = "FIT Values by Locus and Method",
  #       x = "Locus",
  #       y = "FIT"
  #     ) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })

  # ===== GRAPHIQUES FST =====

  output$permutation_plot_fst <- renderPlot({
    req(fst_boot_results())
    
    df <- fst_boot_results()$formatted_results$locus_table %>%
      dplyr::filter(Locus != "Overall")
    
    ggplot(df, aes(x = Observed_FST)) +
      geom_histogram(fill = "lightgreen", alpha = 0.7, bins = input$perm_bins_fst) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Distribution of FST Values Across Loci",
        x = "FST",
        y = "Number of Loci"
      ) +
      theme_minimal()
  })

  output$bootstrap_plot_fst <- renderPlot({
    req(fst_boot_results())
    
    df <- fst_boot_results()$formatted_results$locus_table %>%
      dplyr::filter(Locus != "Overall")
    
    # Determine which columns are available
    available_cols <- c("Observed_FST")
    if ("TwoStage_Median" %in% colnames(df)) {
      available_cols <- c(available_cols, "TwoStage_Median")
    }
    if ("PopOnly_Median" %in% colnames(df)) {
      available_cols <- c(available_cols, "PopOnly_Median")
    }
    # Create long format with only available columns
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = all_of(available_cols),
        names_to = "Type",
        values_to = "Value"
      ) %>%
      dplyr::filter(!is.na(Value))
    
    ggplot(df_long, aes(x = Locus, y = Value, fill = Type)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = "FST Values by Locus and Method",
        x = "Locus",
        y = "FST"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # output$bootstrap_plot_fst <- renderPlot({
  #   req(fst_boot_results())
    
  #   df <- fst_boot_results()$formatted_results$locus_table %>%
  #     dplyr::filter(Locus != "Overall")
    
  #   df_long <- df %>%
  #     tidyr::pivot_longer(
  #       cols = c(Observed_FST, TwoStage_Median, PopOnly_Median),
  #       names_to = "Type",
  #       values_to = "Value"
  #     ) %>%
  #     dplyr::filter(!is.na(Value))
    
  #   ggplot(df_long, aes(x = Locus, y = Value, fill = Type)) +
  #     geom_boxplot(alpha = 0.7) +
  #     labs(
  #       title = "FST Values by Locus and Method",
  #       x = "Locus",
  #       y = "FST"
  #     ) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })

  # Heatmap FST pairwise
  output$fst_heatmap <- renderPlot({
    req(fst_boot_results(), fst_boot_results()$formatted_results$pairwise_fst)
    
    pairwise_matrix <- fst_boot_results()$formatted_results$pairwise_fst
    
    # Convertir en format long pour ggplot
    pairwise_long <- as.data.frame(pairwise_matrix) %>%
      tibble::rownames_to_column("Pop1") %>%
      tidyr::pivot_longer(
        cols = -Pop1,
        names_to = "Pop2",
        values_to = "FST"
      )
    
    ggplot(pairwise_long, aes(x = Pop1, y = Pop2, fill = FST)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "blue", 
        high = "red", 
        mid = "white",
        midpoint = mean(pairwise_matrix, na.rm = TRUE)
      ) +
      labs(
        title = "Pairwise FST Heatmap",
        x = "Population 1",
        y = "Population 2"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ===== FONCTIONS DE TELECHARGEMENT =====

  # Téléchargement FIT
  output$download_fit_table <- downloadHandler(
    filename = function() { paste("fit_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      req(fit_boot_results())
      write.csv(fit_boot_results()$formatted_table, file, row.names = FALSE)
    }
  )

  output$download_fit_table_txt <- downloadHandler(
    filename = function() { paste("fit_results_", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      req(fit_boot_results())
      write.table(fit_boot_results()$formatted_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # Téléchargement FST
  output$download_fst_table <- downloadHandler(
    filename = function() { paste("fst_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      req(fst_boot_results())
      write.csv(fst_boot_results()$formatted_results$locus_table, file, row.names = FALSE)
    }
  )

  output$download_fst_table_txt <- downloadHandler(
    filename = function() { paste("fst_results_", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      req(fst_boot_results())
      write.table(fst_boot_results()$formatted_results$locus_table, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # Téléchargement FST pairwise
  output$download_pairwise_fst <- downloadHandler(
    filename = function() { paste("pairwise_fst_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      req(fst_boot_results(), fst_boot_results()$formatted_results$pairwise_fst)
      write.csv(fst_boot_results()$formatted_results$pairwise_fst, file, row.names = TRUE)
    }
  )

  # ===== FONCTIONS COMPARATIVES =====

  observeEvent(input$run_comparison, {
    # Cette fonction comparerait FIT vs FST si les deux analyses sont disponibles
    if (is.null(fit_boot_results()) || is.null(fst_boot_results())) {
      showNotification("Please run both FIT and FST analyses first", type = "warning")
      return(NULL)
    }
    
    # Logique de comparaison à implémenter
    showNotification("Comparative analysis feature under development", type = "message")
  })

  # Value boxes comparatives
  output$comparison_correlation_box <- renderValueBox({
    # Calculer la corrélation entre FIT et FST si disponibles
    valueBox(
      value = "N/A",
      subtitle = HTML("<small>FIT-FST Correlation<br>Under development</small>"),
      color = "blue",
      icon = icon("chart-line")
    )
  })

  output$comparison_agreement_box <- renderValueBox({
    valueBox(
      value = "N/A", 
      subtitle = HTML("<small>Method Agreement<br>Under development</small>"),
      color = "green",
      icon = icon("check-circle")
    )
  })

  # ===== FONCTION DE SECOURS POUR BOOTSTRAP POP ONLY =====

  # Si la fonction C++ n'est toujours pas disponible, créons une version R de secours
  safe_boot_wc_pop_only_fast_cpp <- function(mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads) {
    if (exists("boot_wc_pop_only_fast_cpp")) {
      # Utiliser la fonction C++ si disponible
      return(boot_wc_pop_only_fast_cpp(
        mat_int, colnames, pop_col, missing_code, B, probs, seed, n_threads
      ))
    } else {
      # Fallback en R
      warning("Using R fallback for population-only bootstrap")
      return(boot_wc_pop_only_fallback_r(mat_int, pop_col, missing_code, B, probs))
    }
  }

  # Version R de secours pour le bootstrap population-only
  boot_wc_pop_only_fallback_r <- function(mat_int, pop_col, missing_code, B, probs) {
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col]
    L <- length(loci_names)
    
    # Stockage des résultats
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    for (b in 1:B) {
      # Rééchantillonnage des populations
      pop_ids <- unique(mat_int[, pop_col])
      boot_pops <- sample(pop_ids, length(pop_ids), replace = TRUE)
      
      # Construire l'échantillon bootstrap
      boot_sample <- do.call(rbind, lapply(boot_pops, function(pop) {
        pop_rows <- which(mat_int[, pop_col] == pop)
        if (length(pop_rows) > 0) {
          mat_int[sample(pop_rows, length(pop_rows), replace = TRUE), ]
        }
      }))
      
      if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
        # Calculer les statistiques WC sur l'échantillon bootstrap
        tryCatch({
          boot_df <- as.data.frame(boot_sample)
          boot_df[,1] <- as.factor(boot_df[,1])  # colonne population
          
          wc_results <- hierfstat::wc(boot_df)
          
          if (!is.null(wc_results$per.loc)) {
            boot_FST[b, ] <- wc_results$per.loc[, "FST"]
            boot_FIT[b, ] <- wc_results$per.loc[, "FIT"]
            boot_HI[b, ] <- wc_results$per.loc[, "Hs"]
            boot_HS[b, ] <- wc_results$per.loc[, "Ho"]
          }
        }, error = function(e) {
          # En cas d'erreur, laisser les valeurs NA
        })
      }
    }
    
    # Calcul des quantiles
    calculate_quantiles <- function(boot_matrix, probs) {
      apply(boot_matrix, 2, function(x) {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) > 0) {
          quantile(x_clean, probs = probs)
        } else {
          rep(NA, length(probs))
        }
      })
    }
    
    Q_FST <- calculate_quantiles(boot_FST, probs)
    Q_FIT <- calculate_quantiles(boot_FIT, probs)
    Q_HI <- calculate_quantiles(boot_HI, probs)
    Q_HS <- calculate_quantiles(boot_HS, probs)
    
    # Quantiles globaux
    Q_FST_overall <- quantile(as.vector(boot_FST), probs = probs, na.rm = TRUE)
    Q_FIT_overall <- quantile(as.vector(boot_FIT), probs = probs, na.rm = TRUE)
    Q_HI_overall <- quantile(as.vector(boot_HI), probs = probs, na.rm = TRUE)
    Q_HS_overall <- quantile(as.vector(boot_HS), probs = probs, na.rm = TRUE)
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # # Fonction pour formater les résultats FIT
  # format_fit_results <- function(fit_results) {
  #   if (is.null(fit_results)) return(NULL)
    
  #   tryCatch({
  #     loci_names <- fit_results$loci_names
  #     conf_level <- fit_results$conf_level
      
  #     # Tableau principal des résultats avec FST et FIS aussi
  #     result_table <- data.frame(
  #       Locus = loci_names,
  #       Observed_FIT = fit_results$observed_fit,
  #       Observed_FST = fit_results$observed_fst,
  #       Observed_FIS = fit_results$observed_fis,
  #       stringsAsFactors = FALSE
  #     )
      
  #     # Ajouter les résultats bootstrap FIT
  #     if (!is.null(fit_results$two_stage)) {
  #       if (!is.null(fit_results$two_stage$FIT) && ncol(fit_results$two_stage$FIT) > 0) {
  #         fit_quantiles <- as.data.frame(t(fit_results$two_stage$FIT))
  #         if (ncol(fit_quantiles) == 3) {
  #           colnames(fit_quantiles) <- c("TwoStage_CI_L", "TwoStage_Median", "TwoStage_CI_U")
  #           result_table <- cbind(result_table, fit_quantiles)
  #         }
  #       }
  #     }
      
  #     if (!is.null(fit_results$pop_only)) {
  #       if (!is.null(fit_results$pop_only$FIT) && ncol(fit_results$pop_only$FIT) > 0) {
  #         fit_quantiles <- as.data.frame(t(fit_results$pop_only$FIT))
  #         if (ncol(fit_quantiles) == 3) {
  #           colnames(fit_quantiles) <- c("PopOnly_CI_L", "PopOnly_Median", "PopOnly_CI_U")
  #           result_table <- cbind(result_table, fit_quantiles)
  #         }
  #       }
  #     }
      
  #     # Ligne globale
  #     overall_row <- data.frame(
  #       Locus = "Overall",
  #       Observed_FIT = fit_results$observed_fit_overall,
  #       Observed_FST = mean(fit_results$observed_fst, na.rm = TRUE),
  #       Observed_FIS = mean(fit_results$observed_fis, na.rm = TRUE),
  #       stringsAsFactors = FALSE
  #     )
      
  #     if (!is.null(fit_results$two_stage) && !is.null(fit_results$two_stage$FIT_overall)) {
  #       overall_row$TwoStage_CI_L <- fit_results$two_stage$FIT_overall[1]
  #       overall_row$TwoStage_Median <- fit_results$two_stage$FIT_overall[2]
  #       overall_row$TwoStage_CI_U <- fit_results$two_stage$FIT_overall[3]
  #     }
      
  #     if (!is.null(fit_results$pop_only) && !is.null(fit_results$pop_only$FIT_overall)) {
  #       overall_row$PopOnly_CI_L <- fit_results$pop_only$FIT_overall[1]
  #       overall_row$PopOnly_Median <- fit_results$pop_only$FIT_overall[2]
  #       overall_row$PopOnly_CI_U <- fit_results$pop_only$FIT_overall[3]
  #     }
      
  #     final_table <- rbind(result_table, overall_row)
      
  #     return(final_table)
      
  #   }, error = function(e) {
  #     stop("Error formatting FIT results: ", e$message)
  #   })
  # }

  # # Fonction pour formater les résultats FST
  # format_fst_results <- function(fst_results) {
  #   if (is.null(fst_results)) return(NULL)
    
  #   tryCatch({
  #     loci_names <- fst_results$loci_names
  #     conf_level <- fst_results$conf_level
      
  #     # Tableau principal des résultats
  #     result_table <- data.frame(
  #       Locus = loci_names,
  #       Observed_FST = fst_results$observed_fst,
  #       stringsAsFactors = FALSE
  #     )
      
  #     # Ajouter les résultats bootstrap selon la méthode
  #     if (!is.null(fst_results$two_stage)) {
  #       if (!is.null(fst_results$two_stage$FST) && ncol(fst_results$two_stage$FST) > 0) {
  #         fst_quantiles <- as.data.frame(t(fst_results$two_stage$FST))
  #         if (ncol(fst_quantiles) == 3) {
  #           colnames(fst_quantiles) <- c("TwoStage_CI_L", "TwoStage_Median", "TwoStage_CI_U")
  #           result_table <- cbind(result_table, fst_quantiles)
  #         }
  #       }
  #     }
      
  #     if (!is.null(fst_results$pop_only)) {
  #       if (!is.null(fst_results$pop_only$FST) && ncol(fst_results$pop_only$FST) > 0) {
  #         fst_quantiles <- as.data.frame(t(fst_results$pop_only$FST))
  #         if (ncol(fst_quantiles) == 3) {
  #           colnames(fst_quantiles) <- c("PopOnly_CI_L", "PopOnly_Median", "PopOnly_CI_U")
  #           result_table <- cbind(result_table, fst_quantiles)
  #         }
  #       }
  #     }
      
  #     # Ligne globale
  #     overall_row <- data.frame(
  #       Locus = "Overall",
  #       Observed_FST = fst_results$observed_fst_overall,
  #       stringsAsFactors = FALSE
  #     )
      
  #     if (!is.null(fst_results$two_stage) && !is.null(fst_results$two_stage$FST_overall)) {
  #       overall_row$TwoStage_CI_L <- fst_results$two_stage$FST_overall[1]
  #       overall_row$TwoStage_Median <- fst_results$two_stage$FST_overall[2]
  #       overall_row$TwoStage_CI_U <- fst_results$two_stage$FST_overall[3]
  #     }
      
  #     if (!is.null(fst_results$pop_only) && !is.null(fst_results$pop_only$FST_overall)) {
  #       overall_row$PopOnly_CI_L <- fst_results$pop_only$FST_overall[1]
  #       overall_row$PopOnly_Median <- fst_results$pop_only$FST_overall[2]
  #       overall_row$PopOnly_CI_U <- fst_results$pop_only$FST_overall[3]
  #     }
      
  #     final_table <- rbind(result_table, overall_row)
      
  #     return(list(
  #       locus_table = final_table,
  #       pairwise_fst = fst_results$pairwise_fst,
  #       pop_names = fst_results$pop_names
  #     ))
      
  #   }, error = function(e) {
  #     stop("Error formatting FST results: ", e$message)
  #   })
  # }

  # Fonction pour formater les résultats FIT - OVERALL EN DERNIÈRE LIGNE
  format_fit_results <- function(fit_results) {
    if (is.null(fit_results)) return(NULL)
    
    tryCatch({
      loci_names <- fit_results$loci_names
      conf_level <- fit_results$conf_level
      
      # Tableau principal des résultats avec FST et FIS aussi
      result_table <- data.frame(
        Locus = loci_names,
        Observed_FIT = fit_results$observed_fit,
        Observed_FST = fit_results$observed_fst,
        Observed_FIS = fit_results$observed_fis,
        stringsAsFactors = FALSE
      )
      
      # Ajouter les résultats bootstrap FIT
      if (!is.null(fit_results$two_stage)) {
        if (!is.null(fit_results$two_stage$FIT) && ncol(fit_results$two_stage$FIT) > 0) {
          fit_quantiles <- as.data.frame(t(fit_results$two_stage$FIT))
          if (ncol(fit_quantiles) == 3) {
            colnames(fit_quantiles) <- c("TwoStage_CI_L", "TwoStage_Median", "TwoStage_CI_U")
            result_table <- cbind(result_table, fit_quantiles)
          }
        }
      }
      
      if (!is.null(fit_results$pop_only)) {
        if (!is.null(fit_results$pop_only$FIT) && ncol(fit_results$pop_only$FIT) > 0) {
          fit_quantiles <- as.data.frame(t(fit_results$pop_only$FIT))
          if (ncol(fit_quantiles) == 3) {
            colnames(fit_quantiles) <- c("PopOnly_CI_L", "PopOnly_Median", "PopOnly_CI_U")
            result_table <- cbind(result_table, fit_quantiles)
          }
        }
      }
      
      # Ligne globale
      overall_row <- data.frame(
        Locus = "Overall",
        Observed_FIT = fit_results$observed_fit_overall,
        Observed_FST = mean(fit_results$observed_fst, na.rm = TRUE),
        Observed_FIS = mean(fit_results$observed_fis, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(fit_results$two_stage) && !is.null(fit_results$two_stage$FIT_overall)) {
        overall_row$TwoStage_CI_L <- fit_results$two_stage$FIT_overall[1]
        overall_row$TwoStage_Median <- fit_results$two_stage$FIT_overall[2]
        overall_row$TwoStage_CI_U <- fit_results$two_stage$FIT_overall[3]
      }
      
      if (!is.null(fit_results$pop_only) && !is.null(fit_results$pop_only$FIT_overall)) {
        overall_row$PopOnly_CI_L <- fit_results$pop_only$FIT_overall[1]
        overall_row$PopOnly_Median <- fit_results$pop_only$FIT_overall[2]
        overall_row$PopOnly_CI_U <- fit_results$pop_only$FIT_overall[3]
      }
      
      # Ajouter les colonnes manquantes à overall_row si nécessaire
      if (ncol(result_table) > ncol(overall_row)) {
        missing_cols <- setdiff(colnames(result_table), colnames(overall_row))
        for (col in missing_cols) {
          overall_row[[col]] <- NA
        }
      }
      
      # OVERALL EN DERNIÈRE LIGNE
      final_table <- rbind(result_table, overall_row)
      
      return(final_table)
      
    }, error = function(e) {
      stop("Error formatting FIT results: ", e$message)
    })
  }

  # Fonction pour formater les résultats FST - OVERALL EN DERNIÈRE LIGNE
  format_fst_results <- function(fst_results) {
    if (is.null(fst_results)) return(NULL)
    
    tryCatch({
      loci_names <- fst_results$loci_names
      conf_level <- fst_results$conf_level
      
      # Tableau principal des résultats
      result_table <- data.frame(
        Locus = loci_names,
        Observed_FST = fst_results$observed_fst,
        stringsAsFactors = FALSE
      )
      
      # Ajouter les résultats bootstrap selon la méthode
      if (!is.null(fst_results$two_stage)) {
        if (!is.null(fst_results$two_stage$FST) && ncol(fst_results$two_stage$FST) > 0) {
          fst_quantiles <- as.data.frame(t(fst_results$two_stage$FST))
          if (ncol(fst_quantiles) == 3) {
            colnames(fst_quantiles) <- c("TwoStage_CI_L", "TwoStage_Median", "TwoStage_CI_U")
            result_table <- cbind(result_table, fst_quantiles)
          }
        }
      }
      
      if (!is.null(fst_results$pop_only)) {
        if (!is.null(fst_results$pop_only$FST) && ncol(fst_results$pop_only$FST) > 0) {
          fst_quantiles <- as.data.frame(t(fst_results$pop_only$FST))
          if (ncol(fst_quantiles) == 3) {
            colnames(fst_quantiles) <- c("PopOnly_CI_L", "PopOnly_Median", "PopOnly_CI_U")
            result_table <- cbind(result_table, fst_quantiles)
          }
        }
      }
      
      # Ligne globale
      overall_row <- data.frame(
        Locus = "Overall",
        Observed_FST = fst_results$observed_fst_overall,
        stringsAsFactors = FALSE
      )
      
      if (!is.null(fst_results$two_stage) && !is.null(fst_results$two_stage$FST_overall)) {
        overall_row$TwoStage_CI_L <- fst_results$two_stage$FST_overall[1]
        overall_row$TwoStage_Median <- fst_results$two_stage$FST_overall[2]
        overall_row$TwoStage_CI_U <- fst_results$two_stage$FST_overall[3]
      }
      
      if (!is.null(fst_results$pop_only) && !is.null(fst_results$pop_only$FST_overall)) {
        overall_row$PopOnly_CI_L <- fst_results$pop_only$FST_overall[1]
        overall_row$PopOnly_Median <- fst_results$pop_only$FST_overall[2]
        overall_row$PopOnly_CI_U <- fst_results$pop_only$FST_overall[3]
      }
      
      # Ajouter les colonnes manquantes à overall_row si nécessaire
      if (ncol(result_table) > ncol(overall_row)) {
        missing_cols <- setdiff(colnames(result_table), colnames(overall_row))
        for (col in missing_cols) {
          overall_row[[col]] <- NA
        }
      }
      
      # OVERALL EN DERNIÈRE LIGNE
      final_table <- rbind(result_table, overall_row)
      
      return(list(
        locus_table = final_table,
        pairwise_fst = fst_results$pairwise_fst,
        pop_names = fst_results$pop_names
      ))
      
    }, error = function(e) {
      stop("Error formatting FST results: ", e$message)
    })
  }

  # ===== LES FONCTIONS FALLBACK R =====

  # Version R de secours pour le bootstrap two-stage
  boot_wc_two_stage_fallback_r <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting R two-stage bootstrap fallback with", B, "replicates\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col]
    L <- length(loci_names)
    
    # Obtenir les populations
    pop_ids <- unique(mat_int[, pop_col])
    R <- length(pop_ids)
    
    cat("  Populations:", R, "\n")
    cat("  Loci:", L, "\n")
    
    # Stockage des résultats - initialiser avec les bonnes dimensions
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    # Donner des noms aux colonnes
    colnames(boot_FST) <- colnames(boot_FIT) <- colnames(boot_HI) <- colnames(boot_HS) <- loci_names
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Étape 1: Rééchantillonnage des populations
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Étape 2: Rééchantillonnage des individus dans chaque population
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        pop_rows <- which(mat_int[, pop_col] == pop_id)
        if (length(pop_rows) > 0) {
          # Rééchantillonnage avec remise dans cette population
          boot_pop_rows <- sample(pop_rows, length(pop_rows), replace = TRUE)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[boot_pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            boot_df[,1] <- as.factor(boot_df[,1])  # colonne population
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                # vérifier que boot_FST[b, ] n'est pas NA avant de calculer FIT
                if (all(!is.na(boot_FST[b, ]))) {
                  boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
                }
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Vérifier qu'on a des données avant de calculer les quantiles
    calculate_quantiles <- function(boot_matrix, probs, loci_names) {
      # Vérifier si la matrice a des données
      if (all(is.na(boot_matrix))) {
        warning("All bootstrap values are NA for this statistic")
        result <- matrix(NA, nrow = length(probs), ncol = length(loci_names))
      } else {
        result <- apply(boot_matrix, 2, function(x) {
          x_clean <- x[!is.na(x)]
          if (length(x_clean) > 0) {
            quantile(x_clean, probs = probs, na.rm = TRUE)
          } else {
            rep(NA, length(probs))
          }
        })
      }
      
      # S'assurer que c'est une matrice avec des dimensions
      if (!is.matrix(result)) {
        result <- matrix(result, nrow = length(probs), ncol = length(loci_names))
      }
      
      # Définir les noms de ligne et colonne
      rownames(result) <- as.character(probs)
      colnames(result) <- loci_names
      
      return(result)
    }
    
    # Calcul des quantiles avec vérification
    Q_FST <- calculate_quantiles(boot_FST, probs, loci_names)
    Q_FIT <- calculate_quantiles(boot_FIT, probs, loci_names)
    Q_HI <- calculate_quantiles(boot_HI, probs, loci_names)
    Q_HS <- calculate_quantiles(boot_HS, probs, loci_names)
    
    # Quantiles globaux avec vérification
    safe_quantile <- function(x, probs) {
      x_clean <- as.vector(x)[!is.na(as.vector(x))]
      if (length(x_clean) > 0) {
        result <- quantile(x_clean, probs = probs, na.rm = TRUE)
        names(result) <- as.character(probs)
        return(result)
      } else {
        result <- rep(NA, length(probs))
        names(result) <- as.character(probs)
        return(result)
      }
    }
    
    Q_FST_overall <- safe_quantile(boot_FST, probs)
    Q_FIT_overall <- safe_quantile(boot_FIT, probs)
    Q_HI_overall <- safe_quantile(boot_HI, probs)
    Q_HS_overall <- safe_quantile(boot_HS, probs)
    
    cat("R two-stage bootstrap completed successfully\n")
    cat("Result dimensions - FST:", dim(Q_FST), "FIT:", dim(Q_FIT), "\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

  # Version R de secours pour le bootstrap population-only
  boot_wc_pop_only_fallback_r <- function(mat_int, pop_col, missing_code, B, probs) {
    cat("Starting R population-only bootstrap fallback with", B, "replicates\n")
    
    n <- nrow(mat_int)
    p <- ncol(mat_int)
    loci_names <- colnames(mat_int)[-pop_col]
    L <- length(loci_names)
    
    # Obtenir les populations
    pop_ids <- unique(mat_int[, pop_col])
    R <- length(pop_ids)
    
    cat("  Populations:", R, "\n")
    cat("  Loci:", L, "\n")
    
    # Stockage des résultats - initialiser avec les bonnes dimensions
    boot_FST <- matrix(NA, nrow = B, ncol = L)
    boot_FIT <- matrix(NA, nrow = B, ncol = L)
    boot_HI <- matrix(NA, nrow = B, ncol = L)
    boot_HS <- matrix(NA, nrow = B, ncol = L)
    
    # Donner des noms aux colonnes
    colnames(boot_FST) <- colnames(boot_FIT) <- colnames(boot_HI) <- colnames(boot_HS) <- loci_names
    
    for (b in 1:B) {
      if (b %% 10 == 0) cat("  Bootstrap iteration", b, "/", B, "\n")
      
      # Rééchantillonnage des populations seulement (sans rééchantillonnage des individus)
      boot_pop_ids <- sample(pop_ids, R, replace = TRUE)
      
      # Construire l'échantillon bootstrap en prenant toutes les données de chaque population échantillonnée
      boot_sample_list <- list()
      
      for (pop_id in boot_pop_ids) {
        pop_rows <- which(mat_int[, pop_col] == pop_id)
        if (length(pop_rows) > 0) {
          # Prendre tous les individus de cette population (sans rééchantillonnage)
          boot_sample_list[[length(boot_sample_list) + 1]] <- mat_int[pop_rows, ]
        }
      }
      
      if (length(boot_sample_list) > 0) {
        boot_sample <- do.call(rbind, boot_sample_list)
        
        if (!is.null(boot_sample) && nrow(boot_sample) > 0) {
          # Calculer les statistiques WC sur l'échantillon bootstrap
          tryCatch({
            boot_df <- as.data.frame(boot_sample)
            boot_df[,1] <- as.factor(boot_df[,1])  # colonne population
            
            wc_results <- hierfstat::wc(boot_df)
            
            if (!is.null(wc_results$per.loc)) {
              # Stocker FST
              if ("FST" %in% colnames(wc_results$per.loc)) {
                boot_FST[b, ] <- wc_results$per.loc[, "FST"]
              }
              
              # Stocker FIS et calculer FIT
              if ("FIS" %in% colnames(wc_results$per.loc)) {
                boot_fis <- wc_results$per.loc[, "FIS"]
                # vérifier que boot_FST[b, ] n'est pas NA avant de calculer FIT
                if (all(!is.na(boot_FST[b, ]))) {
                  boot_FIT[b, ] <- calculate_fit_from_fst_fis(boot_FST[b, ], boot_fis)
                }
              }
              
              # Stocker Hs et Ho
              if ("Hs" %in% colnames(wc_results$per.loc)) {
                boot_HS[b, ] <- wc_results$per.loc[, "Hs"]
              }
              if ("Ho" %in% colnames(wc_results$per.loc)) {
                boot_HI[b, ] <- wc_results$per.loc[, "Ho"]
              }
            }
          }, error = function(e) {
            # En cas d'erreur, laisser les valeurs NA
            if (b %% 10 == 0) cat("    Iteration", b, "failed:", e$message, "\n")
          })
        }
      }
    }
    
    # Vérifier qu'on a des données avant de calculer les quantiles
    calculate_quantiles <- function(boot_matrix, probs, loci_names) {
      # Vérifier si la matrice a des données
      if (all(is.na(boot_matrix))) {
        warning("All bootstrap values are NA for this statistic")
        result <- matrix(NA, nrow = length(probs), ncol = length(loci_names))
      } else {
        result <- apply(boot_matrix, 2, function(x) {
          x_clean <- x[!is.na(x)]
          if (length(x_clean) > 0) {
            quantile(x_clean, probs = probs, na.rm = TRUE)
          } else {
            rep(NA, length(probs))
          }
        })
      }
      
      # S'assurer que c'est une matrice avec des dimensions
      if (!is.matrix(result)) {
        result <- matrix(result, nrow = length(probs), ncol = length(loci_names))
      }
      
      # Définir les noms de ligne et colonne
      rownames(result) <- as.character(probs)
      colnames(result) <- loci_names
      
      return(result)
    }
    
    # Calcul des quantiles avec vérification
    Q_FST <- calculate_quantiles(boot_FST, probs, loci_names)
    Q_FIT <- calculate_quantiles(boot_FIT, probs, loci_names)
    Q_HI <- calculate_quantiles(boot_HI, probs, loci_names)
    Q_HS <- calculate_quantiles(boot_HS, probs, loci_names)
    
    # Quantiles globaux avec vérification
    safe_quantile <- function(x, probs) {
      x_clean <- as.vector(x)[!is.na(as.vector(x))]
      if (length(x_clean) > 0) {
        result <- quantile(x_clean, probs = probs, na.rm = TRUE)
        names(result) <- as.character(probs)
        return(result)
      } else {
        result <- rep(NA, length(probs))
        names(result) <- as.character(probs)
        return(result)
      }
    }
    
    Q_FST_overall <- safe_quantile(boot_FST, probs)
    Q_FIT_overall <- safe_quantile(boot_FIT, probs)
    Q_HI_overall <- safe_quantile(boot_HI, probs)
    Q_HS_overall <- safe_quantile(boot_HS, probs)
    
    cat("R population-only bootstrap completed successfully\n")
    cat("Result dimensions - FST:", dim(Q_FST), "FIT:", dim(Q_FIT), "\n")
    
    return(list(
      FST = Q_FST,
      FIT = Q_FIT,
      HI = Q_HI,
      HS = Q_HS,
      FST_overall = Q_FST_overall,
      FIT_overall = Q_FIT_overall,
      HI_overall = Q_HI_overall,
      HS_overall = Q_HS_overall
    ))
  }

}