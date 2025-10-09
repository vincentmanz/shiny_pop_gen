# simulate_compare_bootstrap_optimized.R
  
suppressPackageStartupMessages({
  library(Rcpp)
  library(data.table)  # Plus rapide que dplyr/tidyr
  library(ggplot2)
})

# ============================================================================
# CONFIGURATION ET CHARGEMENT OPTIMISÉ
# ============================================================================

# Chargement unique des fonctions C++ avec gestion d'erreur
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
    }
  }
}

.load_cpp_functions()

# Configuration pour la reproductibilité et performance
Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")

# ============================================================================
# FONCTIONS DE SIMULATION OPTIMISÉES
# ============================================================================

# Allocation d'échantillons optimisée avec data.table
split_N_dirichlet_fast <- function(N_total, K, min_n = 10L, strict_min = TRUE) {
  if (strict_min && N_total < min_n * K) return(NULL)
  
  eff_min <- max(1L, min(min_n, floor(N_total / K)))
  base <- rep(eff_min, K)
  rem <- N_total - sum(base)
  
  if (rem <= 0L) return(base)
  
  # Génération de poids plus efficace
  w <- rgamma(K, shape = 1)
  w <- w / sum(w)
  add <- as.integer(round(w * rem))
  
  # Ajustement de la différence
  diff <- rem - sum(add)
  if (diff != 0L) {
    ord <- order(w, decreasing = (diff > 0))
    for (i in seq_len(abs(diff))) {
      j <- ord[((i - 1) %% K) + 1L]
      add[j] <- add[j] + sign(diff)
    }
  }
  
  base + add
}

# Simulation Balding-Nichols optimisée
simulate_BN_fixedN_optimized <- function(N_total, K, L, Fst_mean, Fis,
                                        pbar_alpha = 1, pbar_beta = 1,
                                        min_n_pop = 10L, strict_min = TRUE) {
  
  n_per_pop <- split_N_dirichlet_fast(N_total, K, min_n_pop, strict_min)
  if (is.null(n_per_pop)) return(NULL)
  
  # Pré-calculs efficaces
  N <- sum(n_per_pop)
  pop_labels <- rep(seq_len(K), times = n_per_pop)
  pbar <- rbeta(L, pbar_alpha, pbar_beta)
  Fis_k <- rep(Fis, length.out = K)
  
  # Initialisation de la matrice de génotypes
  geno <- matrix(0L, nrow = N, ncol = L)  # 0 = missing par défaut
  
  # Structures pour les vérités
  HS_true <- numeric(L)
  HI_true <- numeric(L)
  FST_true <- numeric(L)
  FIT_true <- numeric(L)
  
  # Fonction de codage optimisée
  code6_fast <- function(g) {
    ifelse(g == 2L, 101101L, ifelse(g == 1L, 101102L, 102102L))
  }
  
  # Simulation par locus
  for (ell in seq_len(L)) {
    Fst_l <- max(min(Fst_mean, 0.9), 1e-6)
    tau <- (1 - Fst_l) / Fst_l
    
    # Génération des fréquences alléliques
    if (is.finite(tau) && tau < 1e9) {
      p_k <- rbeta(K, pbar[ell] * tau, (1 - pbar[ell]) * tau)
    } else {
      p_k <- rep(pbar[ell], K)
    }
    
    # Calcul des statistiques vraies
    HS_true[ell] <- mean(2 * p_k * (1 - p_k))
    HI_true[ell] <- mean((1 - Fis_k) * 2 * p_k * (1 - p_k))
    HT_true <- 2 * mean(p_k) * (1 - mean(p_k))
    
    FST_true[ell] <- if (HT_true > 1e-12) 1 - HS_true[ell] / HT_true else NA_real_
    FIT_true[ell] <- if (HT_true > 1e-12) 1 - HI_true[ell] / HT_true else NA_real_
    
    # Simulation des génotypes par population
    start_idx <- 1L
    for (k in seq_len(K)) {
      n_k <- n_per_pop[k]
      if (n_k > 0L) {
        idx <- start_idx:(start_idx + n_k - 1L)
        pk <- p_k[k]
        f <- min(max(Fis_k[k], 0), 0.999)
        
        # Probabilités des génotypes
        P_AA <- pk^2 + f * pk * (1 - pk)
        P_Aa <- 2 * pk * (1 - pk) * (1 - f)
        
        # Échantillonnage efficace
        g <- sample.int(3L, n_k, replace = TRUE, 
                       prob = c(1 - P_AA - P_Aa, P_Aa, P_AA)) - 1L
        geno[idx, ell] <- code6_fast(g)
        
        start_idx <- start_idx + n_k
      }
    }
  }
  
  # Construction du data.frame final
  hf <- data.frame(pop = as.integer(pop_labels), geno)
  colnames(hf) <- c("pop", paste0("L", seq_len(L)))
  
  truth <- data.frame(
    Locus = paste0("L", seq_len(L)),
    HI_true = HI_true,
    HS_true = HS_true,
    FST_true = FST_true,
    FIT_true = FIT_true,
    stringsAsFactors = FALSE
  )
  
  list(hf = hf, truth = truth)
}

# ============================================================================
# WRAPPERS C++ OPTIMISÉS
# ============================================================================

# Conversion en integer optimisée
.ensure_int_fast <- function(hf) {
  if (!data.table::is.data.table(hf)) {
    hf_dt <- data.table::as.data.table(hf)
  } else {
    hf_dt <- hf
  }
  
  if (names(hf_dt)[1] != "pop") {
    data.table::setnames(hf_dt, 1, "pop")
  }
  
  hf_dt[, pop := as.integer(as.factor(pop))]
  
  # Conversion des colonnes de loci
  locus_cols <- setdiff(names(hf_dt), "pop")
  for (col in locus_cols) {
    if (!is.integer(hf_dt[[col]])) {
      hf_dt[, (col) := as.integer(get(col))]
    }
  }
  
  hf_dt
}

# Formatage des intervalles de confiance optimisé
tidy_ci_fast <- function(QxL, loci, stat, probs) {
  if (is.null(dim(QxL)) {
    QxL <- matrix(QxL, nrow = length(probs), ncol = length(loci))
  }
  
  # Trouver l'index de la médiane
  med_idx <- which.min(abs(as.numeric(rownames(QxL)) - 0.5))
  if (length(med_idx) == 0) med_idx <- ceiling(nrow(QxL) / 2)
  
  data.table::data.table(
    Stat = stat,
    Locus = rep(loci, each = nrow(QxL)),
    Quantile = rep(rownames(QxL), times = length(loci)),
    Value = as.vector(QxL)
  ) %>% 
    data.table::dcast(Locus + Stat ~ Quantile, value.var = "Value") %>%
    {
      q_names <- colnames(.)[-(1:2)]
      data.table::data.table(
        Stat = .$Stat,
        Locus = .$Locus,
        LCL = .[[q_names[1]]],
        Median = .[[q_names[ceiling(length(q_names)/2)]]],
        UCL = .[[q_names[length(q_names)]]]
      )
    }
}

# ============================================================================
# BENCHMARK OPTIMISÉ
# ============================================================================

run_benchmark_optimized <- function(
    N_total_vec = c(100, 500, 2000),
    L_vec = c(5, 20, 100),
    K_vec = c(3, 10),
    Fst_vec = c(0.005, 0.02, 0.05),
    Fis_vec = c(0.00, 0.05, 0.10),
    n_reps = 5,
    n_boot = 400,
    probs = c(0.025, 0.5, 0.975),
    seed = 7,
    min_n_pop = 10L,
    strict_min = TRUE,
    n_threads = max(1L, parallel::detectCores() - 1L)
) {
  set.seed(seed)
  
  # Grille de scénarios optimisée
  scenarios <- data.table::CJ(
    N_total = N_total_vec,
    L = L_vec,
    K = K_vec,
    FST = Fst_vec,
    FIS = Fis_vec
  )
  
  scenarios[, cost := N_total * L * K]
  data.table::setorder(scenarios, cost)
  
  results <- vector("list", nrow(scenarios) * n_reps)
  result_index <- 1L
  
  cat(sprintf("Total scenarios: %d, Replicates: %d\n", nrow(scenarios), n_reps))
  
  for (s in seq_len(nrow(scenarios))) {
    pars <- scenarios[s, ]
    cat(sprintf("Scenario %d/%d: N=%d, L=%d, K=%d, FST=%.3f, FIS=%.3f\n",
                s, nrow(scenarios), pars$N_total, pars$L, pars$K, pars$FST, pars$FIS))
    
    for (rep in seq_len(n_reps)) {
      # Simulation
      sim <- simulate_BN_fixedN_optimized(
        N_total = pars$N_total,
        K = pars$K,
        L = pars$L,
        Fst_mean = pars$FST,
        Fis = pars$FIS,
        min_n_pop = min_n_pop,
        strict_min = strict_min
      )
      
      if (is.null(sim)) next
      
      # Préparation des données
      hf_dt <- .ensure_int_fast(sim$hf)
      mat <- as.matrix(hf_dt)
      storage.mode(mat) <- "integer"
      pop_col <- 0L
      loci_names <- setdiff(names(hf_dt), "pop")
      
      # Bootstrap deux niveaux
      res_two <- boot_wc_two_stage_fast_cpp(
        mat_int = mat,
        colnames = names(hf_dt),
        pop_col = pop_col,
        missing_code = 0L,
        B = as.integer(n_boot),
        probs = probs,
        seed = as.integer(seed + 31L * rep + s),
        n_threads = as.integer(n_threads)
      )
      
      # Bootstrap par populations seulement
      res_pop <- boot_wc_pop_only_fast_cpp(
        mat_int = mat,
        colnames = names(hf_dt),
        pop_col = pop_col,
        missing_code = 0L,
        B = as.integer(n_boot),
        probs = probs,
        seed = as.integer(seed + 53L * rep + s),
        n_threads = as.integer(n_threads)
      )
      
      # Agrégation des résultats
      ci_two <- rbind(
        tidy_ci_fast(res_two$FST, loci_names, "FST", probs),
        tidy_ci_fast(res_two$FIT, loci_names, "FIT", probs),
        tidy_ci_fast(res_two$HI, loci_names, "HI", probs),
        tidy_ci_fast(res_two$HS, loci_names, "HS", probs)
      )
      
      ci_pop <- rbind(
        tidy_ci_fast(res_pop$FST, loci_names, "FST", probs),
        tidy_ci_fast(res_pop$FIT, loci_names, "FIT", probs),
        tidy_ci_fast(res_pop$HI, loci_names, "HI", probs),
        tidy_ci_fast(res_pop$HS, loci_names, "HS", probs)
      )
      
      # Fusion avec les vérités
      truth_long <- data.table::data.table(
        Locus = rep(sim$truth$Locus, 4),
        Stat = rep(c("FST", "FIT", "HI", "HS"), each = nrow(sim$truth)),
        True = c(sim$truth$FST_true, sim$truth$FIT_true, 
                sim$truth$HI_true, sim$truth$HS_true)
      )
      
      ci_two <- merge(ci_two, truth_long, by = c("Locus", "Stat"), all.x = TRUE)
      ci_pop <- merge(ci_pop, truth_long, by = c("Locus", "Stat"), all.x = TRUE)
      
      # Calcul des métriques
      calculate_metrics <- function(df, method) {
        df[, `:=`(
          cover = as.numeric(!is.na(True) & True >= LCL & True <= UCL),
          width = UCL - LCL,
          bias = Median - True,
          abs_err = abs(Median - True),
          Method = method
        )]
        df
      }
      
      ci_two <- calculate_metrics(ci_two, "two-stage")
      ci_pop <- calculate_metrics(ci_pop, "pop-only")
      
      # Combinaison finale
      result <- rbind(ci_two, ci_pop, fill = TRUE)
      result[, `:=`(
        N_total = pars$N_total,
        L = pars$L,
        K = pars$K,
        FST_scn = pars$FST,
        FIS_scn = pars$FIS,
        rep = rep
      )]
      
      results[[result_index]] <- result
      result_index <- result_index + 1L
    }
  }
  
  # Nettoyage et retour final
  results <- results[!sapply(results, is.null)]
  if (length(results) > 0) {
    data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  } else {
    data.table::data.table()
  }
}

# ============================================================================
# MÉTRIQUES ET VISUALISATIONS OPTIMISÉES
# ============================================================================

# Score de Winkler optimisé
winkler_score_fast <- function(L, U, y, alpha = 0.05) {
  width <- U - L
  penalty <- (2/alpha) * (pmax(0, L - y) + pmax(0, y - U))
  width + penalty
}

# Agrégation des scénarios optimisée
summarise_scenarios_fast <- function(res_aug) {
  res_aug[, .(
    coverage = mean(cover, na.rm = TRUE),
    mean_width = mean(width, na.rm = TRUE),
    median_width = median(width, na.rm = TRUE),
    mean_winkler = mean(winkler, na.rm = TRUE),
    prop_neg_LCL = mean(neg_LCL, na.rm = TRUE),
    n_loci = .N
  ), by = .(N_total, L, K, FST_scn, FIS_scn, rep, Method, Stat)]
}

# Comparaison des méthodes optimisée
compare_methods_fast <- function(scen_sum) {
  wide <- data.table::dcast(
    scen_sum,
    N_total + L + K + FST_scn + FIS_scn + rep + Stat ~ Method,
    value.var = c("coverage", "mean_width", "median_width", 
                  "mean_winkler", "prop_neg_LCL", "n_loci")
  )
  
  wide[, `:=`(
    d_coverage = coverage_two_stage - coverage_pop_only,
    d_mean_width = mean_width_two_stage - mean_width_pop_only,
    d_med_width = median_width_two_stage - median_width_pop_only,
    d_winkler = mean_winkler_two_stage - mean_winkler_pop_only,
    width_ratio = mean_width_two_stage / mean_width_pop_only,
    d_prop_negLCL = prop_neg_LCL_two_stage - prop_neg_LCL_pop_only
  )]
  
  wide
}

# ============================================================================
# PIPELINE PRINCIPAL OPTIMISÉ
# ============================================================================

# Exemple d'utilisation
run_complete_analysis <- function() {
  # Exécution du benchmark
  res <- run_benchmark_optimized(
    N_total_vec = c(100, 500, 1000, 2000),
    L_vec = c(5, 20, 50),
    K_vec = c(5, 10),
    Fst_vec = c(0.01, 0.05, 0.10),
    Fis_vec = c(0.00, 0.05),
    n_reps = 10,
    n_boot = 1000,
    n_threads = max(1L, parallel::detectCores() - 1L)
  )
  
  if (nrow(res) == 0) {
    message("Aucun scénario valide n'a pu être simulé")
    return(NULL)
  }
  
  # Calcul des métriques avancées
  res[, winkler := winkler_score_fast(LCL, UCL, True, 0.05)]
  res[, neg_LCL := as.numeric(LCL < 0)]
  
  # Agrégation et comparaison
  scen_sum <- summarise_scenarios_fast(res)
  cmp_scen <- compare_methods_fast(scen_sum)
  
  # Vue d'ensemble
  overview <- cmp_scen[, .(
    scenarios = .N,
    mean_d_coverage = mean(d_coverage, na.rm = TRUE),
    median_width_ratio = median(width_ratio, na.rm = TRUE),
    mean_d_winkler = mean(d_winkler, na.rm = TRUE),
    mean_d_prop_negLCL = mean(d_prop_negLCL, na.rm = TRUE)
  ), by = Stat]
  
  print(overview)
  
  ###

  # ============================================================================
  # VISUALISATIONS OPTIMISÉES
  # ============================================================================

  # Fonction helper pour l'agrégation des plots
  .aggregate_for_plots_fast <- function(res) {
    if (data.table::is.data.table(res)) {
      res[, .(
        coverage = mean(cover, na.rm = TRUE),
        mean_width = mean(width, na.rm = TRUE),
        median_width = median(width, na.rm = TRUE),
        mean_bias = mean(bias, na.rm = TRUE),
        mean_abs_err = mean(abs_err, na.rm = TRUE),
        n_obs = .N
      ), by = .(Method, Stat, N_total, L, K, FST_scn, FIS_scn)]
    } else {
      res %>%
        group_by(Method, Stat, N_total, L, K, FST_scn, FIS_scn) %>%
        summarise(
          coverage = mean(cover, na.rm = TRUE),
          mean_width = mean(width, na.rm = TRUE),
          median_width = median(width, na.rm = TRUE),
          mean_bias = mean(bias, na.rm = TRUE),
          mean_abs_err = mean(abs_err, na.rm = TRUE),
          n_obs = n(),
          .groups = "drop"
        )
    }
  }

  # 1. Graphique de couverture vs N_total
  plot_coverage_vs_N_optimized <- function(res, stat = "FST", facet_by = c("L", "FST_scn")) {
    df <- .aggregate_for_plots_fast(res) 
    
    if (data.table::is.data.table(df)) {
      df <- df[Stat == stat]
    } else {
      df <- df %>% filter(Stat == stat)
    }
    
    facet_by <- intersect(facet_by, colnames(df))
    
    p <- ggplot(df, aes(x = N_total, y = coverage, color = Method, group = Method)) +
      geom_hline(yintercept = 0.95, linetype = 2, color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, alpha = 0.8) + 
      geom_point(size = 2, alpha = 0.9) +
      scale_x_log10(
        breaks = c(20, 50, 100, 200, 500, 1000, 2000),
        labels = scales::comma
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2),
        labels = scales::percent_format(accuracy = 1)
      ) +
      scale_color_brewer(palette = "Set1", name = "Méthode") +
      labs(
        title = sprintf("Couverture vs N_total (%s)", stat),
        subtitle = "Ligne pointillée = couverture nominale 95%",
        x = "Nombre total d'individus (échelle log)",
        y = "Taux de couverture"
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10)
      )
    
    if (length(facet_by) > 0) {
      if (length(facet_by) == 1) {
        p <- p + facet_wrap(facet_by, labeller = label_both)
      } else {
        p <- p + facet_grid(reformulate(facet_by[1], facet_by[2]), labeller = label_both)
      }
    }
    
    return(p)
  }

  # 2. Graphique de largeur d'IC vs N_total
  plot_width_vs_N_optimized <- function(res, stat = "FST", facet_by = c("L", "FST_scn")) {
    df <- .aggregate_for_plots_fast(res)
    
    if (data.table::is.data.table(df)) {
      df <- df[Stat == stat]
    } else {
      df <- df %>% filter(Stat == stat)
    }
    
    facet_by <- intersect(facet_by, colnames(df))
    
    p <- ggplot(df, aes(x = N_total, y = median_width, color = Method, group = Method)) +
      geom_line(linewidth = 0.8, alpha = 0.8) + 
      geom_point(size = 2, alpha = 0.9) +
      scale_x_log10(
        breaks = c(20, 50, 100, 200, 500, 1000, 2000),
        labels = scales::comma
      ) +
      scale_color_brewer(palette = "Set1", name = "Méthode") +
      labs(
        title = sprintf("Largeur médiane des IC vs N_total (%s)", stat),
        x = "Nombre total d'individus (échelle log)",
        y = "Largeur médiane des IC"
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14)
      )
    
    if (length(facet_by) > 0) {
      if (length(facet_by) == 1) {
        p <- p + facet_wrap(facet_by, labeller = label_both)
      } else {
        p <- p + facet_grid(reformulate(facet_by[1], facet_by[2]), labeller = label_both)
      }
    }
    
    return(p)
  }

  # 3. Graphique de biais vs N_total
  plot_bias_vs_N_optimized <- function(res, stat = "FST", facet_by = c("L", "FST_scn")) {
    df <- .aggregate_for_plots_fast(res)
    
    if (data.table::is.data.table(df)) {
      df <- df[Stat == stat]
    } else {
      df <- df %>% filter(Stat == stat)
    }
    
    facet_by <- intersect(facet_by, colnames(df))
    
    p <- ggplot(df, aes(x = N_total, y = mean_bias, color = Method, group = Method)) +
      geom_hline(yintercept = 0, linetype = 2, color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, alpha = 0.8) + 
      geom_point(size = 2, alpha = 0.9) +
      scale_x_log10(
        breaks = c(20, 50, 100, 200, 500, 1000, 2000),
        labels = scales::comma
      ) +
      scale_color_brewer(palette = "Set1", name = "Méthode") +
      labs(
        title = sprintf("Biais moyen vs N_total (%s)", stat),
        subtitle = "Ligne pointillée = biais nul",
        x = "Nombre total d'individus (échelle log)",
        y = "Biais moyen (médiane bootstrap - vérité)"
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10)
      )
    
    if (length(facet_by) > 0) {
      if (length(facet_by) == 1) {
        p <- p + facet_wrap(facet_by, labeller = label_both)
      } else {
        p <- p + facet_grid(reformulate(facet_by[1], facet_by[2]), labeller = label_both)
      }
    }
    
    return(p)
  }

  # 4. Heatmap de différence de couverture
  plot_coverage_heatmap_optimized <- function(cmp_scen, stat = "FST") {
    
    if (data.table::is.data.table(cmp_scen)) {
      plot_data <- cmp_scen[Stat == stat]
    } else {
      plot_data <- cmp_scen %>% filter(Stat == stat)
    }
    
    ggplot(plot_data, aes(x = as.factor(N_total), y = as.factor(L), fill = d_coverage)) +
      geom_tile(color = "white", linewidth = 0.3) +
      facet_grid(FST_scn ~ K, labeller = label_both) +
      scale_fill_gradient2(
        low = "#D73027",
        mid = "white",
        high = "#4575B4",
        midpoint = 0,
        name = "Δ couverture\n(deux-niveaux - pop-only)",
        limits = c(-0.2, 0.2),
        oob = scales::squish
      ) +
      labs(
        title = sprintf("Différence de couverture (%s)", stat),
        subtitle = "Positive = deux-niveaux meilleur, Negative = pop-only meilleur",
        x = "Nombre total d'individus (N)",
        y = "Nombre de loci (L)"
      ) +
      theme_bw(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 9)
      )
  }

  # 5. Boxplot du ratio de largeur d'IC
  plot_width_ratio_boxplot_optimized <- function(cmp_scen, stat = "FST") {
    
    if (data.table::is.data.table(cmp_scen)) {
      plot_data <- cmp_scen[Stat == stat]
    } else {
      plot_data <- cmp_scen %>% filter(Stat == stat)
    }
    
    ggplot(plot_data, aes(x = interaction(FST_scn, K, sep = ", K="), y = width_ratio)) +
      geom_hline(yintercept = 1, linetype = 2, color = "red", linewidth = 0.6) +
      geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.size = 0.8) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred") +
      labs(
        title = sprintf("Ratio de largeur d'IC (deux-niveaux / pop-only) - %s", stat),
        subtitle = "Ligne rouge = ratio 1, Point rouge = moyenne",
        x = "Scénario FST, K populations",
        y = "Ratio de largeur d'IC"
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10)
      )
  }

  # 6. Heatmap du score de Winkler
  plot_winkler_heatmap_optimized <- function(cmp_scen, stat = "FST") {
    
    if (data.table::is.data.table(cmp_scen)) {
      plot_data <- cmp_scen[Stat == stat]
    } else {
      plot_data <- cmp_scen %>% filter(Stat == stat)
    }
    
    ggplot(plot_data, aes(x = as.factor(N_total), y = as.factor(L), fill = d_winkler)) +
      geom_tile(color = "white", linewidth = 0.3) +
      facet_grid(FST_scn ~ K, labeller = label_both) +
      scale_fill_gradient2(
        low = "#4575B4",
        mid = "white", 
        high = "#D73027",
        midpoint = 0,
        name = "Δ score Winkler\n(deux-niveaux - pop-only)",
        limits = c(-0.5, 0.5),
        oob = scales::squish
      ) +
      labs(
        title = sprintf("Différence de score Winkler (%s)", stat),
        subtitle = "Négatif = deux-niveaux meilleur, Positif = pop-only meilleur\n(Plus bas = meilleur)",
        x = "Nombre total d'individus (N)",
        y = "Nombre de loci (L)"
      ) +
      theme_bw(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 9)
      )
  }

  # 7. Graphique de performance multi-métriques
  plot_performance_radar_optimized <- function(cmp_scen, stat = "FST") {
    
    if (data.table::is.data.table(cmp_scen)) {
      plot_data <- cmp_scen[Stat == stat]
    } else {
      plot_data <- cmp_scen %>% filter(Stat == stat)
    }
    
    # Préparation des données pour le radar chart
    radar_data <- plot_data %>%
      group_by(Method) %>%
      summarise(
        Couverture = mean(d_coverage, na.rm = TRUE),
        Largeur_IC = mean(d_med_width, na.rm = TRUE),
        Score_Winkler = mean(d_winkler, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = -Method,
        names_to = "Metric",
        values_to = "Value"
      )
    
    ggplot(radar_data, aes(x = Metric, y = Value, color = Method, group = Method)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = 2, color = "gray50") +
      coord_polar() +
      scale_color_brewer(palette = "Set1", name = "Méthode") +
      labs(
        title = sprintf("Profil de performance multi-métriques (%s)", stat),
        subtitle = "Comparaison des méthodes bootstrap",
        x = "",
        y = "Valeur standardisée"
      ) +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        plot.title = element_text(face = "bold", size = 14)
      )
  }

  # ============================================================================
  # FONCTION DE GÉNÉRATION DE RAPPORT COMPLET
  # ============================================================================

  generate_comprehensive_report <- function(analysis_results, output_dir = "bootstrap_report") {
    
    # Création du dossier de sortie
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Génération de tous les graphiques
    plots <- list()
    
    # Graphiques principaux
    plots$coverage_vs_N <- plot_coverage_vs_N_optimized(analysis_results$raw_results, "FST")
    plots$width_vs_N <- plot_width_vs_N_optimized(analysis_results$raw_results, "FST") 
    plots$bias_vs_N <- plot_bias_vs_N_optimized(analysis_results$raw_results, "FST")
    
    # Graphiques de comparaison
    plots$coverage_heatmap <- plot_coverage_heatmap_optimized(analysis_results$comparison, "FST")
    plots$width_ratio <- plot_width_ratio_boxplot_optimized(analysis_results$comparison, "FST")
    plots$winkler_heatmap <- plot_winkler_heatmap_optimized(analysis_results$comparison, "FST")
    
    # Sauvegarde des graphiques
    for (plot_name in names(plots)) {
      filename <- file.path(output_dir, paste0(plot_name, ".png"))
      ggsave(filename, plots[[plot_name]], width = 10, height = 7, dpi = 300)
    }
    
    # Sauvegarde des données
    write.csv(analysis_results$overview, 
              file.path(output_dir, "method_comparison_overview.csv"), 
              row.names = FALSE)
    
    message("Rapport généré dans le dossier: ", output_dir)
    return(plots)
  }

  # ============================================================================
  # EXEMPLE D'UTILISATION COMPLET
  # ============================================================================

  # Exécution complète avec visualisations
  run_complete_analysis_with_plots <- function() {
    # Exécution du benchmark
    results <- run_complete_analysis()
    
    if (is.null(results)) return(NULL)
    
    # Génération des graphiques
    cat("Génération des visualisations...\n")
    
    # Graphiques principaux
    p1 <- plot_coverage_vs_N_optimized(results$raw_results, "FST")
    p2 <- plot_width_vs_N_optimized(results$raw_results, "FST")
    p3 <- plot_bias_vs_N_optimized(results$raw_results, "FST")
    
    # Graphiques de comparaison
    p4 <- plot_coverage_heatmap_optimized(results$comparison, "FST")
    p5 <- plot_width_ratio_boxplot_optimized(results$comparison, "FST") 
    p6 <- plot_winkler_heatmap_optimized(results$comparison, "FST")
    
    # Affichage
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    print(p5)
    print(p6)
    
    # Génération du rapport complet
    generate_comprehensive_report(results)
    
    return(list(
      results = results,
      plots = list(
        coverage = p1,
        width = p2,
        bias = p3,
        heatmap = p4,
        ratio = p5,
        winkler = p6
      )
    ))
  }

  # Exécution
  # full_analysis <- run_complete_analysis_with_plots()

  ###
  
  list(
    raw_results = res,
    scenario_summary = scen_sum,
    comparison = cmp_scen,
    overview = overview
  )
}

# Exécution
analysis_results <- run_complete_analysis()