## ---- helper_test.R ----
# Fonctions utilitaires pour les tests statistiques et le formatage

# ---------- Fonctions de test statistique ----------

#' Test de permutation bilatéral
#' 
#' Calcule la p-value bilatérale à partir des valeurs observées et permutées
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @return p-value bilatérale (minimum entre les deux queues, multiplié par 2)
#' @examples
#' obs <- 2.5
#' perm <- rnorm(1000, mean = 0, sd = 1)
#' p_two(obs, perm)
p_two <- function(obs, perm_vec) {
  # Validation des entrées
  if (is.na(obs) || length(perm_vec) == 0) {
    return(NA_real_)
  }
  
  # Nettoyage des valeurs manquantes
  v <- perm_vec[!is.na(perm_vec)]
  n_valid <- length(v)
  
  if (n_valid == 0) {
    return(NA_real_)
  }
  
  # Calcul des probabilités des deux queues
  p_upper <- mean(v >= obs)
  p_lower <- mean(v <= obs)
  
  # p-value bilatérale (méthode conservatrice)
  p_value <- 2 * min(p_upper, p_lower)
  
  # Correction pour éviter les p-values > 1
  return(min(1.0, p_value))
}

#' Test de permutation unilatéral (queue supérieure)
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @return p-value unilatérale (queue supérieure)
p_upper <- function(obs, perm_vec) {
  if (is.na(obs) || length(perm_vec) == 0) {
    return(NA_real_)
  }
  
  v <- perm_vec[!is.na(perm_vec)]
  if (length(v) == 0) return(NA_real_)
  
  return(mean(v >= obs))
}

#' Test de permutation unilatéral (queue inférieure)
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @return p-value unilatérale (queue inférieure)
p_lower <- function(obs, perm_vec) {
  if (is.na(obs) || length(perm_vec) == 0) {
    return(NA_real_)
  }
  
  v <- perm_vec[!is.na(perm_vec)]
  if (length(v) == 0) return(NA_real_)
  
  return(mean(v <= obs))
}

#' Test de permutation avec correction de continuité
#' 
#' Évite les p-values de 0 en ajoutant 1 au numérateur et dénominateur
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @return p-value bilatérale avec correction
p_two_continuity <- function(obs, perm_vec) {
  if (is.na(obs) || length(perm_vec) == 0) {
    return(NA_real_)
  }
  
  v <- perm_vec[!is.na(perm_vec)]
  n_valid <- length(v)
  
  if (n_valid == 0) {
    return(NA_real_)
  }
  
  # Correction de continuité
  count_upper <- sum(v >= obs) + 1
  count_lower <- sum(v <= obs) + 1
  
  p_upper <- count_upper / (n_valid + 1)
  p_lower <- count_lower / (n_valid + 1)
  
  p_value <- 2 * min(p_upper, p_lower)
  
  return(min(1.0, p_value))
}

# ---------- Fonctions de formatage ----------

#' Formatage de nombres avec précision fixe
#' 
#' @param x Vecteur numérique à formater
#' @param digits Nombre de décimales (défaut: 6)
#' @return Vecteur de caractères formatés
#' @examples
#' fmt_fixed(c(1.234567, 0.000123, NA, 123.456))
fmt_fixed <- function(x, digits = 6) {
  if (!is.numeric(x)) {
    stop("L'entrée doit être numérique")
  }
  
  # Formatage avec gestion des NA
  result <- character(length(x))
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- "NA"
    } else {
      result[i] <- formatC(x[i], format = "f", digits = digits)
    }
  }
  
  return(result)
}

#' Formatage scientifique pour les très petits nombres
#' 
#' @param x Vecteur numérique
#' @param digits Nombre de chiffres significatifs
#' @param threshold Seuil pour basculer en notation scientifique
#' @return Vecteur formaté
fmt_scientific <- function(x, digits = 3, threshold = 0.001) {
  if (!is.numeric(x)) {
    stop("L'entrée doit être numérique")
  }
  
  result <- character(length(x))
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- "NA"
    } else if (abs(x[i]) < threshold & x[i] != 0) {
      result[i] <- formatC(x[i], format = "e", digits = digits)
    } else {
      result[i] <- formatC(x[i], format = "f", digits = digits)
    }
  }
  
  return(result)
}

#' Formatage des p-values
#' 
#' @param p_vals Vecteur de p-values
#' @param digits Nombre de décimales pour les p-values > 0.001
#' @return Vecteur de p-values formatées
#' @examples
#' pvals <- c(0.000023, 0.0456, 0.789, NA, 0.0000001)
#' fmt_pvalue(pvals)
fmt_pvalue <- function(p_vals, digits = 3) {
  if (!is.numeric(p_vals)) {
    stop("L'entrée doit être numérique")
  }
  
  result <- character(length(p_vals))
  
  for (i in seq_along(p_vals)) {
    p <- p_vals[i]
    
    if (is.na(p)) {
      result[i] <- "NA"
    } else if (p < 0.001) {
      result[i] <- "<0.001"
    } else if (p < 0.01) {
      result[i] <- formatC(p, format = "f", digits = 3)
    } else {
      result[i] <- formatC(p, format = "f", digits = digits)
    }
  }
  
  return(result)
}

#' Formatage des intervalles de confiance
#' 
#' @param estimate Estimation ponctuelle
#' @param lower Borne inférieure de l'IC
#' @param upper Borne supérieure de l'IC
#' @param digits Nombre de décimales
#' @return Chaîne formatée "estimate [lower, upper]"
fmt_confidence_interval <- function(estimate, lower, upper, digits = 3) {
  if (any(is.na(c(estimate, lower, upper)))) {
    return("NA [NA, NA]")
  }
  
  est_fmt <- formatC(estimate, format = "f", digits = digits)
  lower_fmt <- formatC(lower, format = "f", digits = digits)
  upper_fmt <- formatC(upper, format = "f", digits = digits)
  
  return(paste0(est_fmt, " [", lower_fmt, ", ", upper_fmt, "]"))
}

# ---------- Fonctions de validation statistique ----------

#' Vérifie la normalité d'un vecteur avec test de Shapiro-Wilk
#' 
#' @param x Vecteur numérique
#' @return Liste avec statistique W et p-value
check_normality <- function(x) {
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) < 3) {
    return(list(W = NA_real_, p.value = NA_real_, note = "Insufficient data"))
  }
  
  if (length(x_clean) > 5000) {
    # Échantillonnage pour les grands jeux de données
    x_clean <- sample(x_clean, 5000)
  }
  
  test_result <- shapiro.test(x_clean)
  
  return(list(
    W = test_result$statistic,
    p.value = test_result$p.value,
    n = length(x_clean)
  ))
}

#' Calcule la puissance d'un test de permutation
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @param alpha Niveau de significativité (défaut: 0.05)
#' @return Puissance estimée
calculate_power <- function(obs, perm_vec, alpha = 0.05) {
  if (is.na(obs) || length(perm_vec) == 0) {
    return(NA_real_)
  }
  
  v <- perm_vec[!is.na(perm_vec)]
  n_valid <- length(v)
  
  if (n_valid == 0) {
    return(NA_real_)
  }
  
  # Seuil critique basé sur la distribution permutée
  critical_value <- quantile(v, 1 - alpha, na.rm = TRUE)
  
  # Puissance = P(rejeter H0 | H1 vraie)
  # Estimation basée sur la position de obs par rapport au seuil
  power <- mean(v >= critical_value)
  
  return(power)
}

# ---------- Fonctions de rapport ----------

#' Génère un rapport statistique complet
#' 
#' @param obs Valeur observée
#' @param perm_vec Vecteur des valeurs permutées
#' @param test_type Type de test ("two.sided", "greater", "less")
#' @return Liste avec les résultats complets
statistical_report <- function(obs, perm_vec, test_type = "two.sided") {
  # Nettoyage des données
  v_clean <- perm_vec[!is.na(perm_vec)]
  n_valid <- length(v_clean)
  
  if (n_valid == 0 || is.na(obs)) {
    return(list(
      observed = obs,
      n_permutations = n_valid,
      p_value = NA_real_,
      note = "Insufficient data"
    ))
  }
  
  # Calcul de la p-value selon le type de test
  p_value <- switch(test_type,
    "two.sided" = p_two(obs, v_clean),
    "greater" = p_upper(obs, v_clean),
    "less" = p_lower(obs, v_clean),
    NA_real_
  )
  
  # Statistiques descriptives
  perm_mean <- mean(v_clean)
  perm_sd <- sd(v_clean)
  perm_ci <- quantile(v_clean, c(0.025, 0.975))
  
  # Test de normalité
  normality <- check_normality(v_clean)
  
  # Puissance
  power <- calculate_power(obs, v_clean)
  
  return(list(
    observed = obs,
    n_permutations = n_valid,
    p_value = p_value,
    p_value_continuity = p_two_continuity(obs, v_clean),
    permutation_mean = perm_mean,
    permutation_sd = perm_sd,
    permutation_ci_95 = perm_ci,
    normality_test = normality,
    estimated_power = power,
    test_type = test_type
  ))
}

#' Formatage d'un rapport statistique pour l'affichage
#' 
#' @param report Liste de rapport statistique
#' @return DataFrame formaté
format_statistical_report <- function(report) {
  if (!is.list(report) || is.null(report$p_value)) {
    return(data.frame(Statistic = "Error", Value = "Invalid report"))
  }
  
  results <- data.frame(
    Statistic = c(
      "Valeur observée",
      "Nombre de permutations",
      "P-value (bilatérale)",
      "P-value (avec correction)",
      "Moyenne des permutations",
      "Écart-type des permutations",
      "IC 95% des permutations",
      "Normalité (Shapiro-Wilk)",
      "Puissance estimée"
    ),
    Value = c(
      fmt_fixed(report$observed, 4),
      as.character(report$n_permutations),
      fmt_pvalue(report$p_value),
      fmt_pvalue(report$p_value_continuity),
      fmt_fixed(report$permutation_mean, 4),
      fmt_fixed(report$permutation_sd, 4),
      paste0("[", fmt_fixed(report$permutation_ci_95[1], 4), ", ", 
             fmt_fixed(report$permutation_ci_95[2], 4), "]"),
      ifelse(!is.na(report$normality_test$p.value), 
             fmt_pvalue(report$normality_test$p.value), "NA"),
      fmt_fixed(report$estimated_power, 3)
    ),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# ---------- Tests unitaires ----------

#' Tests unitaires pour les fonctions statistiques
test_statistical_helpers <- function() {
  message("Lancement des tests unitaires...")
  
  # Test données
  set.seed(123)
  obs <- 2.0
  perm_data <- rnorm(1000, mean = 0, sd = 1)
  
  # Test p_two
  p_val <- p_two(obs, perm_data)
  stopifnot(is.numeric(p_val), p_val >= 0, p_val <= 1)
  message("✓ p_two: OK")
  
  # Test fmt_fixed
  formatted <- fmt_fixed(c(1.234567, 0.000123))
  stopifnot(is.character(formatted), length(formatted) == 2)
  message("✓ fmt_fixed: OK")
  
  # Test fmt_pvalue
  pvals <- c(0.0001, 0.05, 0.5)
  p_fmt <- fmt_pvalue(pvals)
  stopifnot(all(nchar(p_fmt) > 0))
  message("✓ fmt_pvalue: OK")
  
  # Test rapport statistique
  report <- statistical_report(obs, perm_data)
  stopifnot(is.list(report), !is.null(report$p_value))
  message("✓ statistical_report: OK")
  
  message("Tous les tests passés avec succès!")
}

# Exemple d'utilisation
if (FALSE) {
  # Exemple interactif
  test_statistical_helpers()
  
  # Données d'exemple
  obs_value <- 2.5
  permuted_values <- rnorm(10000, mean = 0, sd = 1.5)
  
  # Rapport complet
  report <- statistical_report(obs_value, permuted_values)
  print(format_statistical_report(report))
}