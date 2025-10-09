# test_permute.R - Test de la fonction corrigée
library(Rcpp)
library(hierfstat)

# Recompilation
Rcpp::sourceCpp("www/permute.cpp")

# Données de test simples
set.seed(123)
test_data <- data.frame(
  pop = rep(1:2, each = 10),
  locus1 = c(100100, 100200, 100100, 100200, 100100, 
             200200, 200200, 100200, 100100, 100100,
             100100, 100200, 200200, 100100, 100200,
             100100, 100100, 200200, 100200, 100100),
  locus2 = c(300300, 300400, 300300, 300400, 300300,
             300400, 300300, 300400, 300300, 300300,
             300300, 300400, 300300, 300400, 300300,
             300400, 300300, 300400, 300300, 300400)
)

# Conversion en matrice entière
test_mat <- as.matrix(test_data)
storage.mode(test_mat) <- "integer"

cat("🔧 Test de la fonction batch_permute_wc_fis...\n")

# Test avec un petit nombre de permutations
tryCatch({
  result <- batch_permute_wc_fis(test_mat, pop_col_1based = 1, missing_code = 0, B = 10)
  
  cat("✅ SUCCÈS - Fonction exécutée sans erreur\n")
  cat("Dimensions résultat:", nrow(result$permutations), "x", ncol(result$permutations), "\n")
  cat("Noms des loci:", paste(result$locus_names, collapse = ", "), "\n")
  
  # Test de la fonction utilitaire
  fis_test <- quick_fis_test(test_mat, 1, 0)
  cat("FIS calculés:", paste(round(fis_test, 4), collapse = ", "), "\n")
  
}, error = function(e) {
  cat("❌ ERREUR:", e$message, "\n")
})

# Test de simulation d'une permutation
tryCatch({
  sim_result <- simulate_permutation(test_mat, 1, 0)
  cat("✅ Simulation de permutation réussie\n")
  cat("Dimensions matrice simulée:", nrow(sim_result), "x", ncol(sim_result), "\n")
}, error = function(e) {
  cat("❌ Erreur simulation:", e$message, "\n")
})