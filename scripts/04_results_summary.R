# scripts/04_results_summary.R
cat("📈 Synthèse des résultats...\n")

# Chargement des résultats
if (file.exists("data/genetic_analysis_results.RData")) {
  load("data/genetic_analysis_results.RData")
  
  # Génération du rapport
  generate_report(results)
}

cat("✅ Synthèse des résultats terminée\n")