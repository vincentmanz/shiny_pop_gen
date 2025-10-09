# scripts/01_import_data.R
cat("📁 Importation des données...\n")

# Fonction d'importation
import_genetic_data <- function(csv_path = "data/filtered_data.csv") {
  if (!file.exists(csv_path)) {
    stop("❌ Fichier CSV introuvable: ", csv_path)
  }
  
  CSV_BOU <- read.csv(csv_path)
  cat("✅ Données CSV chargées:", nrow(CSV_BOU), "lignes,", ncol(CSV_BOU), "colonnes\n")
  
  # Création de formatted_data
  formatted_data <- list(
    Population   = CSV_BOU$Population,
    individual   = paste0("Ind_", substr(CSV_BOU$Population, 1, 3), "_", 1:nrow(CSV_BOU)),
    ploidy       = 2,
    haplotype    = as.data.frame(CSV_BOU[, 5:10]),
    missing_code = "0/0",
    GPS          = if ("GPS" %in% names(CSV_BOU)) CSV_BOU$GPS else NULL,
    level1       = if ("level1" %in% names(CSV_BOU)) CSV_BOU$level1 else NULL,
    level2       = if ("level2" %in% names(CSV_BOU)) CSV_BOU$level2 else NULL,
    level3       = if ("level3" %in% names(CSV_BOU)) CSV_BOU$level3 else NULL
  )
  
  # Sauvegarde
  save(formatted_data, file = "data/formatted_data.RData")
  cat("✅ Données formatées sauvegardées: data/formatted_data.RData\n")
  
  return(formatted_data)
}

# Exécution
formatted_data <- import_genetic_data()