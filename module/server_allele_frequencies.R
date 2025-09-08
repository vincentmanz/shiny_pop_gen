# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server_allele_frequencies <- function(input, output, session) {
  
  # Variables globales (héritées de la page précédente)
  tryCatch({
    filtered_data <- read.csv("data/filtered_data.csv", header = TRUE)
  }, error = function(e) {
    showNotification("Erreur lors du chargement des données", type = "error")
    return(NULL)
  })
  
  if (is.null(filtered_data)) return(NULL)
  
  n_marker <- 6
  n_pop <- length(unique(filtered_data$Population))
  marker_start <- 6
  marker_end <- 11
  sequence_length <- length(marker_start:marker_end)
  n_indv <- nrow(filtered_data)
  pops <- unique(filtered_data$Population)
  num_cores <- parallel::detectCores()
  missing_data_code <- "0/0"
  column_genotype_start <- 6
  column_genotype_end <- 11
  R <- 1000
  n_rep <- 1000
  
  # Noms des marqueurs - vérification de l'existence des colonnes
  if (ncol(filtered_data) >= marker_end) {
    marker_names <- names(filtered_data)[marker_start:marker_end]
  } else {
    # Si les indices ne correspondent pas, prendre toutes les colonnes après Population
    marker_names <- names(filtered_data)[5:ncol(filtered_data)]  # En supposant que Population est en colonne 1
    marker_start <- 5
    marker_end <- ncol(filtered_data)
    n_marker <- length(marker_names)
  }
  
  # Vérifier que les colonnes existent
  marker_names <- marker_names[marker_names %in% names(filtered_data)]
  
  if (length(marker_names) == 0) {
    showNotification("Aucune colonne de marqueurs trouvée", type = "error")
    return(NULL)
  }
  
  # Initialiser les choix des inputs
  observe({
    updateSelectInput(session, "selected_population",
                     choices = c("Toutes les populations" = "all", 
                               setNames(pops, pops)))
    
    updateSelectInput(session, "selected_marker",
                     choices = c("Tous les marqueurs" = "all",
                               setNames(marker_names, marker_names)))
    
    # Initialiser les populations pour le filtre
    updateCheckboxGroupInput(session, "populations_to_include",
                           choices = setNames(pops, pops),
                           selected = pops)
  })
  
  # Fonction pour extraire les allèles d'un génotype
  extract_alleles <- function(genotype) {
    if (is.na(genotype) || genotype == missing_data_code || genotype == "") {
      return(c(NA, NA))
    }
    alleles <- strsplit(as.character(genotype), "/")[[1]]
    if (length(alleles) == 2) {
      return(as.numeric(alleles))
    } else {
      return(c(NA, NA))
    }
  }
  
  # Calculer les fréquences alléliques
  calculate_allele_frequencies <- reactive({
    req(input$update_analysis)
    
    isolate({
      results_by_pop <- list()
      results_global <- list()
      
      for (marker in marker_names) {
        # Par population
        pop_results <- data.frame()
        all_alleles <- c()
        
        for (pop in pops) {
          pop_data <- filtered_data[filtered_data$Population == pop, marker]
          alleles <- unlist(lapply(pop_data, extract_alleles))
          alleles <- alleles[!is.na(alleles)]
          all_alleles <- c(all_alleles, alleles)
          
          if (length(alleles) > 0) {
            freq_table <- table(alleles)
            freq_prop <- prop.table(freq_table)
            
            pop_result <- data.frame(
              Population = pop,
              Marqueur = marker,
              Allele = names(freq_prop),
              Frequence = as.numeric(freq_prop),
              Count = as.numeric(freq_table),
              N_total = length(alleles),
              stringsAsFactors = FALSE
            )
            pop_results <- rbind(pop_results, pop_result)
          }
        }
        
        results_by_pop[[marker]] <- pop_results
        
        # Global
        if (length(all_alleles) > 0) {
          freq_table_global <- table(all_alleles)
          freq_prop_global <- prop.table(freq_table_global)
          
          global_result <- data.frame(
            Marqueur = marker,
            Allele = names(freq_prop_global),
            Frequence = as.numeric(freq_prop_global),
            Count = as.numeric(freq_table_global),
            N_total = length(all_alleles),
            stringsAsFactors = FALSE
          )
          results_global[[marker]] <- global_result
        }
      }
      
      list(
        by_pop = do.call(rbind, results_by_pop),
        global = do.call(rbind, results_global)
      )
    })
  })
  
  # Calculer les données manquantes
  calculate_missing_data <- reactive({
    missing_summary <- data.frame()
    
    # Vérifier que les données et les colonnes existent
    if (is.null(filtered_data) || length(marker_names) == 0) {
      return(missing_summary)
    }
    
    for (pop in pops) {
      pop_data <- filtered_data[filtered_data$Population == pop, marker_names, drop = FALSE]
      n_indiv_pop <- nrow(pop_data)
      
      for (marker in marker_names) {
        if (marker %in% names(pop_data)) {
          missing_count <- sum(is.na(pop_data[[marker]]) | 
                             pop_data[[marker]] == missing_data_code | 
                             pop_data[[marker]] == "" |
                             is.null(pop_data[[marker]]))
          
          missing_summary <- rbind(missing_summary, data.frame(
            Population = pop,
            Marqueur = marker,
            N_individus = n_indiv_pop,
            Donnees_manquantes = missing_count,
            Pourcentage_manquant = round((missing_count / n_indiv_pop) * 100, 2),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    missing_summary
  })
  
  # Value boxes
  output$total_individuals <- renderValueBox({
    valueBox(
      value = if(!is.null(filtered_data)) n_indv else 0,
      subtitle = "Individus Total",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_populations <- renderValueBox({
    valueBox(
      value = if(!is.null(filtered_data)) n_pop else 0,
      subtitle = "Populations",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$total_markers <- renderValueBox({
    valueBox(
      value = if(!is.null(filtered_data)) length(marker_names) else 0,
      subtitle = "Marqueurs",
      icon = icon("dna"),
      color = "purple"
    )
  })
  
  output$total_missing <- renderValueBox({
    if (is.null(filtered_data) || length(marker_names) == 0) {
      valueBox(
        value = "N/A",
        subtitle = "Données Manquantes",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    } else {
      missing_data <- calculate_missing_data()
      total_missing_pct <- if(nrow(missing_data) > 0) {
        round(mean(missing_data$Pourcentage_manquant), 2)
      } else {
        0
      }
      
      valueBox(
        value = paste0(total_missing_pct, "%"),
        subtitle = "Données Manquantes",
        icon = icon("exclamation-triangle"),
        color = if(total_missing_pct > 20) "red" else if(total_missing_pct > 10) "yellow" else "green"
      )
    }
  })
  
  # Table des données manquantes
  output$missing_data_summary <- renderTable({
    if (is.null(filtered_data) || length(marker_names) == 0) {
      return(data.frame(Message = "Aucune donnée disponible"))
    }
    
    missing_data <- calculate_missing_data()
    if (nrow(missing_data) == 0) {
      return(data.frame(Message = "Aucune donnée manquante calculée"))
    }
    
    summary_by_pop <- aggregate(cbind(Donnees_manquantes, N_individus) ~ Population, 
                               data = missing_data, FUN = sum)
    summary_by_pop$Pourcentage <- round((summary_by_pop$Donnees_manquantes / 
                                       (summary_by_pop$N_individus * length(marker_names))) * 100, 2)
    names(summary_by_pop) <- c("Population", "Total Manquant", "Total Possible", "% Manquant")
    summary_by_pop
  }, striped = TRUE, hover = TRUE)
  
  # Table des fréquences par population
  output$allele_freq_by_pop <- DT::renderDataTable({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    data_to_show <- freq_data$by_pop
    
    if (input$selected_population != "all") {
      data_to_show <- data_to_show[data_to_show$Population == input$selected_population, ]
    }
    
    if (input$selected_marker != "all") {
      data_to_show <- data_to_show[data_to_show$Marqueur == input$selected_marker, ]
    }
    
    data_to_show$Frequence <- round(data_to_show$Frequence, 4)
    
    DT::datatable(data_to_show, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("Frequence", 
                     backgroundColor = DT::styleInterval(c(0.1, 0.5), 
                                                        c("#ffebee", "#fff3e0", "#e8f5e8")))
  })
  
  # Table des fréquences globales
  output$allele_freq_global <- DT::renderDataTable({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    data_to_show <- freq_data$global
    
    if (input$selected_marker != "all") {
      data_to_show <- data_to_show[data_to_show$Marqueur == input$selected_marker, ]
    }
    
    data_to_show$Frequence <- round(data_to_show$Frequence, 4)
    
    DT::datatable(data_to_show,
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("Frequence",
                     backgroundColor = DT::styleInterval(c(0.1, 0.5),
                                                        c("#ffebee", "#fff3e0", "#e8f5e8")))
  })
  
  # Graphique des fréquences alléliques
  output$allele_freq_plot <- renderPlot({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    if (input$selected_marker != "all") {
      plot_data <- freq_data$by_pop[freq_data$by_pop$Marqueur == input$selected_marker, ]
      title_suffix <- paste("- Marqueur:", input$selected_marker)
    } else {
      plot_data <- freq_data$by_pop
      title_suffix <- "- Tous marqueurs"
    }
    
    if (input$selected_population != "all") {
      plot_data <- plot_data[plot_data$Population == input$selected_population, ]
    }
    
    if (nrow(plot_data) > 0) {
      library(ggplot2)
      ggplot(plot_data, aes(x = Allele, y = Frequence, fill = Population)) +
        geom_col(position = "dodge") +
        facet_wrap(~Marqueur, scales = "free_x") +
        labs(title = paste("Fréquences Alléliques", title_suffix),
             x = "Allèles", y = "Fréquence") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d()
    }
  })
  
  # Graphique des données manquantes
  output$missing_data_plot <- renderPlot({
    missing_data <- calculate_missing_data()
    
    library(ggplot2)
    ggplot(missing_data, aes(x = Marqueur, y = Pourcentage_manquant, fill = Population)) +
      geom_col(position = "dodge") +
      labs(title = "Pourcentage de Données Manquantes",
           x = "Marqueurs", y = "% Données Manquantes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d() +
      geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.7)
  })
  
  # Détails par marqueur
  output$marker_details <- DT::renderDataTable({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    missing_data <- calculate_missing_data()
    
    # Résumé par marqueur
    marker_summary <- data.frame()
    
    for (marker in marker_names) {
      freq_marker <- freq_data$global[freq_data$global$Marqueur == marker, ]
      missing_marker <- missing_data[missing_data$Marqueur == marker, ]
      
      n_alleles <- nrow(freq_marker)
      avg_missing <- round(mean(missing_marker$Pourcentage_manquant), 2)
      total_observations <- sum(freq_marker$Count)
      
      marker_summary <- rbind(marker_summary, data.frame(
        Marqueur = marker,
        N_alleles = n_alleles,
        Total_observations = total_observations,
        Moyenne_manquant_pct = avg_missing,
        Alleles = paste(freq_marker$Allele, collapse = ", "),
        stringsAsFactors = FALSE
      ))
    }
    
    DT::datatable(marker_summary,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Marqueur", "Nb Allèles", "Total Obs.", 
                              "% Moy. Manquant", "Liste Allèles")) %>%
      DT::formatStyle("Moyenne_manquant_pct",
                     backgroundColor = DT::styleInterval(c(10, 20),
                                                        c("#e8f5e8", "#fff3e0", "#ffebee")))
  })
  
  # =============================================================================
  # NOUVELLES FONCTIONNALITÉS AJOUTÉES
  # =============================================================================
  
  # Calcul des indices de diversité génétique
  calculate_genetic_diversity <- reactive({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    diversity_results <- data.frame()
    
    for (pop in pops) {
      for (marker in marker_names) {
        # Données pour cette population et ce marqueur
        pop_marker_data <- freq_data$by_pop[
          freq_data$by_pop$Population == pop & freq_data$by_pop$Marqueur == marker, ]
        
        if (nrow(pop_marker_data) > 0) {
          # Nombre d'allèles
          Na <- nrow(pop_marker_data)
          
          # Fréquences alléliques
          freqs <- pop_marker_data$Frequence
          
          # Hétérozygotie attendue (He)
          He <- 1 - sum(freqs^2)
          
          # Nombre effectif d'allèles
          Ne <- 1 / sum(freqs^2)
          
          # Calcul Ho (hétérozygotie observée) - approximation
          pop_genotypes <- filtered_data[filtered_data$Population == pop, marker]
          total_genotypes <- sum(!is.na(pop_genotypes) & 
                               pop_genotypes != missing_data_code & 
                               pop_genotypes != "")
          
          # Compter les hétérozygotes
          het_count <- 0
          for (geno in pop_genotypes) {
            if (!is.na(geno) && geno != missing_data_code && geno != "") {
              alleles <- extract_alleles(geno)
              if (!any(is.na(alleles)) && alleles[1] != alleles[2]) {
                het_count <- het_count + 1
              }
            }
          }
          
          Ho <- if(total_genotypes > 0) het_count / total_genotypes else 0
          
          # Coefficient de consanguinité (Fis)
          Fis <- if(He > 0) (He - Ho) / He else 0
          
          diversity_results <- rbind(diversity_results, data.frame(
            Population = pop,
            Marqueur = marker,
            Na = Na,
            Ne = round(Ne, 3),
            He = round(He, 3),
            Ho = round(Ho, 3),
            Fis = round(Fis, 3),
            N_genotypes = total_genotypes,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    diversity_results
  })
  
  # Matrice présence/absence des allèles
  calculate_allele_matrix <- reactive({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    # Obtenir tous les allèles uniques par marqueur
    matrix_list <- list()
    
    for (marker in marker_names) {
      marker_data <- freq_data$by_pop[freq_data$by_pop$Marqueur == marker, ]
      all_alleles <- unique(marker_data$Allele)
      
      # Créer matrice pour ce marqueur
      marker_matrix <- matrix(0, nrow = length(pops), ncol = length(all_alleles),
                             dimnames = list(pops, paste0(marker, "_", all_alleles)))
      
      for (i in seq_along(pops)) {
        pop_alleles <- marker_data[marker_data$Population == pops[i], "Allele"]
        marker_matrix[i, paste0(marker, "_", pop_alleles)] <- 1
      }
      
      matrix_list[[marker]] <- marker_matrix
    }
    
    # Combiner toutes les matrices
    full_matrix <- do.call(cbind, matrix_list)
    as.data.frame(full_matrix)
  })
  
  # Analyse des allèles rares
  calculate_rare_alleles <- reactive({
    req(input$rare_threshold)
    freq_data <- calculate_allele_frequencies()
    
    rare_alleles <- freq_data$global[freq_data$global$Frequence < (input$rare_threshold / 100), ]
    rare_alleles$Frequence_pct <- round(rare_alleles$Frequence * 100, 2)
    rare_alleles[order(rare_alleles$Frequence), ]
  })
  
  # Analyse de qualité des données
  calculate_data_quality <- reactive({
    missing_data <- calculate_missing_data()
    freq_data <- calculate_allele_frequencies()
    
    quality_summary <- data.frame()
    
    for (pop in pops) {
      pop_missing <- missing_data[missing_data$Population == pop, ]
      pop_size <- nrow(filtered_data[filtered_data$Population == pop, ])
      
      avg_missing <- round(mean(pop_missing$Pourcentage_manquant), 2)
      markers_with_data <- sum(pop_missing$Pourcentage_manquant < 100)
      
      quality_summary <- rbind(quality_summary, data.frame(
        Population = pop,
        Taille_echantillon = pop_size,
        Marqueurs_avec_donnees = markers_with_data,
        Moyenne_manquant_pct = avg_missing,
        Qualite = case_when(
          avg_missing < 10 ~ "Excellente",
          avg_missing < 25 ~ "Bonne", 
          avg_missing < 50 ~ "Moyenne",
          TRUE ~ "Faible"
        ),
        stringsAsFactors = FALSE
      ))
    }
    
    quality_summary
  })
  
  # Outputs pour la diversité génétique
  output$genetic_diversity_table <- DT::renderDataTable({
    diversity_data <- calculate_genetic_diversity()
    
    DT::datatable(diversity_data,
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle(c("He", "Ho", "Fis"),
                     backgroundColor = DT::styleInterval(c(0.3, 0.6),
                                                        c("#ffebee", "#fff3e0", "#e8f5e8")))
  })
  
  # Output pour la matrice allélique
  output$allele_matrix <- DT::renderDataTable({
    matrix_data <- calculate_allele_matrix()
    
    DT::datatable(matrix_data,
                  options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
                  rownames = TRUE) %>%
      DT::formatStyle(names(matrix_data),
                     backgroundColor = DT::styleEqual(1, "#4CAF50"),
                     color = DT::styleEqual(1, "white"))
  })
  
  # Output pour les allèles rares
  output$rare_alleles_table <- DT::renderDataTable({
    rare_data <- calculate_rare_alleles()
    
    DT::datatable(rare_data[, c("Marqueur", "Allele", "Frequence_pct", "Count", "N_total")],
                  options = list(pageLength = 10),
                  rownames = FALSE,
                  colnames = c("Marqueur", "Allèle", "Fréquence (%)", "Effectif", "Total")) %>%
      DT::formatStyle("Frequence_pct",
                     backgroundColor = DT::styleInterval(c(1, 3),
                                                        c("#ffcdd2", "#ffecb3", "#fff3e0")))
  })
  
  # Output pour la qualité des données
  output$data_quality_summary <- DT::renderDataTable({
    quality_data <- calculate_data_quality()
    
    DT::datatable(quality_data,
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE) %>%
      DT::formatStyle("Qualite",
                     backgroundColor = DT::styleEqual(
                       c("Excellente", "Bonne", "Moyenne", "Faible"),
                       c("#4CAF50", "#8BC34A", "#FF9800", "#F44336")
                     ),
                     color = "white")
  })
  
  # Statistiques rapides
  output$quick_stats_pop <- renderText({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    if (input$selected_population != "all") {
      data_subset <- freq_data$by_pop[freq_data$by_pop$Population == input$selected_population, ]
      pop_name <- input$selected_population
    } else {
      data_subset <- freq_data$by_pop
      pop_name <- "Toutes populations"
    }
    
    if (nrow(data_subset) > 0) {
      total_alleles <- nrow(data_subset)
      avg_freq <- round(mean(data_subset$Frequence), 3)
      max_freq <- round(max(data_subset$Frequence), 3)
      min_freq <- round(min(data_subset$Frequence), 3)
      
      paste0(
        "Population: ", pop_name, "\n",
        "Total allèles: ", total_alleles, "\n",
        "Fréq. moyenne: ", avg_freq, "\n",
        "Fréq. max: ", max_freq, "\n",
        "Fréq. min: ", min_freq
      )
    } else {
      "Aucune donnée disponible"
    }
  })
  
  output$quick_stats_global <- renderText({
    req(input$update_analysis)
    freq_data <- calculate_allele_frequencies()
    
    if (nrow(freq_data$global) > 0) {
      total_alleles <- nrow(freq_data$global)
      avg_freq <- round(mean(freq_data$global$Frequence), 3)
      polymorphic_markers <- length(unique(freq_data$global$Marqueur))
      
      paste0(
        "Analyse globale\n",
        "Total allèles: ", total_alleles, "\n",
        "Marqueurs polymorphes: ", polymorphic_markers, "\n",
        "Fréq. moyenne: ", avg_freq
      )
    } else {
      "Aucune donnée disponible"
    }
  })
  
  # Nouveaux graphiques
  output$diversity_plot <- renderPlot({
    req(input$update_analysis)
    diversity_data <- calculate_genetic_diversity()
    
    if (nrow(diversity_data) > 0) {
      library(ggplot2)
      ggplot(diversity_data, aes(x = Population, y = He, fill = Population)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~Marqueur) +
        labs(title = "Hétérozygotie Attendue par Population",
             x = "Population", y = "Hétérozygotie Attendue (He)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        scale_fill_viridis_d()
    }
  })
  
  output$allele_richness_plot <- renderPlot({
    req(input$update_analysis)
    diversity_data <- calculate_genetic_diversity()
    
    if (nrow(diversity_data) > 0) {
      library(ggplot2)
      ggplot(diversity_data, aes(x = Population, y = Na, fill = Marqueur)) +
        geom_col(position = "dodge") +
        labs(title = "Richesse Allélique par Population et Marqueur",
             x = "Population", y = "Nombre d'Allèles (Na)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d()
    }
  })
  
  output$allele_sharing_heatmap <- renderPlot({
    matrix_data <- calculate_allele_matrix()
    
    if (nrow(matrix_data) > 0) {
      # Calculer similarité entre populations
      similarity_matrix <- cor(t(as.matrix(matrix_data)), use = "complete.obs")
      
      # Convertir en format long pour ggplot
      library(reshape2)
      sim_long <- melt(similarity_matrix)
      
      library(ggplot2)
      ggplot(sim_long, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, name = "Similarité") +
        labs(title = "Partage d'Allèles entre Populations",
             x = "Population", y = "Population") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Fonctions de téléchargement
  output$download_freq_pop <- downloadHandler(
    filename = function() {
      paste0("frequencies_par_population_", Sys.Date(), ".csv")
    },
    content = function(file) {
      freq_data <- calculate_allele_frequencies()
      write.csv(freq_data$by_pop, file, row.names = FALSE)
    }
  )
  
  output$download_freq_global <- downloadHandler(
    filename = function() {
      paste0("frequencies_globales_", Sys.Date(), ".csv")
    },
    content = function(file) {
      freq_data <- calculate_allele_frequencies()
      write.csv(freq_data$global, file, row.names = FALSE)
    }
  )
  
  output$download_diversity <- downloadHandler(
    filename = function() {
      paste0("diversite_genetique_", Sys.Date(), ".csv")
    },
    content = function(file) {
      diversity_data <- calculate_genetic_diversity()
      write.csv(diversity_data, file, row.names = FALSE)
    }
  )
  
  output$download_matrix <- downloadHandler(
    filename = function() {
      paste0("matrice_allelique_", Sys.Date(), ".csv")
    },
    content = function(file) {
      matrix_data <- calculate_allele_matrix()
      write.csv(matrix_data, file, row.names = TRUE)
    }
  )
}