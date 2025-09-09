# server_general_stats_part1.R
# 

# Load the saved RData object
load("data/formatted_data.RData") 


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
pops <- unique(formatted_data$Population)
num_cores <- parallel::detectCores()
missing_data_code <- formatted_data$missing_code


# default values bootstrap and randomization
R <- 1000
n_rep <- 1000

# Define the server logic
server_general_stats <- function(input, output, session) {
  # Reactive values
  result_stats_reactive <- reactiveVal(NULL)
  missing_data_reac <- reactiveVal(NULL)
  filtered_data_reac <- reactiveVal(NULL)
  plot_output <- reactiveValues()
  mydata_genind_reac <- reactiveVal(NULL)
  mydata_hierfstat_reac <- reactiveVal(NULL)
  result_stats_download <- reactiveVal(NULL)
  
  # Bootstrap reactive values
  boot_loci_results <- reactiveVal(NULL)
  boot_pop_results <- reactiveVal(NULL)
  
  # Permutation reactive values
  perm_local_results <- reactiveVal(NULL)
  perm_global_results <- reactiveVal(NULL)
  perm_subdiv_results <- reactiveVal(NULL)
  
  # Update the select input choices
  updateSelectInput(session, 'Level', choices = c("select" = "", colnames(filtered_data)))
  
  # Fonction de recodage robuste pour FST-max
  recode_for_fstmax <- function(hierfstat_data) {
    pops <- unique(hierfstat_data[,1])
    loci <- colnames(hierfstat_data)[-1]
    new_data <- hierfstat_data
    
    allele_counter <- 1
    
    for(pop in pops) {
      pop_rows <- which(hierfstat_data[,1] == pop)
      
      for(loc in loci) {
        # Extraire les génotypes pour cette population et ce locus
        genotypes <- hierfstat_data[pop_rows, loc]
        
        # Identifier tous les allèles uniques (en gérant correctement les NA et "0/0")
        alleles <- unique(unlist(strsplit(as.character(na.omit(genotypes[genotypes != "0/0"])), "/")))
        alleles <- alleles[!is.na(alleles) & alleles != "0"]
        
        if(length(alleles) > 0) {
          # Trier numériquement les allèles
          alleles <- sort(as.numeric(alleles))
          
          # Créer un mapping des allèles originaux vers les nouveaux codes
          allele_map <- setNames(
            sprintf("%03d", allele_counter:(allele_counter + length(alleles) - 1)),
            as.character(alleles)
          )
          allele_counter <- allele_counter + length(alleles)
          
          # Appliquer le recodage
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
    
    # Convertir en numérique (en gérant correctement les valeurs manquantes)
    for(loc in loci) {
      # Remplacer "0/0" par NA avant conversion
      new_data[,loc] <- ifelse(new_data[,loc] == "0/0", NA, new_data[,loc])
      # Supprimer les "/" et convertir en numérique
      new_data[,loc] <- suppressWarnings(as.numeric(gsub("/", "", new_data[,loc])))
    }
    
    return(new_data)
  }
  
  # Compute genind object
  mydata_genind <- adegenet::df2genind(X = as.matrix(formatted_data$haplotype), sep = "/", ncode = 6, ind.names = formatted_data$individual, pop = formatted_data$Population, NA.char = formatted_data$missing_code, ploidy = formatted_data$ploidy, type = "codom", strata = NULL, hierarchy = NULL)
  mydata_genind_reac(mydata_genind)
  
  # Compute hierfstat object
  mydata_hierfstat <- genind2hierfstat(mydata_genind)
  mydata_hierfstat_reac(mydata_hierfstat)
  
  # create the missing df
  missing_data <- info_table(mydata_genind, plot = FALSE, percent = TRUE, df = TRUE)
  missing_data <- as.data.frame(missing_data) %>% spread(key = Locus, value = Missing)
  missing_data <- missing_data %>% column_to_rownames(var = "Population")
  missing_data <- missing_data * 100
  missing_data_reac(missing_data)
  
  # Observe Event for Running the basic.stats
  observeEvent(input$run_basic_stats, {
    # Retrieve reactive value
    mydata_hierfstat_a <- mydata_hierfstat_reac()
    mydata_genind_a <- mydata_genind_reac()
    
    # Retrieve the stats selected values
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
    
    # Check if any statistics are selected
    if (!any(selected_stats)) {
      shinyalert(
        title = "No Statistics Selected",
        text = "Please select at least one statistic to compute.",
        type = "warning"
      )
      return(NULL)
    }
    
    # Compute basic stats
    result <- basic.stats(mydata_hierfstat_a)
    df_result_basic <- as.data.frame(result$perloc)
    
    # Compute Weir and Cockerham F-statistics
    data <- as.data.frame(as.loci(mydata_genind_a))
    result_f_stats <- Fst(as.loci(data))
    colnames(result_f_stats) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
    
    # Compute G statistics
    df_result_basic <- df_result_basic %>% mutate(
      GST = 1 - Hs / Ht,
      `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
    )
    
    # Variable pour stocker Fst-max (toujours calculer si Fst' est demandé)
    fst_max_values <- NULL
    
    # Compute FST-max si demandé OU si Fst' est demandé
    if(input$fst_max_checkbox || input$fst_prim_checkbox) {
      tryCatch({
        # Recode alleles for FST-max
        mydata_hierfstat_recoded <- recode_for_fstmax(mydata_hierfstat_a)
        
        # Calculate basic stats on recoded data
        result_fstmax <- basic.stats(mydata_hierfstat_recoded)
        
        # Stocker les valeurs Fst-max
        fst_max_values <- result_fstmax$perloc$Fst
        
        # Ajouter Fst-max aux résultats seulement si demandé pour l'affichage
        if(input$fst_max_checkbox) {
          df_result_basic$`Fst-max` <- fst_max_values
        }
        
        # Verify FST-max > FST standard
        if(any(fst_max_values < result_f_stats[, "Fst (W&C)"], na.rm = TRUE)) {
          showNotification(
            "Warning: Some FST-max values are lower than FST (W&C). Check allele recoding.",
            type = "warning"
          )
        }
      }, error = function(e) {
        showNotification(paste("Error in FST-max calculation:", e$message), type = "error")
        fst_max_values <- rep(NA, nrow(result_f_stats))
      })
    }
    
    # Compute Fst' (Fst/Fst-max) if requested
    if(input$fst_prim_checkbox) {
      if(is.null(fst_max_values) || all(is.na(fst_max_values))) {
        showNotification("Fst-max values are missing or all NA. Cannot calculate Fst'.", type = "error")
        df_result_basic$`Fst'` <- rep(NA, nrow(df_result_basic))
      } else {
        tryCatch({
          # Calculate Fst' = Fst / Fst-max en utilisant les valeurs de result_f_stats
          fst_wc_values <- result_f_stats[, "Fst (W&C)"]
          fst_prime_values <- fst_wc_values / fst_max_values
          
          # Ensure values are between 0 and 1
          fst_prime_values <- pmin(pmax(fst_prime_values, 0, na.rm = TRUE), 1, na.rm = TRUE)
          
          # Add to results
          df_result_basic$`Fst'` <- fst_prime_values
          
          # Diagnostic information
          cat("Fst (W&C) values:", fst_wc_values, "\n")
          cat("Fst-max values:", fst_max_values, "\n")
          cat("Fst' values:", fst_prime_values, "\n")
          
          # Check for potential issues
          if(any(fst_prime_values > 1.1, na.rm = TRUE)) {
            showNotification("Warning: Some Fst' values are >1.1. Check calculations.", type = "warning")
          }
          
          # Additional checks
          issues <- list()
          if(any(fst_prime_values < 0, na.rm = TRUE)) {
            issues <- c(issues, "negative values")
          }
          if(any(is.nan(fst_prime_values), na.rm = TRUE)) {
            issues <- c(issues, "NaN values")
          }
          if(any(is.infinite(fst_prime_values), na.rm = TRUE)) {
            issues <- c(issues, "infinite values")
          }
          
          if(length(issues) > 0) {
            showNotification(paste("Potential issues detected in Fst':", paste(issues, collapse = ", ")), type = "warning")
            # Replace problematic values with NA
            fst_prime_values[fst_prime_values < 0 | is.nan(fst_prime_values) | is.infinite(fst_prime_values)] <- NA
            df_result_basic$`Fst'` <- fst_prime_values
          }
        }, error = function(e) {
          showNotification(paste("Error in Fst' calculation:", e$message), type = "error")
          df_result_basic$`Fst'` <- rep(NA, nrow(df_result_basic))
        })
      }
    }
    
    # Merge all results
    result_stats <- merge(result_f_stats, df_result_basic, by = "row.names", all.x = TRUE)
    
    # Vérifier les colonnes disponibles avant de renommer
    available_cols <- colnames(result_stats)
    if("Fst" %in% available_cols && "Fis" %in% available_cols) {
      # Trouver les indices des colonnes Fst et Fis de Nei
      fst_nei_col <- which(available_cols == "Fst" & !available_cols %in% c("Fst (W&C)"))
      fis_nei_col <- which(available_cols == "Fis" & !available_cols %in% c("Fis (W&C)"))
      
      if(length(fst_nei_col) > 0) colnames(result_stats)[fst_nei_col[1]] <- "Fst (Nei)"
      if(length(fis_nei_col) > 0) colnames(result_stats)[fis_nei_col[1]] <- "Fis (Nei)"
    }
    
    result_stats <- col_to_rowname(result_stats, "Row.names")
    result_stats_reactive(result_stats)
    
    # Prepare final table for display - only show selected statistics
    available_stats <- intersect(names(selected_stats), colnames(result_stats))
    selected_available_stats <- available_stats[selected_stats[available_stats]]
    
    if (length(selected_available_stats) > 0) {
      # select and add Locus column
      result_stats_select <- result_stats[, selected_available_stats, drop = FALSE]
      result_stats_select$Locus <- rownames(result_stats_select)
      result_stats_select <- result_stats_select[, c("Locus", setdiff(names(result_stats_select), "Locus"))]
      
      # ---- display copy with exactly 5 decimals ----
      result_stats_display <- result_stats_select
      num_cols <- vapply(result_stats_display, is.numeric, logical(1))
      num_cols[match("Locus", names(result_stats_display))] <- FALSE 
      
      result_stats_display <- format_numeric_cols(result_stats_select, digits = 5, exclude = "Locus")
      result_stats_download(result_stats_display)

    } else {
      showNotification("No valid statistics to display", type = "warning")
      result_stats_download(NULL)
      result_stats_numeric_reactive(NULL)
    }

    output$basic_stats_result <- renderTable({
      req(result_stats_download())
      result_stats_download()   
    }, rownames = FALSE)
    
    output$basic_stats_ui <- renderUI({
      if (is.null(result_stats_download())) {
        h5("Run the analysis first to display results.")
      } else {
        tagList(
          tableOutput("basic_stats_result"),
          br(),
          downloadButton("download_gstats_csv", "Download CSV", class = "btn btn-primary")
        )
      }
    })
  })
  
  # server_general_stats_part2.R
  # Continuation de la partie 1
  
  # Basic stats download
  output$download_gstats_csv <- downloadHandler(
    filename = function() paste0("basic_stats_result_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_stats_numeric_reactive())
      df <- result_stats_numeric_reactive()
      # round numeric columns to 5 decimals for the file
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- lapply(df[num_cols], round, 5)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  
  # Observe Event for handling the plot generation
  observeEvent(input$run_plot_heatmap, {
    # Retrieve reactive value
    filtered_data_indv <- filtered_data_reac()
    missing_data <- missing_data_reac()
    
    # Data shaping
    missing_data <- rownames_to_column(missing_data, var = "location")
    
    ## Melt the data for ggplot2
    heatmap_data_melted <- melt(missing_data, value.name = "location")
    colnames(heatmap_data_melted) <- c("location", "marker", "percent")
    
    heatmap_missing <- ggplot(data = heatmap_data_melted, aes(x = location, y = marker, fill = percent)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#D6EAF8", high = "#1F618D", name = "Missing data (%)") +
      labs(title = "Distribution of Missing Data by Population and Genetic Marker",
           x = "Population", 
           y = "Genetic Marker") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13))
    
    output$plot_output <- renderPlot({
      heatmap_missing
    })
    
    # Downloadable png of the selected plot
    output$download_plot_png <- downloadHandler(
      filename = function() {
        paste("plot_heatmap_missing_data_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(filename = file, plot = heatmap_missing, dpi = 300)
      }
    )
  })
  
  # Observe Event for handling the plot generation for GST
  observeEvent(input$run_plot_GST, {
    # Retrieve reactive values
    result_stats <- result_stats_reactive()
    
    plot_GST <- ggplot(data = result_stats, aes(x = GST, y = Hs)) +
      geom_point(color = "#2C3E50") +
      geom_smooth(method = lm, color = "red", se = FALSE) +
      labs(title = "Relationship between Genetic Diversity (Hs) and Genetic Differentiation (GST)",
           x = "GST (Genetic Differentiation)",
           y = "Hs (Expected Heterozygosity)") +
      theme_ipsum()
    
    output$plot_output <- renderPlot({
      plot_GST
    })
    
    # Downloadable png of the selected plot
    output$download_plot_png <- downloadHandler(
      filename = function() {
        paste("plot_GST_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(filename = file, plot = plot_GST, dpi = 300)
      }
    )
  })
  
  # Observe Event for handling the plot generation for FIS
  observeEvent(input$run_plot_FIS, {
    # Retrieve reactive values
    result_stats <- result_stats_reactive()
    missing_data <- missing_data_reac()
    
    # Data formatting
    result_stats <- result_stats %>% rownames_to_column(var = "Markers")
    fis <- as.data.frame(result_stats) %>% select("Fis (W&C)", "Markers")
    fis <- column_to_rownames(fis, var = "Markers")
    missing_data_transposed <- t(as.data.frame(missing_data))
    missing_data_transposed_total <- as.data.frame(missing_data_transposed) %>% select("Total")
    missing_data_transposed_total <- subset(missing_data_transposed_total, !rownames(missing_data_transposed_total) %in% "Mean")
    colnames(missing_data_transposed_total) <- ("Missing %")
    fis_missing_merged <- merge(fis, missing_data_transposed_total, by = "row.names", all.x = TRUE) %>% column_to_rownames("Row.names")
    
    plot_FIS <- ggplot(fis_missing_merged, aes(x = `Missing %`, y = `Fis (W&C)`)) +
      geom_point(color = "#2C3E50") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
      labs(title = "Influence of Missing Data on Fis (Wright's Inbreeding Coefficient)",
           x = "Percentage of Missing Genotypes",
           y = "Fis (Wright's Inbreeding Coefficient)") +
      theme_ipsum()
    
    # Define the output$plot_output to render the plot
    output$plot_output <- renderPlot({
      plot_FIS
    })
    # Downloadable png of the selected plot ----
    output$download_plot_png <- downloadHandler(
      filename = function() {
        paste("plot_FIS_", Sys.Date(),  ".png")
      },
      content = function(file) {
        ggsave(filename = file, plot = plot_FIS, dpi = 300)
      }
    )
  })
  
  # Bootstrap on Loci Analysis
  observeEvent(input$run_boot_loci, {
    withProgress(message = 'Running Bootstrap on Loci', value = 0, {
      
      mydata_hierfstat_a <- mydata_hierfstat_reac()
      n_boot <- input$boot_loci_n
      n_loci <- ncol(mydata_hierfstat_a) - 1
      n_pops <- length(unique(mydata_hierfstat_a[,1]))
      
      # Selected parameters
      params_selected <- c(
        "FIS_WC" = input$boot_fis_wc,
        "FST_WC" = input$boot_fst_wc,
        "FIT_WC" = input$boot_fit_wc,
        "HS_NEI" = input$boot_hs_nei,
        "HT_NEI" = input$boot_ht_nei
      )
      
      selected_params <- names(params_selected)[params_selected]
      
      if(length(selected_params) == 0) {
        showNotification("Please select at least one parameter to bootstrap", type = "warning")
        return()
      }
      
      # Initialize storage
      boot_results_by_pop <- list()
      boot_results_by_locus <- list()
      boot_results_overall <- list()
      
      for(param in selected_params) {
        boot_results_by_pop[[param]] <- array(NA, dim = c(n_boot, n_pops), 
                                              dimnames = list(NULL, unique(mydata_hierfstat_a[,1])))
        boot_results_by_locus[[param]] <- array(NA, dim = c(n_boot, n_loci),
                                                dimnames = list(NULL, colnames(mydata_hierfstat_a)[-1]))
        boot_results_overall[[param]] <- numeric(n_boot)
      }
      
      incProgress(0.1, detail = "Initializing bootstrap...")
      
      # Bootstrap procedure
      for(boot_i in 1:n_boot) {
        if(boot_i %% 500 == 0) {
          incProgress(0.8/n_boot * 500, detail = paste("Bootstrap", boot_i, "of", n_boot))
        }
        
        # Sample loci with replacement
        sampled_loci <- sample(2:ncol(mydata_hierfstat_a), n_loci, replace = TRUE)
        boot_data <- mydata_hierfstat_a[, c(1, sampled_loci)]
        
        tryCatch({
          # Calculate basic stats for this bootstrap sample
          boot_basic <- basic.stats(boot_data)
          
          # Calculate Weir & Cockerham stats
          boot_genind <- hierfstat2genind(boot_data)
          boot_loci_data <- as.loci(boot_genind)
          boot_wc <- Fst(boot_loci_data)
          
          # Extract results by population
          if("FIS_WC" %in% selected_params) {
            boot_results_by_pop[["FIS_WC"]][boot_i, ] <- boot_wc[, "Fis"]
          }
          if("FST_WC" %in% selected_params) {
            boot_results_by_pop[["FST_WC"]][boot_i, ] <- boot_wc[, "Fst"]
          }
          if("FIT_WC" %in% selected_params) {
            boot_results_by_pop[["FIT_WC"]][boot_i, ] <- boot_wc[, "Fit"]
          }
          if("HS_NEI" %in% selected_params) {
            boot_results_by_pop[["HS_NEI"]][boot_i, ] <- boot_basic$pop.freq$Hs
          }
          if("HT_NEI" %in% selected_params) {
            boot_results_by_pop[["HT_NEI"]][boot_i, ] <- boot_basic$pop.freq$Ht
          }
          
          # Extract results by locus
          if("FIS_WC" %in% selected_params) {
            boot_results_by_locus[["FIS_WC"]][boot_i, ] <- boot_basic$perloc$Fis
          }
          if("FST_WC" %in% selected_params) {
            boot_results_by_locus[["FST_WC"]][boot_i, ] <- boot_basic$perloc$Fst
          }
          if("FIT_WC" %in% selected_params) {
            boot_results_by_locus[["FIT_WC"]][boot_i, ] <- boot_basic$perloc$Fit
          }
          if("HS_NEI" %in% selected_params) {
            boot_results_by_locus[["HS_NEI"]][boot_i, ] <- boot_basic$perloc$Hs
          }
          if("HT_NEI" %in% selected_params) {
            boot_results_by_locus[["HT_NEI"]][boot_i, ] <- boot_basic$perloc$Ht
          }
          
          # Overall results (mean across populations/loci)
          if("FIS_WC" %in% selected_params) {
            boot_results_overall[["FIS_WC"]][boot_i] <- mean(boot_wc[, "Fis"], na.rm = TRUE)
          }
          if("FST_WC" %in% selected_params) {
            boot_results_overall[["FST_WC"]][boot_i] <- mean(boot_wc[, "Fst"], na.rm = TRUE)
          }
          if("FIT_WC" %in% selected_params) {
            boot_results_overall[["FIT_WC"]][boot_i] <- mean(boot_wc[, "Fit"], na.rm = TRUE)
          }
          if("HS_NEI" %in% selected_params) {
            boot_results_overall[["HS_NEI"]][boot_i] <- boot_basic$overall$Hs
          }
          if("HT_NEI" %in% selected_params) {
            boot_results_overall[["HT_NEI"]][boot_i] <- boot_basic$overall$Ht
          }
          
        }, error = function(e) {
          # Skip this bootstrap iteration if error occurs
          warning(paste("Error in bootstrap", boot_i, ":", e$message))
        })
      }
      
      incProgress(0.1, detail = "Calculating confidence intervals...")
      
      # Calculate confidence intervals
      alpha_95 <- ifelse(input$ci_95, 0.05, NA)
      alpha_99 <- ifelse(input$ci_99, 0.01, NA)
      alphas <- c(alpha_95, alpha_99)[!is.na(c(alpha_95, alpha_99))]
      
      # Create summary tables
      summary_tables <- list()
      
      for(alpha in alphas) {
        ci_level <- paste0(100 * (1 - alpha), "%")
        lower_q <- alpha / 2
        upper_q <- 1 - alpha / 2
        
        # By Population
        by_pop_summary <- data.frame(
          Population = character(0),
          Parameter = character(0),
          Mean = numeric(0),
          Lower_CI = numeric(0),
          Upper_CI = numeric(0),
          stringsAsFactors = FALSE
        )
        
        for(param in selected_params) {
          for(pop in colnames(boot_results_by_pop[[param]])) {
            values <- boot_results_by_pop[[param]][, pop]
            values <- values[!is.na(values)]
            if(length(values) > 0) {
              by_pop_summary <- rbind(by_pop_summary, data.frame(
                Population = pop,
                Parameter = param,
                Mean = mean(values, na.rm = TRUE),
                Lower_CI = quantile(values, lower_q, na.rm = TRUE),
                Upper_CI = quantile(values, upper_q, na.rm = TRUE),
                CI_Level = ci_level,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # By Locus
        by_locus_summary <- data.frame(
          Locus = character(0),
          Parameter = character(0),
          Mean = numeric(0),
          Lower_CI = numeric(0),
          Upper_CI = numeric(0),
          stringsAsFactors = FALSE
        )
        
        # server_general_stats_part3.R
        # Continuation de la partie 2
        
        for(param in selected_params) {
          for(locus in colnames(boot_results_by_locus[[param]])) {
            values <- boot_results_by_locus[[param]][, locus]
            values <- values[!is.na(values)]
            if(length(values) > 0) {
              by_locus_summary <- rbind(by_locus_summary, data.frame(
                Locus = locus,
                Parameter = param,
                Mean = mean(values, na.rm = TRUE),
                Lower_CI = quantile(values, lower_q, na.rm = TRUE),
                Upper_CI = quantile(values, upper_q, na.rm = TRUE),
                CI_Level = ci_level,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # Overall
        overall_summary <- data.frame(
          Parameter = character(0),
          Mean = numeric(0),
          Lower_CI = numeric(0),
          Upper_CI = numeric(0),
          stringsAsFactors = FALSE
        )
        
        for(param in selected_params) {
          values <- boot_results_overall[[param]]
          values <- values[!is.na(values)]
          if(length(values) > 0) {
            overall_summary <- rbind(overall_summary, data.frame(
              Parameter = param,
              Mean = mean(values, na.rm = TRUE),
              Lower_CI = quantile(values, lower_q, na.rm = TRUE),
              Upper_CI = quantile(values, upper_q, na.rm = TRUE),
              CI_Level = ci_level,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        summary_tables[[ci_level]] <- list(
          by_pop = by_pop_summary,
          by_locus = by_locus_summary,
          overall = overall_summary
        )
      }
      
      boot_loci_results(summary_tables)
      
      # Update UI outputs for bootstrap on loci
      output$boot_loci_summary <- renderTable({
        req(boot_loci_results())
        first_ci <- names(boot_loci_results())[1]
        if(!is.null(first_ci)) {
          boot_loci_results()[[first_ci]]$overall
        }
      })
      
      output$boot_loci_by_pop <- renderTable({
        req(boot_loci_results())
        first_ci <- names(boot_loci_results())[1]
        if(!is.null(first_ci)) {
          boot_loci_results()[[first_ci]]$by_pop
        }
      })
      
      output$boot_loci_by_locus <- renderTable({
        req(boot_loci_results())
        first_ci <- names(boot_loci_results())[1]
        if(!is.null(first_ci)) {
          boot_loci_results()[[first_ci]]$by_locus
        }
      })
      
      output$boot_loci_overall <- renderTable({
        req(boot_loci_results())
        first_ci <- names(boot_loci_results())[1]
        if(!is.null(first_ci)) {
          boot_loci_results()[[first_ci]]$overall
        }
      })
      
    })
  })
  
  # Bootstrap on Populations Analysis
  observeEvent(input$run_boot_pop, {
    withProgress(message = 'Running Bootstrap on Populations', value = 0, {
      
      mydata_hierfstat_a <- mydata_hierfstat_reac()
      n_boot <- input$boot_pop_n
      n_loci <- ncol(mydata_hierfstat_a) - 1
      populations <- unique(mydata_hierfstat_a[,1])
      n_pops <- length(populations)
      
      # Selected parameters
      params_selected <- c(
        "FIS_WC" = input$boot_pop_fis_wc,
        "FST_WC" = input$boot_pop_fst_wc,
        "FIT_WC" = input$boot_pop_fit_wc,
        "HS_NEI" = input$boot_pop_hs_nei,
        "HT_NEI" = input$boot_pop_ht_nei
      )
      
      selected_params <- names(params_selected)[params_selected]
      
      if(length(selected_params) == 0) {
        showNotification("Please select at least one parameter to bootstrap", type = "warning")
        return()
      }
      
      # Initialize storage
      boot_results_by_locus <- list()
      boot_results_overall <- list()
      excluded_bootstraps <- 0
      
      for(param in selected_params) {
        boot_results_by_locus[[param]] <- array(NA, dim = c(n_boot, n_loci),
                                                dimnames = list(NULL, colnames(mydata_hierfstat_a)[-1]))
        boot_results_overall[[param]] <- numeric(n_boot)
      }
      
      incProgress(0.1, detail = "Initializing bootstrap...")
      
      # Bootstrap procedure
      valid_boots <- 0
      boot_attempts <- 0
      
      while(valid_boots < n_boot && boot_attempts < n_boot * 2) {
        boot_attempts <- boot_attempts + 1
        
        if(boot_attempts %% 500 == 0) {
          incProgress(0.8/n_boot * 500, detail = paste("Bootstrap attempt", boot_attempts, "Valid:", valid_boots))
        }
        
        # Sample populations with replacement
        sampled_pops <- sample(populations, n_pops, replace = TRUE)
        
        # Check if we have more than one unique population (to avoid FST division by zero)
        if(input$exclude_single_pop && length(unique(sampled_pops)) == 1) {
          excluded_bootstraps <- excluded_bootstraps + 1
          next
        }
        
        # Create bootstrap dataset
        boot_data <- data.frame()
        for(pop in sampled_pops) {
          pop_data <- mydata_hierfstat_a[mydata_hierfstat_a[,1] == pop, ]
          boot_data <- rbind(boot_data, pop_data)
        }
        
        tryCatch({
          # Calculate basic stats for this bootstrap sample
          boot_basic <- basic.stats(boot_data)
          
          # Calculate Weir & Cockerham stats
          boot_genind <- hierfstat2genind(boot_data)
          boot_loci_data <- as.loci(boot_genind)
          boot_wc <- Fst(boot_loci_data)
          
          valid_boots <- valid_boots + 1
          
          # Extract results by locus
          if("FIS_WC" %in% selected_params) {
            boot_results_by_locus[["FIS_WC"]][valid_boots, ] <- boot_basic$perloc$Fis
          }
          if("FST_WC" %in% selected_params) {
            boot_results_by_locus[["FST_WC"]][valid_boots, ] <- boot_basic$perloc$Fst
          }
          if("FIT_WC" %in% selected_params) {
            boot_results_by_locus[["FIT_WC"]][valid_boots, ] <- boot_basic$perloc$Fit
          }
          if("HS_NEI" %in% selected_params) {
            boot_results_by_locus[["HS_NEI"]][valid_boots, ] <- boot_basic$perloc$Hs
          }
          if("HT_NEI" %in% selected_params) {
            boot_results_by_locus[["HT_NEI"]][valid_boots, ] <- boot_basic$perloc$Ht
          }
          
          # Overall results
          if("FIS_WC" %in% selected_params) {
            boot_results_overall[["FIS_WC"]][valid_boots] <- mean(boot_basic$perloc$Fis, na.rm = TRUE)
          }
          if("FST_WC" %in% selected_params) {
            boot_results_overall[["FST_WC"]][valid_boots] <- mean(boot_basic$perloc$Fst, na.rm = TRUE)
          }
          if("FIT_WC" %in% selected_params) {
            boot_results_overall[["FIT_WC"]][valid_boots] <- mean(boot_basic$perloc$Fit, na.rm = TRUE)
          }
          if("HS_NEI" %in% selected_params) {
            boot_results_overall[["HS_NEI"]][valid_boots] <- boot_basic$overall$Hs
          }
          if("HT_NEI" %in% selected_params) {
            boot_results_overall[["HT_NEI"]][valid_boots] <- boot_basic$overall$Ht
          }
          
        }, error = function(e) {
          # Skip this bootstrap iteration if error occurs
          warning(paste("Error in bootstrap", boot_attempts, ":", e$message))
        })
      }
      
      incProgress(0.1, detail = "Calculating confidence intervals...")
      
      # Calculate confidence intervals for bootstrap on populations
      alpha_95 <- ifelse(input$boot_pop_ci_95, 0.05, NA)
      alpha_99 <- ifelse(input$boot_pop_ci_99, 0.01, NA)
      alphas <- c(alpha_95, alpha_99)[!is.na(c(alpha_95, alpha_99))]
      
      # Create summary tables
      summary_tables <- list()
      
      for(alpha in alphas) {
        ci_level <- paste0(100 * (1 - alpha), "%")
        lower_q <- alpha / 2
        upper_q <- 1 - alpha / 2
        
        # By Locus
        by_locus_summary <- data.frame(
          Locus = character(0),
          Parameter = character(0),
          Mean = numeric(0),
          Lower_CI = numeric(0),
          Upper_CI = numeric(0),
          stringsAsFactors = FALSE
        )
        
        for(param in selected_params) {
          for(locus in colnames(boot_results_by_locus[[param]])) {
            values <- boot_results_by_locus[[param]][1:valid_boots, locus]
            values <- values[!is.na(values)]
            if(length(values) > 0) {
              by_locus_summary <- rbind(by_locus_summary, data.frame(
                Locus = locus,
                Parameter = param,
                Mean = mean(values, na.rm = TRUE),
                Lower_CI = quantile(values, lower_q, na.rm = TRUE),
                Upper_CI = quantile(values, upper_q, na.rm = TRUE),
                CI_Level = ci_level,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # Overall
        overall_summary <- data.frame(
          Parameter = character(0),
          Mean = numeric(0),
          Lower_CI = numeric(0),
          Upper_CI = numeric(0),
          stringsAsFactors = FALSE
        )
        
        for(param in selected_params) {
          values <- boot_results_overall[[param]][1:valid_boots]
          values <- values[!is.na(values)]
          if(length(values) > 0) {
            overall_summary <- rbind(overall_summary, data.frame(
              Parameter = param,
              Mean = mean(values, na.rm = TRUE),
              Lower_CI = quantile(values, lower_q, na.rm = TRUE),
              Upper_CI = quantile(values, upper_q, na.rm = TRUE),
              CI_Level = ci_level,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        summary_tables[[ci_level]] <- list(
          by_locus = by_locus_summary,
          overall = overall_summary,
          excluded = excluded_bootstraps,
          valid_boots = valid_boots
        )
      }
      
      boot_pop_results(summary_tables)
      
      # Update UI outputs for bootstrap on populations
      output$boot_pop_summary <- renderTable({
        req(boot_pop_results())
        first_ci <- names(boot_pop_results())[1]
        if(!is.null(first_ci)) {
          boot_pop_results()[[first_ci]]$overall
        }
      })
      
      output$boot_pop_by_locus <- renderTable({
        req(boot_pop_results())
        first_ci <- names(boot_pop_results())[1]
        if(!is.null(first_ci)) {
          boot_pop_results()[[first_ci]]$by_locus
        }
      })
      
      output$boot_pop_overall <- renderTable({
        req(boot_pop_results())
        first_ci <- names(boot_pop_results())[1]
        if(!is.null(first_ci)) {
          boot_pop_results()[[first_ci]]$overall
        }
      })
      
      output$boot_pop_excluded_info <- renderText({
        req(boot_pop_results())
        first_ci <- names(boot_pop_results())[1]
        if(!is.null(first_ci)) {
          paste("Excluded bootstraps with single population:", 
                boot_pop_results()[[first_ci]]$excluded,
                "\nValid bootstraps completed:", 
                boot_pop_results()[[first_ci]]$valid_boots)
        }
      })
      
    })
  })
  
  # Local Panmixia Permutation Test
  observeEvent(input$run_perm_local, {
    withProgress(message = 'Running Local Panmixia Test', value = 0, {
      
      mydata_hierfstat_a <- mydata_hierfstat_reac()
      n_perm <- input$perm_local_n
      populations <- unique(mydata_hierfstat_a[,1])
      n_pops <- length(populations)
      loci_names <- colnames(mydata_hierfstat_a)[-1]
      n_loci <- length(loci_names)
      
      # Calculate observed FIS values
      observed_basic <- basic.stats(mydata_hierfstat_a)
      observed_fis_by_pop_locus <- observed_basic$Fis  # Matrix: populations x loci
      observed_fis_mean_by_pop <- rowMeans(observed_fis_by_pop_locus, na.rm = TRUE)
      observed_fis_mean_by_locus <- colMeans(observed_fis_by_pop_locus, na.rm = TRUE)
      observed_fis_overall <- mean(observed_fis_by_pop_locus, na.rm = TRUE)
      
      incProgress(0.1, detail = "Running permutations...")
      
      # Storage for permutation results
      perm_fis_by_pop_locus <- array(NA, dim = c(n_perm, n_pops, n_loci),
                                     dimnames = list(NULL, populations, loci_names))
      
      for(perm_i in 1:n_perm) {
        if(perm_i %% 1000 == 0) {
          incProgress(0.8/n_perm * 1000, detail = paste("Permutation", perm_i, "of", n_perm))
        }
        
        # Create permuted dataset
        perm_data <- mydata_hierfstat_a
        
        # For each population and locus, permute alleles among individuals
        for(pop in populations) {
          pop_indices <- which(mydata_hierfstat_a[,1] == pop)
          if(length(pop_indices) > 1) {
            for(locus_idx in 2:ncol(mydata_hierfstat_a)) {
              # Extract genotypes for this pop and locus
              genotypes <- mydata_hierfstat_a[pop_indices, locus_idx]
              
              # Extract individual alleles
              alleles <- c()
              for(gt in genotypes) {
                if(!is.na(gt) && gt != 0) {
                  # Split genotype into alleles (assuming 3-digit format)
                  allele1 <- floor(gt/1000)
                  allele2 <- gt %% 1000
                  if(allele1 > 0 && allele2 > 0) {
                    alleles <- c(alleles, allele1, allele2)
                  }
                }
              }
              
              # Randomly reassign alleles to form new genotypes
              if(length(alleles) >= 2) {
                shuffled_alleles <- sample(alleles)
                
                # Reform genotypes
                new_genotypes <- rep(NA, length(genotypes))
                allele_idx <- 1
                for(ind_idx in seq_along(genotypes)) {
                  if(!is.na(genotypes[ind_idx]) && genotypes[ind_idx] != 0) {
                    if(allele_idx < length(shuffled_alleles)) {
                      a1 <- shuffled_alleles[allele_idx]
                      a2 <- shuffled_alleles[allele_idx + 1]
                      new_genotypes[ind_idx] <- a1 * 1000 + a2
                      allele_idx <- allele_idx + 2
                    }
                  }
                }
                perm_data[pop_indices, locus_idx] <- new_genotypes
              }
            }
          }
        }
        
        tryCatch({
          # Calculate FIS for permuted data
          perm_basic <- basic.stats(perm_data)
          perm_fis_by_pop_locus[perm_i, , ] <- perm_basic$Fis
        }, error = function(e) {
          warning(paste("Error in permutation", perm_i, ":", e$message))
        })
      }
      
      incProgress(0.1, detail = "Calculating p-values...")
      
      # Calculate p-values
      pvalues_by_pop_locus <- array(NA, dim = c(n_pops, n_loci),
                                    dimnames = list(populations, loci_names))
      pvalues_mean_by_pop <- numeric(n_pops)
      names(pvalues_mean_by_pop) <- populations
      pvalues_mean_by_locus <- numeric(n_loci)
      names(pvalues_mean_by_locus) <- loci_names
      pvalue_overall <- NA
      
      # P-values by population and locus
      for(pop_idx in 1:n_pops) {
        for(locus_idx in 1:n_loci) {
          obs_val <- observed_fis_by_pop_locus[pop_idx, locus_idx]
          perm_vals <- perm_fis_by_pop_locus[, pop_idx, locus_idx]
          perm_vals <- perm_vals[!is.na(perm_vals)]
          
          if(!is.na(obs_val) && length(perm_vals) > 0) {
            p_plus <- sum(perm_vals >= obs_val) / length(perm_vals)
            p_minus <- sum(perm_vals <= obs_val) / length(perm_vals)
            pvalues_by_pop_locus[pop_idx, locus_idx] <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
          }
        }
      }
      
      # P-values mean by population
      for(pop_idx in 1:n_pops) {
        obs_val <- observed_fis_mean_by_pop[pop_idx]
        perm_vals <- rowMeans(perm_fis_by_pop_locus[, pop_idx, ], na.rm = TRUE)
        perm_vals <- perm_vals[!is.na(perm_vals)]
        
        if(!is.na(obs_val) && length(perm_vals) > 0) {
          p_plus <- sum(perm_vals >= obs_val) / length(perm_vals)
          p_minus <- sum(perm_vals <= obs_val) / length(perm_vals)
          pvalues_mean_by_pop[pop_idx] <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
        }
      }
      
      # P-values mean by locus
      for(locus_idx in 1:n_loci) {
        obs_val <- observed_fis_mean_by_locus[locus_idx]
        perm_vals <- colMeans(perm_fis_by_pop_locus[, , locus_idx], na.rm = TRUE)
        perm_vals <- perm_vals[!is.na(perm_vals)]
        
        if(!is.na(obs_val) && length(perm_vals) > 0) {
          p_plus <- sum(perm_vals >= obs_val) / length(perm_vals)
          p_minus <- sum(perm_vals <= obs_val) / length(perm_vals)
          pvalues_mean_by_locus[locus_idx] <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
        }
      }
      
      # Overall p-value
      perm_overall_vals <- apply(perm_fis_by_pop_locus, 1, mean, na.rm = TRUE)
      perm_overall_vals <- perm_overall_vals[!is.na(perm_overall_vals)]
      
      if(!is.na(observed_fis_overall) && length(perm_overall_vals) > 0) {
        p_plus <- sum(perm_overall_vals >= observed_fis_overall) / length(perm_overall_vals)
        p_minus <- sum(perm_overall_vals <= observed_fis_overall) / length(perm_overall_vals)
        pvalue_overall <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
      }
      
      # Prepare results
      results <- list(
        pvalues_by_pop_locus = pvalues_by_pop_locus,
        pvalues_mean_by_pop = pvalues_mean_by_pop,
        pvalues_mean_by_locus = pvalues_mean_by_locus,
        pvalue_overall = pvalue_overall,
        observed_fis_by_pop_locus = observed_fis_by_pop_locus,
        observed_fis_mean_by_pop = observed_fis_mean_by_pop,
        observed_fis_mean_by_locus = observed_fis_mean_by_locus,
        observed_fis_overall = observed_fis_overall
      )
      
      perm_local_results(results)
      
    })
  })
  
  # server_general_stats_part4.R
  # Continuation de la partie 3 et fin du serveur
  
  # Global Panmixia Permutation Test
  observeEvent(input$run_perm_global, {
    withProgress(message = 'Running Global Panmixia Test', value = 0, {
      
      mydata_hierfstat_a <- mydata_hierfstat_reac()
      n_perm <- input$perm_global_n
      loci_names <- colnames(mydata_hierfstat_a)[-1]
      n_loci <- length(loci_names)
      
      # Calculate observed FIT values
      observed_basic <- basic.stats(mydata_hierfstat_a)
      observed_fit_by_locus <- observed_basic$perloc$Fit
      observed_fit_overall <- mean(observed_fit_by_locus, na.rm = TRUE)
      
      incProgress(0.1, detail = "Running permutations...")
      
      # Storage for permutation results
      perm_fit_by_locus <- array(NA, dim = c(n_perm, n_loci),
                                 dimnames = list(NULL, loci_names))
      perm_fit_overall <- numeric(n_perm)
      
      for(perm_i in 1:n_perm) {
        if(perm_i %% 1000 == 0) {
          incProgress(0.8/n_perm * 1000, detail = paste("Permutation", perm_i, "of", n_perm))
        }
        
        # Create permuted dataset
        perm_data <- mydata_hierfstat_a
        
        # For each locus, permute alleles among ALL individuals
        for(locus_idx in 2:ncol(mydata_hierfstat_a)) {
          # Extract all genotypes for this locus
          genotypes <- mydata_hierfstat_a[, locus_idx]
          
          # Extract individual alleles
          alleles <- c()
          valid_indices <- c()
          for(i in seq_along(genotypes)) {
            gt <- genotypes[i]
            if(!is.na(gt) && gt != 0) {
              # Split genotype into alleles (assuming 3-digit format)
              allele1 <- floor(gt/1000)
              allele2 <- gt %% 1000
              if(allele1 > 0 && allele2 > 0) {
                alleles <- c(alleles, allele1, allele2)
                valid_indices <- c(valid_indices, i)
              }
            }
          }
          
          # Randomly reassign alleles to form new genotypes
          if(length(alleles) >= 2) {
            shuffled_alleles <- sample(alleles)
            
            # Reform genotypes
            new_genotypes <- genotypes
            allele_idx <- 1
            for(ind_idx in valid_indices) {
              if(allele_idx < length(shuffled_alleles)) {
                a1 <- shuffled_alleles[allele_idx]
                a2 <- shuffled_alleles[allele_idx + 1]
                new_genotypes[ind_idx] <- a1 * 1000 + a2
                allele_idx <- allele_idx + 2
              }
            }
            perm_data[, locus_idx] <- new_genotypes
          }
        }
        
        tryCatch({
          # Calculate FIT for permuted data
          perm_basic <- basic.stats(perm_data)
          perm_fit_by_locus[perm_i, ] <- perm_basic$perloc$Fit
          perm_fit_overall[perm_i] <- mean(perm_basic$perloc$Fit, na.rm = TRUE)
        }, error = function(e) {
          warning(paste("Error in permutation", perm_i, ":", e$message))
        })
      }
      
      incProgress(0.1, detail = "Calculating p-values...")
      
      # Calculate p-values
      pvalues_by_locus <- numeric(n_loci)
      names(pvalues_by_locus) <- loci_names
      pvalue_overall <- NA
      
      # P-values by locus
      for(locus_idx in 1:n_loci) {
        obs_val <- observed_fit_by_locus[locus_idx]
        perm_vals <- perm_fit_by_locus[, locus_idx]
        perm_vals <- perm_vals[!is.na(perm_vals)]
        
        if(!is.na(obs_val) && length(perm_vals) > 0) {
          p_plus <- sum(perm_vals >= obs_val) / length(perm_vals)
          p_minus <- sum(perm_vals <= obs_val) / length(perm_vals)
          pvalues_by_locus[locus_idx] <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
        }
      }
      
      # Overall p-value
      perm_overall_vals <- perm_fit_overall[!is.na(perm_fit_overall)]
      if(!is.na(observed_fit_overall) && length(perm_overall_vals) > 0) {
        p_plus <- sum(perm_overall_vals >= observed_fit_overall) / length(perm_overall_vals)
        p_minus <- sum(perm_overall_vals <= observed_fit_overall) / length(perm_overall_vals)
        pvalue_overall <- min(p_plus, p_minus) + 1 - max(p_plus, p_minus)
      }
      
      # Prepare results
      results <- list(
        pvalues_by_locus = pvalues_by_locus,
        pvalue_overall = pvalue_overall,
        observed_fit_by_locus = observed_fit_by_locus,
        observed_fit_overall = observed_fit_overall,
        perm_fit_by_locus = perm_fit_by_locus,
        perm_fit_overall = perm_fit_overall
      )
      
      perm_global_results(results)
      
    })
  })
  
  # Population Subdivision Permutation Test
  observeEvent(input$run_perm_subdiv, {
    withProgress(message = 'Running Population Subdivision Test', value = 0, {
      
      mydata_hierfstat_a <- mydata_hierfstat_reac()
      n_perm <- input$perm_subdiv_n
      populations <- unique(mydata_hierfstat_a[,1])
      n_pops <- length(populations)
      loci_names <- colnames(mydata_hierfstat_a)[-1]
      n_loci <- length(loci_names)
      
      incProgress(0.1, detail = "Calculating observed G-statistics...")
      
      # Calculate observed G-statistics
      observed_G_by_locus <- numeric(n_loci)
      names(observed_G_by_locus) <- loci_names
      contingency_tables <- list()
      
      for(locus_idx in 1:n_loci) {
        locus_name <- loci_names[locus_idx]
        locus_data <- mydata_hierfstat_a[, locus_idx + 1]
        
        # Create contingency table for this locus
        # Extract alleles for each population
        allele_counts <- data.frame()
        all_alleles <- c()
        
        # First, collect all unique alleles across populations
        for(pop in populations) {
          pop_indices <- which(mydata_hierfstat_a[,1] == pop)
          pop_genotypes <- locus_data[pop_indices]
          
          for(gt in pop_genotypes) {
            if(!is.na(gt) && gt != 0) {
              allele1 <- floor(gt/1000)
              allele2 <- gt %% 1000
              if(allele1 > 0) all_alleles <- c(all_alleles, allele1)
              if(allele2 > 0) all_alleles <- c(all_alleles, allele2)
            }
          }
        }
        
        unique_alleles <- sort(unique(all_alleles))
        
        # Create contingency table
        if(length(unique_alleles) > 1) {
          contingency_matrix <- matrix(0, nrow = n_pops, ncol = length(unique_alleles),
                                       dimnames = list(populations, as.character(unique_alleles)))
          
          for(pop_idx in 1:n_pops) {
            pop <- populations[pop_idx]
            pop_indices <- which(mydata_hierfstat_a[,1] == pop)
            pop_genotypes <- locus_data[pop_indices]
            
            for(gt in pop_genotypes) {
              if(!is.na(gt) && gt != 0) {
                allele1 <- floor(gt/1000)
                allele2 <- gt %% 1000
                if(allele1 > 0 && allele1 %in% unique_alleles) {
                  contingency_matrix[pop_idx, as.character(allele1)] <- 
                    contingency_matrix[pop_idx, as.character(allele1)] + 1
                }
                if(allele2 > 0 && allele2 %in% unique_alleles) {
                  contingency_matrix[pop_idx, as.character(allele2)] <- 
                    contingency_matrix[pop_idx, as.character(allele2)] + 1
                }
              }
            }
          }
          
          contingency_tables[[locus_name]] <- contingency_matrix
          
          # Calculate G-statistic (log-likelihood ratio)
          # G = 2 * sum(observed * log(observed/expected))
          row_totals <- rowSums(contingency_matrix)
          col_totals <- colSums(contingency_matrix)
          grand_total <- sum(contingency_matrix)
          
          G_stat <- 0
          if(grand_total > 0) {
            for(i in 1:nrow(contingency_matrix)) {
              for(j in 1:ncol(contingency_matrix)) {
                observed <- contingency_matrix[i, j]
                expected <- (row_totals[i] * col_totals[j]) / grand_total
                if(observed > 0 && expected > 0) {
                  G_stat <- G_stat + 2 * observed * log(observed / expected)
                }
              }
            }
          }
          
          observed_G_by_locus[locus_idx] <- G_stat
        } else {
          observed_G_by_locus[locus_idx] <- 0
          contingency_tables[[locus_name]] <- matrix(0, nrow = n_pops, ncol = 1,
                                                     dimnames = list(populations, "No_variation"))
        }
      }
      
      observed_G_total <- sum(observed_G_by_locus, na.rm = TRUE)
      
      incProgress(0.1, detail = "Running permutations...")
      
      # Storage for permutation results
      perm_G_by_locus <- array(NA, dim = c(n_perm, n_loci),
                               dimnames = list(NULL, loci_names))
      perm_G_total <- numeric(n_perm)
      
      for(perm_i in 1:n_perm) {
        if(perm_i %% 1000 == 0) {
          incProgress(0.7/n_perm * 1000, detail = paste("Permutation", perm_i, "of", n_perm))
        }
        
        # Permute complete individuals among populations
        perm_data <- mydata_hierfstat_a
        perm_pops <- sample(mydata_hierfstat_a[,1])
        perm_data[,1] <- perm_pops
        
        # Calculate G-statistics for permuted data
        perm_G_locus <- numeric(n_loci)
        
        for(locus_idx in 1:n_loci) {
          locus_data <- perm_data[, locus_idx + 1]
          
          # Create contingency table for permuted data
          all_alleles <- c()
          for(pop in populations) {
            pop_indices <- which(perm_data[,1] == pop)
            pop_genotypes <- locus_data[pop_indices]
            
            for(gt in pop_genotypes) {
              if(!is.na(gt) && gt != 0) {
                allele1 <- floor(gt/1000)
                allele2 <- gt %% 1000
                if(allele1 > 0) all_alleles <- c(all_alleles, allele1)
                if(allele2 > 0) all_alleles <- c(all_alleles, allele2)
              }
            }
          }
          
          unique_alleles <- sort(unique(all_alleles))
          
          if(length(unique_alleles) > 1) {
            contingency_matrix <- matrix(0, nrow = n_pops, ncol = length(unique_alleles),
                                         dimnames = list(populations, as.character(unique_alleles)))
            
            for(pop_idx in 1:n_pops) {
              pop <- populations[pop_idx]
              pop_indices <- which(perm_data[,1] == pop)
              pop_genotypes <- locus_data[pop_indices]
              
              for(gt in pop_genotypes) {
                if(!is.na(gt) && gt != 0) {
                  allele1 <- floor(gt/1000)
                  allele2 <- gt %% 1000
                  if(allele1 > 0 && allele1 %in% unique_alleles) {
                    contingency_matrix[pop_idx, as.character(allele1)] <- 
                      contingency_matrix[pop_idx, as.character(allele1)] + 1
                  }
                  if(allele2 > 0 && allele2 %in% unique_alleles) {
                    contingency_matrix[pop_idx, as.character(allele2)] <- 
                      contingency_matrix[pop_idx, as.character(allele2)] + 1
                  }
                }
              }
            }
            
            # Calculate G-statistic for permuted data
            row_totals <- rowSums(contingency_matrix)
            col_totals <- colSums(contingency_matrix)
            grand_total <- sum(contingency_matrix)
            
            G_stat <- 0
            if(grand_total > 0) {
              for(i in 1:nrow(contingency_matrix)) {
                for(j in 1:ncol(contingency_matrix)) {
                  observed <- contingency_matrix[i, j]
                  expected <- (row_totals[i] * col_totals[j]) / grand_total
                  if(observed > 0 && expected > 0) {
                    G_stat <- G_stat + 2 * observed * log(observed / expected)
                  }
                }
              }
            }
            
            perm_G_locus[locus_idx] <- G_stat
          } else {
            perm_G_locus[locus_idx] <- 0
          }
        }
        
        perm_G_by_locus[perm_i, ] <- perm_G_locus
        perm_G_total[perm_i] <- sum(perm_G_locus, na.rm = TRUE)
      }
      
      incProgress(0.1, detail = "Calculating p-values...")
      
      # Calculate p-values
      pvalues_by_locus <- numeric(n_loci)
      names(pvalues_by_locus) <- loci_names
      pvalue_total <- NA
      
      # P-values by locus
      for(locus_idx in 1:n_loci) {
        obs_val <- observed_G_by_locus[locus_idx]
        perm_vals <- perm_G_by_locus[, locus_idx]
        perm_vals <- perm_vals[!is.na(perm_vals)]
        
        if(!is.na(obs_val) && length(perm_vals) > 0) {
          pvalues_by_locus[locus_idx] <- sum(perm_vals >= obs_val) / length(perm_vals)
        }
      }
      
      # Total p-value
      perm_total_vals <- perm_G_total[!is.na(perm_G_total)]
      if(!is.na(observed_G_total) && length(perm_total_vals) > 0) {
        pvalue_total <- sum(perm_total_vals >= observed_G_total) / length(perm_total_vals)
      }
      
      # Prepare results
      results <- list(
        pvalues_by_locus = pvalues_by_locus,
        pvalue_total = pvalue_total,
        observed_G_by_locus = observed_G_by_locus,
        observed_G_total = observed_G_total,
        contingency_tables = contingency_tables,
        perm_G_by_locus = perm_G_by_locus,
        perm_G_total = perm_G_total
      )
      
      perm_subdiv_results(results)
      
    })
  })
  
  # UI outputs for all permutation tests
  
  # Local Panmixia UI outputs
  output$perm_local_by_pop_locus <- renderTable({
    req(perm_local_results())
    results <- perm_local_results()
    
    # Convert matrix to data frame for display
    pval_df <- as.data.frame(results$pvalues_by_pop_locus)
    pval_df$Population <- rownames(pval_df)
    pval_df <- pval_df[, c("Population", colnames(pval_df)[1:(ncol(pval_df)-1)])]
    pval_df
  }, rownames = FALSE)
  
  output$perm_local_by_pop <- renderTable({
    req(perm_local_results())
    results <- perm_local_results()
    
    data.frame(
      Population = names(results$pvalues_mean_by_pop),
      Observed_FIS = results$observed_fis_mean_by_pop,
      P_value = results$pvalues_mean_by_pop
    )
  }, rownames = FALSE)
  
  output$perm_local_by_locus <- renderTable({
    req(perm_local_results())
    results <- perm_local_results()
    
    data.frame(
      Locus = names(results$pvalues_mean_by_locus),
      Observed_FIS = results$observed_fis_mean_by_locus,
      P_value = results$pvalues_mean_by_locus
    )
  }, rownames = FALSE)
  
  output$perm_local_overall <- renderTable({
    req(perm_local_results())
    results <- perm_local_results()
    
    data.frame(
      Statistic = "Overall FIS",
      Observed_Value = results$observed_fis_overall,
      P_value = results$pvalue_overall
    )
  }, rownames = FALSE)
  
  output$perm_local_significant <- renderTable({
    req(perm_local_results())
    results <- perm_local_results()
    
    # Find significant results (p < 0.05)
    sig_results <- data.frame(
      Test = character(0),
      Population = character(0),
      Locus = character(0),
      Observed_FIS = numeric(0),
      P_value = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Check by population and locus
    for(i in 1:nrow(results$pvalues_by_pop_locus)) {
      for(j in 1:ncol(results$pvalues_by_pop_locus)) {
        pval <- results$pvalues_by_pop_locus[i,j]
        if(!is.na(pval) && pval < 0.05) {
          sig_results <- rbind(sig_results, data.frame(
            Test = "By Pop & Locus",
            Population = rownames(results$pvalues_by_pop_locus)[i],
            Locus = colnames(results$pvalues_by_pop_locus)[j],
            Observed_FIS = results$observed_fis_by_pop_locus[i,j],
            P_value = pval,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Check other significant results...
    if(nrow(sig_results) == 0) {
      data.frame(Message = "No significant results at α = 0.05")
    } else {
      sig_results
    }
  }, rownames = FALSE)
  
  # Global Panmixia UI outputs
  output$perm_global_by_locus <- renderTable({
    req(perm_global_results())
    results <- perm_global_results()
    
    data.frame(
      Locus = names(results$pvalues_by_locus),
      Observed_FIT = results$observed_fit_by_locus,
      P_value = results$pvalues_by_locus
    )
  }, rownames = FALSE)
  
  output$perm_global_overall <- renderTable({
    req(perm_global_results())
    results <- perm_global_results()
    
    data.frame(
      Statistic = "Overall FIT",
      Observed_Value = results$observed_fit_overall,
      P_value = results$pvalue_overall
    )
  }, rownames = FALSE)
  
  output$perm_global_significant <- renderTable({
    req(perm_global_results())
    results <- perm_global_results()
    
    sig_results <- data.frame(
      Test = character(0),
      Locus = character(0),
      Observed_FIT = numeric(0),
      P_value = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Check by locus
    for(i in seq_along(results$pvalues_by_locus)) {
      pval <- results$pvalues_by_locus[i]
      if(!is.na(pval) && pval < 0.05) {
        sig_results <- rbind(sig_results, data.frame(
          Test = "By Locus",
          Locus = names(results$pvalues_by_locus)[i],
          Observed_FIT = results$observed_fit_by_locus[i],
          P_value = pval,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Check overall
    if(!is.na(results$pvalue_overall) && results$pvalue_overall < 0.05) {
      sig_results <- rbind(sig_results, data.frame(
        Test = "Overall",
        Locus = "All loci",
        Observed_FIT = results$observed_fit_overall,
        P_value = results$pvalue_overall,
        stringsAsFactors = FALSE
      ))
    }
    
    if(nrow(sig_results) == 0) {
      data.frame(Message = "No significant results at α = 0.05")
    } else {
      sig_results
    }
  }, rownames = FALSE)
  
  # Subdivision UI outputs
  output$perm_subdiv_results <- renderTable({
    req(perm_subdiv_results())
    results <- perm_subdiv_results()
    
    data.frame(
      Locus = names(results$pvalues_by_locus),
      Observed_G = results$observed_G_by_locus,
      P_value = results$pvalues_by_locus
    )
  }, rownames = FALSE)
  
  output$perm_subdiv_overall <- renderTable({
    req(perm_subdiv_results())
    results <- perm_subdiv_results()
    
    data.frame(
      Statistic = "Total G",
      Observed_Value = results$observed_G_total,
      P_value = results$pvalue_total
    )
  }, rownames = FALSE)
  
  output$perm_subdiv_significant <- renderTable({
    req(perm_subdiv_results())
    results <- perm_subdiv_results()
    
    sig_results <- data.frame(
      Test = character(0),
      Locus = character(0),
      Observed_G = numeric(0),
      P_value = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Check by locus
    for(i in seq_along(results$pvalues_by_locus)) {
      pval <- results$pvalues_by_locus[i]
      if(!is.na(pval) && pval < 0.05) {
        sig_results <- rbind(sig_results, data.frame(
          Test = "By Locus",
          Locus = names(results$pvalues_by_locus)[i],
          Observed_G = results$observed_G_by_locus[i],
          P_value = pval,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Check overall
    if(!is.na(results$pvalue_total) && results$pvalue_total < 0.05) {
      sig_results <- rbind(sig_results, data.frame(
        Test = "Overall",
        Locus = "All loci",
        Observed_G = results$observed_G_total,
        P_value = results$pvalue_total,
        stringsAsFactors = FALSE
      ))
    }
    
    if(nrow(sig_results) == 0) {
      data.frame(Message = "No significant results at α = 0.05")
    } else {
      sig_results
    }
  }, rownames = FALSE)
  
  # Download handlers for all analyses
  
  # Bootstrap downloads
  output$download_boot_loci_csv <- downloadHandler(
    filename = function() {
      paste0("bootstrap_loci_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(boot_loci_results())
      first_ci <- names(boot_loci_results())[1]
      if(!is.null(first_ci)) {
        write.csv(boot_loci_results()[[first_ci]]$overall, file, row.names = FALSE)
      }
    }
  )
  
  output$download_boot_pop_csv <- downloadHandler(
    filename = function() {
      paste0("bootstrap_populations_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(boot_pop_results())
      first_ci <- names(boot_pop_results())[1]
      if(!is.null(first_ci)) {
        write.csv(boot_pop_results()[[first_ci]]$overall, file, row.names = FALSE)
      }
    }
  )
  
  # Permutation downloads
  output$download_perm_local_csv <- downloadHandler(
    filename = function() {
      paste0("permutation_local_panmixia_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(perm_local_results())
      results <- perm_local_results()
      pval_df <- as.data.frame(results$pvalues_by_pop_locus)
      pval_df$Population <- rownames(pval_df)
      write.csv(pval_df, file, row.names = FALSE)
    }
  )
  
  output$download_perm_global_csv <- downloadHandler(
    filename = function() {
      paste0("permutation_global_panmixia_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(perm_global_results())
      results <- perm_global_results()
      result_df <- data.frame(
        Locus = names(results$pvalues_by_locus),
        Observed_FIT = results$observed_fit_by_locus,
        P_value = results$pvalues_by_locus
      )
      write.csv(result_df, file, row.names = FALSE)
    }
  )
  
  output$download_perm_subdiv_csv <- downloadHandler(
    filename = function() {
      paste0("permutation_subdivision_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(perm_subdiv_results())
      results <- perm_subdiv_results()
      result_df <- data.frame(
        Locus = names(results$pvalues_by_locus),
        Observed_G = results$observed_G_by_locus,
        P_value = results$pvalues_by_locus
      )
      write.csv(result_df, file, row.names = FALSE)
    }
  )
  
  # Summary UI outputs
  output$summary_basic_stats <- renderTable({
    req(result_stats_download())
    result_stats_download()
  })
  
  output$summary_bootstrap <- renderTable({
    boot_loci <- boot_loci_results()
    boot_pop <- boot_pop_results()
    
    summary_df <- data.frame(
      Analysis = character(0),
      Parameter = character(0),
      Mean = numeric(0),
      CI_95_Lower = numeric(0),
      CI_95_Upper = numeric(0),
      stringsAsFactors = FALSE
    )
    
    if(!is.null(boot_loci) && "95%" %in% names(boot_loci)) {
      for(i in 1:nrow(boot_loci$"95%"$overall)) {
        summary_df <- rbind(summary_df, data.frame(
          Analysis = "Bootstrap on Loci",
          Parameter = boot_loci$"95%"$overall$Parameter[i],
          Mean = boot_loci$"95%"$overall$Mean[i],
          CI_95_Lower = boot_loci$"95%"$overall$Lower_CI[i],
          CI_95_Upper = boot_loci$"95%"$overall$Upper_CI[i],
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if(!is.null(boot_pop) && "95%" %in% names(boot_pop)) {
      for(i in 1:nrow(boot_pop$"95%"$overall)) {
        summary_df <- rbind(summary_df, data.frame(
          Analysis = "Bootstrap on Populations",
          Parameter = boot_pop$"95%"$overall$Parameter[i],
          Mean = boot_pop$"95%"$overall$Mean[i],
          CI_95_Lower = boot_pop$"95%"$overall$Lower_CI[i],
          CI_95_Upper = boot_pop$"95%"$overall$Upper_CI[i],
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if(nrow(summary_df) == 0) {
      data.frame(Message = "No bootstrap results available")
    } else {
      summary_df
    }
  })
  
  output$summary_permutation <- renderTable({
    local_res <- perm_local_results()
    global_res <- perm_global_results()
    subdiv_res <- perm_subdiv_results()
    
    summary_df <- data.frame(
      Test = character(0),
      Statistic = character(0),
      P_value = numeric(0),
      Significant = character(0),
      stringsAsFactors = FALSE
    )
    
    if(!is.null(local_res)) {
      summary_df <- rbind(summary_df, data.frame(
        Test = "Local Panmixia",
        Statistic = "Overall FIS",
        P_value = local_res$pvalue_overall,
        Significant = ifelse(!is.na(local_res$pvalue_overall) && local_res$pvalue_overall < 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      ))
    }
    
    if(!is.null(global_res)) {
      summary_df <- rbind(summary_df, data.frame(
        Test = "Global Panmixia",
        Statistic = "Overall FIT",
        P_value = global_res$pvalue_overall,
        Significant = ifelse(!is.na(global_res$pvalue_overall) && global_res$pvalue_overall < 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      ))
    }
    
    if(!is.null(subdiv_res)) {
      summary_df <- rbind(summary_df, data.frame(
        Test = "Population Subdivision",
        Statistic = "Total G",
        P_value = subdiv_res$pvalue_total,
        Significant = ifelse(!is.na(subdiv_res$pvalue_total) && subdiv_res$pvalue_total < 0.05, "Yes", "No"),
        stringsAsFactors = FALSE
      ))
    }
    
    if(nrow(summary_df) == 0) {
      data.frame(Message = "No permutation results available")
    } else {
      summary_df
    }
  })
}