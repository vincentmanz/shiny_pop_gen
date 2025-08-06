# server_general_stats.R

# environment variables
filtered_data <- read.csv("data/data-2023-09-11.csv", header = TRUE)
# Chemin metadata

# information inherited from previous page
n_marker <- 6
n_pop <- 8
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
  panmixia_boot_download <- reactiveVal(NULL)

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

  ## Create genind object
  filtered_data <- data.frame(indv = paste(substr(filtered_data$Population, 1, 3), row.names(filtered_data), sep = "."), filtered_data)
  filtered_data_reac(filtered_data)

  # Compute genind object
  mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end], sep = "/", ncode = 6, ind.names = filtered_data$indv, pop = filtered_data$Population, NA.char = "NA", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL)
  mydata_genind_reac(mydata_genind)

  # Compute hierfstat object
  mydata_hierfstat <- genind2hierfstat(mydata_genind)
  mydata_hierfstat_reac(mydata_hierfstat)

  # create the missing df
  mydata_genind <- df2genind(X = filtered_data[, column_genotype_start:column_genotype_end], sep = "/", ncode = 6, ind.names = filtered_data$indv, pop = filtered_data$Population, NA.char = "0/0", ploidy = 2, type = "codom", strata = NULL, hierarchy = NULL)
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

    # Compute FST-max if requested
    if(input$fst_max_checkbox) {
      tryCatch({
        # Recode alleles for FST-max
        mydata_hierfstat_recoded <- recode_for_fstmax(mydata_hierfstat_a)
        
        # Calculate basic stats on recoded data
        result_fstmax <- basic.stats(mydata_hierfstat_recoded)
        
        # Add FST-max to results
        df_result_basic$`Fst-max` <- result_fstmax$perloc$Fst
        
        # Verify FST-max > FST standard
        if(any(df_result_basic$`Fst-max` < df_result_basic$`Fst (W&C)`, na.rm = TRUE)) {
          showNotification(
            "Warning: Some FST-max values are lower than FST (W&C). Check allele recoding.",
            type = "warning"
          )
        }
      }, error = function(e) {
        showNotification(paste("Error in FST-max calculation:", e$message), type = "error")
        df_result_basic$`Fst-max` <- NA
      })
    }

    # Compute Fst' (Fst/Fst-max) if requested
    if(input$fst_prim_checkbox) {
      # Check if required columns exist
      required_cols <- c("Fst (W&C)", "Fst-max")
      missing_cols <- setdiff(required_cols, colnames(df_result_basic))
  
      if(length(missing_cols) > 0) {
        showNotification(paste("Required columns missing for Fst' calculation:", paste(missing_cols, collapse = ", "), "Please calculate these statistics first."), type = "error")
        df_result_basic$`Fst'` <- NA
      } else if(all(is.na(df_result_basic$`Fst-max`))) {
        showNotification("Fst-max values are all NA. Please check Fst-max calculation.", type = "error")
        df_result_basic$`Fst'` <- NA
      } else {
        tryCatch({
          # Calculate Fst' = Fst / Fst-max
          df_result_basic$`Fst'` <- df_result_basic$`Fst (W&C)` / df_result_basic$`Fst-max`
      
          # Ensure values are between 0 and 1
          df_result_basic$`Fst'` <- pmin(pmax(df_result_basic$`Fst'`, 0, na.rm = TRUE), 1, na.rm = TRUE)
      
          # Check for potential issues
          if(any(df_result_basic$`Fst'` > 1.1, na.rm = TRUE)) {
            showNotification("Warning: Some Fst' values are >1.1. Check Fst and Fst-max calculations.", type = "warning")
          }
      
          # Additional checks
          issues <- list()
          if(any(df_result_basic$`Fst'` < 0, na.rm = TRUE)) {
            issues <- c(issues, "negative values")
          }
          if(any(is.nan(df_result_basic$`Fst'`), na.rm = TRUE)) {
            issues <- c(issues, "NaN values")
          }
          if(any(is.infinite(df_result_basic$`Fst'`), na.rm = TRUE)) {
            issues <- c(issues, "infinite values")
          }
      
          if(length(issues) > 0) {
            showNotification(paste("Potential issues detected:", paste(issues, collapse = ", ")), type = "warning")
            # Replace problematic values with NA
            df_result_basic$`Fst'`[df_result_basic$`Fst'` < 0 | is.nan(df_result_basic$`Fst'`) | is.infinite(df_result_basic$`Fst'`)] <- NA
          }
        }, error = function(e) {
          showNotification(paste("Error in Fst' calculation:", e$message), type = "error")
          df_result_basic$`Fst'` <- NA
        })
      }
    }

    # Merge all results
    result_stats <- merge(result_f_stats, df_result_basic, by = "row.names", all.x = TRUE)
    colnames(result_stats)[11:12] <- c("Fst (Nei)", "Fis (Nei)")
    rownames(result_stats) <- result_stats[, 1]
    result_stats <- result_stats[, -1]
    result_stats_reactive(result_stats)

    # Prepare final table for display
    result_stats_select <- result_stats[, names(selected_stats)[selected_stats], drop = FALSE]
    result_stats_select$Locus <- rownames(result_stats_select)
    result_stats_select <- result_stats_select[, c("Locus", names(result_stats_select)[names(result_stats_select) != "Locus"])]

    result_stats_download(result_stats_select)

    output$basic_stats_result <- renderTable({
      req(result_stats_download())
      result_stats_download()
    })

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

  # Basic stats download
  output$download_gstats_csv <- downloadHandler(
    filename = function() {
      paste0("basic_stats_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(result_stats_download())
      write.csv(result_stats_download(), file, row.names = FALSE)
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

  # Observe Event for Running Panmixia Analysis
  observeEvent(input$run_panmixia, {
    # retrieve reactive values
    result_stats <- result_stats_reactive()
    filtered_data_indv <- filtered_data_reac()

    # Access the input values
    n_rep <- input$numboot

    #level
    level1 <- input$level1
    # Variable dynamique Level1, 2 ou 3 selectionner par le user / par defaut pop

    # bootstrap over level1, Columns to include in the botraping
    columns_to_fstat <- colnames(filtered_data_indv[, marker_start:marker_end])

    waiter_show(html = spin_flowers(), color = "rgba(245, 40, 145, 0.2)")

    # Perform bootstrapping with stratification
    boot_mat_strat <- boot(
      sim = "ordinary",
      data = filtered_data_indv,
      statistic = boot_fonction,
      R = n_rep,
      strata = as.numeric(as.factor(filtered_data_indv$level1)),
      columns = columns_to_fstat,
      parallel = "multicore",
      ncpus = num_cores,
    )

    # format the bootstrap results
    boot_mat_strat_CI <- tidy(boot_mat_strat, conf.int = TRUE, conf.method = "perc") # nolint: line_length_linter, object_name_linter.
    boot_mat_strat_CI <- as.data.frame(boot_mat_strat_CI)
    rownames(boot_mat_strat_CI) <- columns_to_fstat

    panmixia_boot_download(boot_mat_strat_CI)
    output$panmixia_boot_result <- renderTable({
      req(panmixia_boot_download())
      panmixia_boot_download()
    })

    # Reset rownames as a column in the data frame
    boot_mat_strat_CI$Marker <- rownames(boot_mat_strat_CI)

    # Convert Sample to a factor (assuming it contains unique values)
    boot_mat_strat_CI$Marker <- as.factor(boot_mat_strat_CI$Marker)

    panmixia_plot <- boot_mat_strat_CI %>%
      ggplot(aes(x = Marker, y = statistic)) +
      geom_point() +
      geom_errorbar(aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)), width = 0.2, position = position_dodge(0.05)) + # nolint: line_length_linter.
      xlab("Loci") +
      ylab("Fis (W&C)")

    #  render the result
    output$panmixia_boot_plot <- renderPlot({
      req(panmixia_plot)
      return(panmixia_plot)
    })

    # Downloadable csv of selected dataset
    output$download_panmixia_boot_plot <- downloadHandler(
      filename = function() {
        paste("panmixia_plot_", Sys.Date(),  ".png")
      },
      content = function(file) {
        ggsave(filename = file, plot = panmixia_plot, dpi = 300)
      }
    )
    waiter_hide()
  })

  # Panmixia download
  output$download_panmixia_csv <- downloadHandler(
    filename = function() {
      paste0("panmixia_stats_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(panmixia_boot_download())
      write.csv(panmixia_boot_download(), file, row.names = FALSE)
    }
  )
}