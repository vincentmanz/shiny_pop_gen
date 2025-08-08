# ==== environment variables ====
num_cores <- parallel::detectCores()
R_default <- 1000      # default bootstrap reps
n_rep_default <- 1000  # default replicates (HW-Panmixia)

server_general_stats <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    any_action <- reactive({
      list(
        input$run_basic_stats,
        input$run_plot_heatmap,
        input$run_plot_GST,
        input$run_plot_FIS,
        input$run_panmixia
      )
    })
    
    safe_vec <- function(x, n) {
      if (is.null(x)) return(rep(NA, n))
      if (length(x) == n) return(x)
      if (length(x) == 1) return(rep(x, n))
      rep(NA, n)
    }
    
    safe_gps <- function(x, n) {
      if (is.data.frame(x) || is.matrix(x)) {
        x <- as.data.frame(x)
        if (nrow(x) == n) {
          if (ncol(x) == 2) {
            names(x) <- c("Latitude", "Longitude")
            return(x)
          }
        }
      }
      data.frame(Latitude = rep(NA, n), Longitude = rep(NA, n))
    }
    
    formatted_metadata <- eventReactive(any_action(), {
      f <- "data/formatted_data.RData"
      
      if (!file.exists(f)) {
        shinyalert::shinyalert(
          title = "No formatted data",
          text  = "I can't find data/formatted_data.RData. Go to the Data tab and click 'Assign metadata' first.",
          type  = "error"
        )
        return(NULL)
      }
      
      ok <- tryCatch({
        load(f)
        TRUE
      }, error = function(e) {
        shinyalert::shinyalert("Load error", paste("Couldn't load:", e$message), type = "error")
        FALSE
      })
      if (!ok) return(NULL)
      
      if (!exists("formatted_data")) {
        shinyalert::shinyalert("Missing object", "The RData file doesn't contain `formatted_data`.", type = "error")
        return(NULL)
      }
      if (!is.list(formatted_data)) {
        shinyalert::shinyalert("Bad object", "`formatted_data` is not a list.", type = "error")
        return(NULL)
      }
      
      n_hap <- if (!is.null(formatted_data$haplotype)) nrow(formatted_data$haplotype) else NA_integer_
      n_pop <- if (!is.null(formatted_data$Population)) length(formatted_data$Population) else NA_integer_
      n <- if (!is.na(n_hap)) n_hap else n_pop
      if (is.na(n) || n <= 0) {
        shinyalert::shinyalert("Empty data", "Could not infer row count from `formatted_data`.", type = "error")
        return(NULL)
      }
      
      x <- formatted_data$haplotype
      if (is.null(x) || !is.data.frame(x) || nrow(x) != n) {
        shinyalert::shinyalert("Bad haplotype", "Haplotype table missing or row-mismatched.", type = "error")
        return(NULL)
      }
      
      gps_df <- safe_gps(formatted_data$GPS, n)
      haplo_df <- as.data.frame(x, stringsAsFactors = FALSE)
      
      df_formated <- dplyr::bind_cols(
        data.frame(Population = safe_vec(formatted_data$Population, n), stringsAsFactors = FALSE),
        gps_df,
        data.frame(
          level1 = safe_vec(formatted_data$level1, n),
          level2 = safe_vec(formatted_data$level2, n),
          level3 = safe_vec(formatted_data$level3, n),
          stringsAsFactors = FALSE
        ),
        haplo_df
      )
      
      list(
        df_formated        = df_formated,
        n_marker           = ncol(haplo_df),
        n_pop              = length(unique(df_formated$Population)),
        n_indv             = nrow(df_formated),
        pops               = unique(df_formated$Population),
        missing_data_code  = if (!is.null(formatted_data$missing_code)) formatted_data$missing_code else "NA",
        haplotype          = haplo_df
      )
    }, ignoreInit = TRUE)
    
    geno_cols <- reactive({
      md <- formatted_metadata(); req(md)
      colnames(md$haplotype)
    })
    
    observeEvent(any_action(), {
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      if (!is.null(df$level1)) {
        choices <- sort(unique(df$level1))
        updateSelectInput(session, "level1",
                          choices = c("All" = "", choices),
                          selected = ""
        )
      }
    }, ignoreInit = TRUE)
    
    filtered_data <- reactive({
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      if (!is.null(input$level1) && nzchar(input$level1)) {
        df <- df[df$level1 == input$level1, , drop = FALSE]
      }
      df$indv <- paste(substr(df$Population, 1, 3), seq_len(nrow(df)), sep = ".")
      df
    })
    
    mydata_genind <- reactive({
      md <- formatted_metadata(); req(md)
      df <- filtered_data(); req(nrow(df) > 0)
      df2genind(
        X          = df[, geno_cols(), drop = FALSE],
        sep        = "/",
        ncode      = 6,
        ind.names  = df$indv,
        pop        = df$Population,
        NA.char    = md$missing_data_code,
        ploidy     = 2,
        type       = "codom"
      )
    })
    
    mydata_hierfstat <- reactive({
      genind2hierfstat(mydata_genind())
    })
    
    missing_data_tbl <- reactive({
      gi <- mydata_genind(); req(gi)
      md_tab <- poppr::info_table(gi, plot = FALSE, percent = TRUE, df = TRUE)
      validate(need(is.data.frame(md_tab), "No Missing Data Found!"))
      validate(need(all(c("Population","Locus","Missing") %in% names(md_tab)),
                    "Missing-data table lacks Population/Locus/Missing."))
      md_wide <- tidyr::pivot_wider(md_tab, names_from = Locus, values_from = Missing)
      md_wide <- tibble::column_to_rownames(md_wide, var = "Population")
      as.data.frame(md_wide) * 100
    })
    
    result_stats <- reactiveVal(NULL)
    
    # ---------------- BASIC STATS ----------------
    observeEvent(input$run_basic_stats, {
      md <- formatted_metadata()
      if (is.null(md)) return()
      
      df <- filtered_data()
      if (is.null(df) || nrow(df) == 0) {
        shinyalert::shinyalert("No data", "Filtered data is empty. Check your level1 filter.", type = "warning")
        return()
      }
      
      my_gi <- tryCatch(mydata_genind(), error = function(e) {
        shinyalert::shinyalert("genind error", e$message, type = "error")
        NULL
      })
      if (is.null(my_gi)) return()
      
      my_hier <- tryCatch(mydata_hierfstat(), error = function(e) {
        shinyalert::shinyalert("hierfstat error", e$message, type = "error")
        NULL
      })
      if (is.null(my_hier)) return()
      
      n_pop <- md$n_pop
      result   <- hierfstat::basic.stats(my_hier)
      df_basic <- as.data.frame(result$perloc)
      loci_df  <- as.data.frame(pegas::as.loci(my_gi))
      wc       <- pegas::Fst(pegas::as.loci(loci_df))
      colnames(wc) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
      wc <- as.data.frame(wc)
      
      df_basic <- df_basic %>%
        dplyr::mutate(
          GST     = 1 - Hs / Ht,
          `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
        )
      
      merged <- merge(wc, df_basic, by = "row.names", all.x = TRUE)
      rownames(merged) <- merged[, 1]
      merged <- merged[, -1, drop = FALSE]
      if ("Fst" %in% names(merged)) names(merged)[names(merged) == "Fst"] <- "Fst (Nei)"
      if ("Fis" %in% names(merged)) names(merged)[names(merged) == "Fis"] <- "Fis (Nei)"
      
      result_stats(merged)
      
      selected_stats <- c(
        "Ho" = input$ho_checkbox,
        "Hs" = input$hs_checkbox,
        "Ht" = input$ht_checkbox,
        "Fit (W&C)" = input$fit_wc_checkbox,
        "Fis (W&C)" = input$fis_wc_checkbox,
        "Fst (W&C)" = input$fst_wc_checkbox,
        "Fis (Nei)" = input$fis_n_checkbox,
        "Fst (Nei)" = input$fst_n_checkbox,
        "GST"       = input$GST_checkbox,
        "GST''"     = input$GST_sec_checkbox
      )
      
      if (!any(selected_stats)) {
        shinyalert::shinyalert(
          title = "No statistics selected",
          text  = "Please tick at least one metric.",
          type  = "warning"
        )
        return(invisible(NULL))
      }
      
      to_show <- merged[, names(selected_stats)[selected_stats], drop = FALSE]
      output$basic_stats_result <- renderTable({
        req(ncol(to_show) > 0)
        to_show
      }, rownames = TRUE)
      output$download_gstats_csv <- downloadHandler(
        filename = function() paste0("basic_stats_result_", Sys.Date(), ".csv"),
        content  = function(file) write.csv(to_show, file, row.names = TRUE)
      )
    })
    
    # ==== Heatmap of missing data ====
    observeEvent(input$run_plot_heatmap, {
      filtered_data_indv <- filtered_data()
      missing_data <- missing_data_tbl()
      
      # Data shaping
      missing_data <- tibble::rownames_to_column(missing_data, var = "location")
      
      # Melt for ggplot2
      heatmap_data_melted <- reshape2::melt(missing_data, id.vars = "location", value.name = "percent")
      colnames(heatmap_data_melted) <- c("location", "marker", "percent")
      
      # Plot
      heatmap_missing <- ggplot(heatmap_data_melted, aes(x = location, y = marker, fill = percent)) +
        geom_tile() +
        labs(x = "Population", y = "Marker", fill = "Missing %") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      output$plot_output <- renderPlot({ heatmap_missing })
      
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_heatmap_missing_data_", Sys.Date(), ".png"),
        content = function(file) ggsave(file, plot = heatmap_missing, dpi = 300)
      )
    })
    
    # ==== GST plot ====
    observeEvent(input$run_plot_GST, {
      df_stats <- result_stats()
      validate(need(!is.null(df_stats), "Please run basic stats first"))
      
      plot_GST <- ggplot(df_stats, aes(x = GST, y = Hs)) +
        geom_point() +
        geom_smooth(method = lm, color = "red", se = FALSE) +
        hrbrthemes::theme_ipsum()
      
      output$plot_output <- renderPlot({ plot_GST })
      
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_GST_", Sys.Date(), ".png"),
        content = function(file) ggsave(file, plot = plot_GST, dpi = 300)
      )
    })
    
    # ==== FIS vs Missing % ====
    observeEvent(input$run_plot_FIS, {
      df_stats <- result_stats()
      missing_data <- missing_data_tbl()
      
      fis <- tibble::rownames_to_column(df_stats, var = "Markers") %>%
        dplyr::select(Markers, `Fis (W&C)`) %>%
        tibble::column_to_rownames("Markers")
      
      missing_data_t <- t(as.data.frame(missing_data))
      missing_total <- as.data.frame(missing_data_t) %>% dplyr::select(Total)
      missing_total <- subset(missing_total, !rownames(missing_total) %in% "Mean")
      colnames(missing_total) <- "Missing %"
      
      fis_missing <- merge(fis, missing_total, by = "row.names", all.x = TRUE) %>%
        tibble::column_to_rownames("Row.names")
      
      plot_FIS <- ggplot(fis_missing, aes(x = `Missing %`, y = `Fis (W&C)`)) +
        geom_point() +
        hrbrthemes::theme_ipsum() +
        scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
        geom_smooth(method = "lm", se = FALSE, color = "red")
      
      output$plot_output <- renderPlot({ plot_FIS })
      
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_FIS_", Sys.Date(), ".png"),
        content = function(file) ggsave(file, plot = plot_FIS, dpi = 300)
      )
    })
    
    # ==== Panmixia Bootstrap ====
    observeEvent(input$run_panmixia, {
      filtered_data_indv <- filtered_data()
      n_rep <- input$numboot
      columns_to_fstat <- colnames(formatted_data$haplotype)
      
      waiter_show(html = spin_flowers(), color = "rgba(245, 40, 145, 0.2)")
      
      boot_mat_strat <- boot(
        data = filtered_data_indv,
        statistic = boot_fonction,
        R = n_rep,
        strata = as.numeric(as.factor(filtered_data_indv$Population)),
        columns = columns_to_fstat,
        parallel = "multicore",
        ncpus = num_cores
      )
      
      boot_CI <- broom::tidy(boot_mat_strat, conf.int = TRUE, conf.method = "perc")
      boot_CI <- as.data.frame(boot_CI)
      rownames(boot_CI) <- columns_to_fstat
      
      output$panmixia_boot_result <- renderTable(boot_CI, rownames = TRUE)
      
      output$download_panmixia_csv <- downloadHandler(
        filename = function() paste0("panmixia_stats_result_", Sys.Date(), ".csv"),
        content = function(file) write.csv(boot_CI, file, row.names = TRUE)
      )
      
      boot_CI$Marker <- factor(rownames(boot_CI))
      
      panmixia_plot <- ggplot(boot_CI, aes(x = Marker, y = statistic)) +
        geom_point() +
        geom_errorbar(aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)),
                      width = 0.2, position = position_dodge(0.05)) +
        xlab("Loci") + ylab("Fis (W&C)")
      
      output$panmixia_boot_plot <- renderPlot({ panmixia_plot })
      
      output$download_panmixia_boot_plot <- downloadHandler(
        filename = function() paste0("panmixia_plot_", Sys.Date(), ".png"),
        content = function(file) ggsave(file, plot = panmixia_plot, dpi = 300)
      )
      
      waiter_hide()
    })
    
  })
}
