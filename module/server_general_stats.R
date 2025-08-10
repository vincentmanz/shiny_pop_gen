# server_general_stats.R
# Use with general_stats_ui("gs") and server_general_stats("gs")

num_cores     <- parallel::detectCores()
R_default     <- 1000
n_rep_default <- 1000
# formatted_metadata <- reactive({
#   f <- "data/formatted_data.RData"
#   if (!file.exists(f)) return(NULL)
#   
#   ok <- tryCatch({ load(f); TRUE }, error = function(e) FALSE)
#   if (!ok || !exists("formatted_data") || !is.list(formatted_data)) return(NULL)
#   
#   n <- if (!is.null(formatted_data$haplotype)) nrow(formatted_data$haplotype)
#   else if (!is.null(formatted_data$Population)) length(formatted_data$Population)
#   else return(NULL)
#   
#   gps_df   <- safe_gps(formatted_data$GPS, n)
#   haplo_df <- as.data.frame(formatted_data$haplotype, stringsAsFactors = FALSE)
#   
#   # Start with mandatory columns
#   df_formated <- dplyr::bind_cols(
#     data.frame(Population = safe_vec(formatted_data$Population, n), stringsAsFactors = FALSE),
#     gps_df
#   )
#   
#   # Add optional levels only if they exist in formatted_data
#   if (!is.null(formatted_data$level1)) {
#     df_formated$level1 <- safe_vec(formatted_data$level1, n)
#   }
#   if (!is.null(formatted_data$level2)) {
#     df_formated$level2 <- safe_vec(formatted_data$level2, n)
#   }
#   if (!is.null(formatted_data$level3)) {
#     df_formated$level3 <- safe_vec(formatted_data$level3, n)
#   }
#   
#   # Add haplotype markers at the end
#   df_formated <- dplyr::bind_cols(df_formated, haplo_df)
#   
#   list(
#     df_formated       = df_formated,
#     n_pop             = length(unique(df_formated$Population)),
#     missing_data_code = if (!is.null(formatted_data$missing_code)) formatted_data$missing_code else "NA",
#     haplotype         = haplo_df
#   )
# })
# 
# observeEvent(formatted_metadata(), {
#   md <- formatted_metadata(); req(md)
#   df <- md$df_formated
#   
#   # Build list of grouping choices
#   groupable <- c("Population")
#   if ("level1" %in% names(df) && any(nzchar(df$level1))) groupable <- c(groupable, "Level1")
#   if ("level2" %in% names(df) && any(nzchar(df$level2))) groupable <- c(groupable, "Level2")
#   if ("level3" %in% names(df) && any(nzchar(df$level3))) groupable <- c(groupable, "Level3")
#   
#   updateSelectInput(session, "pop_unit",
#                     choices  = groupable,
#                     selected = "Population")
#   
#   # Populate filter for Level1
#   if ("level1" %in% names(df) && any(nzchar(df$level1))) {
#     levs <- sort(unique(na.omit(df$level1)))
#     updateSelectInput(session, "level1",
#                       choices  = c("All" = "", levs),
#                       selected = "")
#   } else {
#     updateSelectInput(session, "level1",
#                       choices  = c("All" = ""),
#                       selected = "")
#   }
# }, ignoreInit = FALSE)
# 

# INSTEAD IN LEVEL, PRE-POPULATE WITH POPULATION, l1,... L3, WHEN PUSH THE RUN BOTTUN, CHECK IF THE SELCTED LEVEL IS IN formatted_metadata


server_general_stats <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Re-run loaders when any of these buttons are pressed (lazy I/O)
    any_action <- reactive({
      list(
        input$run_basic_stats,
        input$run_plot_heatmap,
        input$run_plot_GST,
        input$run_plot_FIS,
        input$run_panmixia
      )
    })
    
    # ---- helpers ------------------------------------------------------------
    safe_vec <- function(x, n) {
      if (is.null(x)) return(rep(NA, n))
      if (length(x) == n) return(x)
      if (length(x) == 1) return(rep(x, n))
      rep(NA, n)
    }
    
    safe_gps <- function(x, n) {
      if (is.data.frame(x) || is.matrix(x)) {
        x <- as.data.frame(x)
        if (nrow(x) == n && ncol(x) == 2) {
          names(x) <- c("Latitude", "Longitude")
          return(x)
        }
      }
      data.frame(Latitude = rep(NA, n), Longitude = rep(NA, n))
    }
    
    # ---- data loader (lazy; only after first click) -------------------------
    formatted_metadata <- eventReactive(any_action(), {
      f <- "data/formatted_data.RData"
      if (!file.exists(f)) {
        if ("shinyalert" %in% .packages()) {
          shinyalert::shinyalert(
            title = "No formatted data",
            text  = "I can't find data/formatted_data.RData. Go to the Data tab and click 'Assign metadata' first.",
            type  = "error"
          )
        }
        return(NULL)
      }
      
      ok <- tryCatch({ load(f); TRUE }, error = function(e) {
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("Load error", paste("Couldn't load:", e$message), type = "error")
        FALSE
      })
      if (!ok) return(NULL)
      
      if (!exists("formatted_data") || !is.list(formatted_data)) {
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("Bad object", "`formatted_data` missing or not a list.", type = "error")
        return(NULL)
      }
      
      n_hap <- if (!is.null(formatted_data$haplotype)) nrow(formatted_data$haplotype) else NA_integer_
      n_pop <- if (!is.null(formatted_data$Population)) length(formatted_data$Population) else NA_integer_
      n     <- if (!is.na(n_hap)) n_hap else n_pop
      if (is.na(n) || n <= 0) {
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("Empty data", "Could not infer row count from `formatted_data`.", type = "error")
        return(NULL)
      }
      
      x <- formatted_data$haplotype
      if (is.null(x) || !is.data.frame(x) || nrow(x) != n) {
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("Bad haplotype", "Haplotype table missing or row-mismatched.", type = "error")
        return(NULL)
      }
      
      gps_df   <- safe_gps(formatted_data$GPS, n)
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
    
    # Which column to use as grouping (set in UI via selectInput ns("pop_unit"))
    pop_col <- reactive({
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      
      val <- input$pop_unit
      if (is.null(val) || !nzchar(val) || !(val %in% c("Population", "Level1", "Level2", "Level3"))) {
        return("Population")
      }
      
      # Map UI choice to column name in df, but only if it exists
      col_map <- c("Population" = "Population",
                   "Level1" = "level1",
                   "Level2" = "level2",
                   "Level3" = "level3")
      
      colname <- col_map[[val]]
      if (!colname %in% names(df)) {
        return("Population")
      }
      
      return(colname)
    })
    
    # Genotype columns
    geno_cols <- reactive({
      md <- formatted_metadata(); req(md)
      colnames(md$haplotype)
    })
    
    # Populate the “Group by” and level1 filter choices when data first loads
    observeEvent(any_action(), {
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      
      # Grouping choices
      groupable <- c("Population")
      if ("level1" %in% names(df) && any(!is.na(df$level1))) groupable <- c(groupable, "Level1")
      if ("level2" %in% names(df) && any(!is.na(df$level2))) groupable <- c(groupable, "Level2")
      if ("level3" %in% names(df) && any(!is.na(df$level3))) groupable <- c(groupable, "Level3")
      
      updateSelectInput(session, "pop_unit",
                        choices  = groupable,
                        selected = "Population")
      
      # Level1 filter choices
      if ("level1" %in% names(df) && any(!is.na(df$level1))) {
        levs <- sort(unique(na.omit(df$level1)))
        updateSelectInput(session, "level1",
                          choices  = c("All" = "", levs),
                          selected = "")
      } else {
        updateSelectInput(session, "level1",
                          choices  = c("All" = ""),
                          selected = "")
      }
    }, ignoreInit = TRUE)
    
    # Optional filter by level1, and create individual IDs
    filtered_data <- reactive({
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      
      if (!is.null(input$level1) && nzchar(input$level1)) {
        df <- df[df$level1 == input$level1, , drop = FALSE]
      }
      
      df$indv <- paste(substr(df$Population, 1, 3), seq_len(nrow(df)), sep = ".")
      df
    })
    
    # Build genind using the selected grouping
    mydata_genind <- reactive({
      md <- formatted_metadata(); req(md)
      df <- filtered_data();     req(nrow(df) > 0)
      
      grp_col <- pop_col()
      if (!grp_col %in% names(df)) grp_col <- "Population"
      grp <- df[[grp_col]]
      
      validate(need(any(!is.na(grp)), "Selected grouping has only NA values."))
      
      df2genind(
        X          = df[, geno_cols(), drop = FALSE],
        sep        = "/",
        ncode      = 6,
        ind.names  = df$indv,
        pop        = grp,
        NA.char    = md$missing_data_code,
        ploidy     = 2,
        type       = "codom"
      )
    })
    
    mydata_hierfstat <- reactive({
      genind2hierfstat(mydata_genind())
    })
    
    # Missing data table (Population x Locus with % missing)
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
    
    # Will store the full stats table (one row per locus)
    result_stats <- reactiveVal(NULL)
    
    # ================= BASIC STATS ===========================================
    observeEvent(input$run_basic_stats, {
      md <- formatted_metadata(); req(md)
      
      df <- filtered_data()
      if (is.null(df) || nrow(df) == 0) {
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("No data", "Filtered data is empty. Check your level1 filter.", type = "warning")
        return()
      }
      
      my_gi <- tryCatch(mydata_genind(), error = function(e) {
        if ("shinyalert" %in% .packages()) shinyalert::shinyalert("genind error", e$message, type = "error")
        NULL
      })
      if (is.null(my_gi)) return()
      
      my_hier <- tryCatch(mydata_hierfstat(), error = function(e) {
        if ("shinyalert" %in% .packages()) shinyalert::shinyalert("hierfstat error", e$message, type = "error")
        NULL
      })
      if (is.null(my_hier)) return()
      
      n_pop   <- md$n_pop
      result  <- hierfstat::basic.stats(my_hier)
      df_basic <- as.data.frame(result$perloc)
      
      # Weir & Cockerham via pegas
      loci_df <- as.data.frame(pegas::as.loci(my_gi))
      wc      <- pegas::Fst(pegas::as.loci(loci_df))
      colnames(wc) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
      wc <- as.data.frame(wc)
      
      # GST & GST''
      df_basic <- df_basic %>%
        dplyr::mutate(
          GST     = 1 - Hs / Ht,
          `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
        )
      
      merged <- merge(wc, df_basic, by = "row.names", all.x = TRUE)
      rownames(merged) <- merged[, 1]
      merged <- merged[, -1, drop = FALSE]
      
      # rename Nei columns if present
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
        if ("shinyalert" %in% .packages())
          shinyalert::shinyalert("No statistics selected", "Please tick at least one metric.", type = "warning")
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
    
    # ================= HEATMAP (missingness) =================================
    observeEvent(input$run_plot_heatmap, {
      missing_data <- missing_data_tbl()
      req(nrow(missing_data) > 0)
      
      heatmap_df <- tibble::rownames_to_column(missing_data, var = "location")
      heatmap_df <- reshape2::melt(heatmap_df, id.vars = "location", value.name = "percent")
      names(heatmap_df) <- c("location", "marker", "percent")
      
      heatmap_missing <- ggplot2::ggplot(heatmap_df, ggplot2::aes(x = location, y = marker, fill = percent)) +
        ggplot2::geom_tile() +
        ggplot2::labs(x = "Population", y = "Marker", fill = "Missing %") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      
      output$plot_output <- renderPlot({ heatmap_missing })
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_heatmap_missing_data_", Sys.Date(), ".png"),
        content  = function(file) ggsave(filename = file, plot = heatmap_missing, dpi = 300)
      )
    })
    
    # ================= GST vs Hs =============================================
    observeEvent(input$run_plot_GST, {
      res <- result_stats()
      validate(need(!is.null(res), "Please run basic stats first"))
      validate(need(all(c("GST","Hs") %in% colnames(res)), "GST/Hs not available."))
      
      plot_GST <- ggplot2::ggplot(res, ggplot2::aes(x = GST, y = Hs)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = stats::lm, se = FALSE, color = "red") +
        hrbrthemes::theme_ipsum()
      
      output$plot_output <- renderPlot({ plot_GST })
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_GST_", Sys.Date(), ".png"),
        content  = function(file) ggsave(filename = file, plot = plot_GST, dpi = 300)
      )
    })
    
    # ================= FIS vs Missing % ======================================
    observeEvent(input$run_plot_FIS, {
      res <- result_stats()
      validate(need(!is.null(res), "Please run basic stats first"))
      validate(need("Fis (W&C)" %in% colnames(res), "Fis (W&C) not available."))
      
      missing_data <- missing_data_tbl(); req(nrow(missing_data) > 0)
      
      res2 <- res %>% tibble::rownames_to_column(var = "Markers")
      fis  <- res2 %>% dplyr::select(Markers, `Fis (W&C)`) %>% tibble::column_to_rownames("Markers")
      
      md_t <- t(as.data.frame(missing_data)) |> as.data.frame()
      if ("Mean" %in% rownames(md_t)) md_t <- md_t[setdiff(rownames(md_t), "Mean"), , drop = FALSE]
      validate(need("Total" %in% colnames(md_t), "Missing data table has no 'Total' column."))
      
      md_total <- md_t %>% dplyr::select(Total)
      colnames(md_total) <- "Missing %"
      
      merged <- merge(fis, md_total, by = "row.names", all.x = TRUE) %>%
        tibble::column_to_rownames("Row.names")
      
      plot_FIS <- ggplot2::ggplot(merged, ggplot2::aes(x = `Missing %`, y = `Fis (W&C)`)) +
        ggplot2::geom_point() +
        hrbrthemes::theme_ipsum() +
        ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red")
      
      output$plot_output <- renderPlot({ plot_FIS })
      output$download_plot_png <- downloadHandler(
        filename = function() paste0("plot_FIS_", Sys.Date(), ".png"),
        content  = function(file) ggsave(filename = file, plot = plot_FIS, dpi = 300)
      )
    })
    
    # ================= Panmixia bootstrap (optional) ==========================
    observeEvent(input$run_panmixia, {
      df_indv <- filtered_data(); req(nrow(df_indv) > 0)
      n_rep   <- if (!is.null(input$numboot)) input$numboot else n_rep_default
      
      # columns to bootstrap over: all loci
      md <- formatted_metadata(); req(md)
      columns_to_fstat <- colnames(md$haplotype)
      
      if ("waiter" %in% .packages()) waiter::waiter_show(html = waiter::spin_flowers(),
                                                         color = "rgba(245, 40, 145, 0.2)")
      
      boot_mat_strat <- boot::boot(
        data     = df_indv,
        statistic= boot_fonction,   # defined in helper.R
        R        = n_rep,
        strata   = as.numeric(as.factor(df_indv$Population)),
        columns  = columns_to_fstat,
        parallel = "multicore",
        ncpus    = num_cores
      )
      
      boot_CI <- broom::tidy(boot_mat_strat, conf.int = TRUE, conf.method = "perc") |> as.data.frame()
      rownames(boot_CI) <- columns_to_fstat
      
      output$panmixia_boot_result <- renderTable(boot_CI, rownames = TRUE)
      output$download_panmixia_csv <- downloadHandler(
        filename = function() paste0("panmixia_stats_result_", Sys.Date(), ".csv"),
        content  = function(file) write.csv(boot_CI, file, row.names = TRUE)
      )
      
      boot_CI$Marker <- factor(rownames(boot_CI))
      panmixia_plot <- ggplot2::ggplot(boot_CI, ggplot2::aes(x = Marker, y = statistic)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)),
                               width = 0.2, position = ggplot2::position_dodge(0.05)) +
        ggplot2::xlab("Loci") + ggplot2::ylab("Fis (W&C)")
      
      output$panmixia_boot_plot <- renderPlot({ panmixia_plot })
      output$download_panmixia_boot_plot <- downloadHandler(
        filename = function() paste0("panmixia_plot_", Sys.Date(), ".png"),
        content  = function(file) ggsave(filename = file, plot = panmixia_plot, dpi = 300)
      )
      
      if ("waiter" %in% .packages()) waiter::waiter_hide()
    })
    
  })
}
