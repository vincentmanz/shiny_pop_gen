# server_general_stats.R
# Use with a module UI like: general_stats_ui("gs") and server_general_stats("gs")

# ==== environment variables ====
num_cores <- parallel::detectCores()
R_default <- 1000      # default bootstrap reps (if you need it elsewhere)
n_rep_default <- 1000  # default replicates (HW-Panmixia)

server_general_stats <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # --- one reactive dependency that changes when ANY action button is pressed
    any_action <- reactive({
      list(
        input$run_basic_stats,
        input$run_plot_heatmap,
        input$run_plot_GST,
        input$run_plot_FIS,
        input$run_panmixia
      )
    })
    
    # ---- SAFE builder helpers ------------------------------------------------
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
    
    # --- lazy-load formatted_data.RData when the FIRST button is pressed ------
    formatted_metadata <- eventReactive(any_action(), {
      load("data/formatted_data.RData")  # must create `formatted_data` in env
      
      validate(
        need(exists("formatted_data"), "formatted_data not found in RData."),
        need(is.list(formatted_data), "formatted_data is not a list.")
      )
      
      # Determine row count from haplotype first, then Population
      n_hap <- if (!is.null(formatted_data$haplotype)) nrow(formatted_data$haplotype) else NA_integer_
      n_pop <- if (!is.null(formatted_data$Population)) length(formatted_data$Population) else NA_integer_
      n <- if (!is.na(n_hap)) n_hap else n_pop
      validate(need(!is.na(n) && n > 0, "Could not infer row count from formatted_data."))
      
      # Build pieces safely
      gps_df   <- safe_gps(formatted_data$GPS, n)
      haplo_df <- {
        x <- formatted_data$haplotype
        validate(need(!is.null(x) && nrow(x) == n, "haplotype table missing or row-mismatched."))
        as.data.frame(x, stringsAsFactors = FALSE)
      }
      
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
    }, ignoreInit = TRUE)  # don't touch disk at app load
    
    # --- genotype columns from haplotype -------------------------------------
    geno_cols <- reactive({
      md <- formatted_metadata(); req(md)
      colnames(md$haplotype)
    })
    
    # --- populate level1 dropdown AFTER first click (lazy) --------------------
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
    
    # --- filtered dataframe (optional filter by level1) -----------------------
    filtered_data <- reactive({
      md <- formatted_metadata(); req(md)
      df <- md$df_formated
      
      if (!is.null(input$level1) && nzchar(input$level1)) {
        df <- df[df$level1 == input$level1, , drop = FALSE]
      }
      
      df$indv <- paste(substr(df$Population, 1, 3), seq_len(nrow(df)), sep = ".")
      df
    })
    
    # --- genind & hierfstat ---------------------------------------------------
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
    
    # --- missing-data table (built only when needed) --------------------------
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
    
    # --- stash full stats after basic stats run --------------------------------
    result_stats <- reactiveVal(NULL)
    
    ############################################################################
    # Actions
    ############################################################################
    
    # ---- BASIC STATS (Ho, Hs, Ht, W&C, Nei, GST, GST'') ----------------------
    observeEvent(input$run_basic_stats, {
      my_hier <- mydata_hierfstat()
      my_gi   <- mydata_genind()
      n_pop   <- formatted_metadata()$n_pop
      
      # hierfstat per-locus
      result   <- hierfstat::basic.stats(my_hier)
      df_basic <- as.data.frame(result$perloc)
      
      # Weir & Cockerham via pegas
      loci_df <- as.data.frame(adegenet::as.loci(my_gi))
      wc      <- pegas::Fst(pegas::as.loci(loci_df))
      colnames(wc) <- c("Fit (W&C)", "Fst (W&C)", "Fis (W&C)")
      wc <- as.data.frame(wc)
      
      # GST & GST''
      df_basic <- df_basic %>%
        dplyr::mutate(
          GST     = 1 - Hs / Ht,
          `GST''` = (n_pop * (Ht - Hs)) / ((n_pop * Hs - Ht) * (1 - Hs))
        )
      
      # merge
      merged <- merge(wc, df_basic, by = "row.names", all.x = TRUE)
      rownames(merged) <- merged[, 1]
      merged <- merged[, -1, drop = FALSE]
      
      # optional rename if Nei columns exist
      if ("Fst" %in% names(merged)) names(merged)[names(merged) == "Fst"] <- "Fst (Nei)"
      if ("Fis" %in% names(merged)) names(merged)[names(merged) == "Fis"] <- "Fis (Nei)"
      
      result_stats(merged)
      
      # selection from UI
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
        if ("shinyalert" %in% .packages()) {
          shinyalert::shinyalert(
            title = "No statistics selected",
            text  = "Please tick at least one metric.",
            type  = "warning"
          )
        }
        return(invisible(NULL))
      }
      
      to_show <- merged[, names(selected_stats)[selected_stats], drop = FALSE]
      
      output$basic_stats_result <- renderTable({
        req(ncol(to_show) > 0)
        to_show
      })
      
      output$download_gstats_csv <- downloadHandler(
        filename = function() paste0("basic_stats_result_", Sys.Date(), ".csv"),
        content  = function(file) write.csv(to_show, file, row.names = TRUE)
      )
    })
    
    # ---- HEATMAP (missingness) -----------------------------------------------
    observeEvent(input$run_plot_heatmap, {
      missing_data <- missing_data_tbl()
      req(nrow(missing_data) > 0)
      
      heatmap_df <- tibble::rownames_to_column(missing_data, var = "location") |>
        reshape2::melt(value.name = "percent")
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
    
    # ---- GST vs Hs plot (add buttons/UI as needed) ---------------------------
    observeEvent(input$run_plot_GST, {
      res <- result_stats()
      validate(need(!is.null(res), "Run basic stats first."))
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
    
    # ---- FIS vs Missing% plot (add buttons/UI as needed) ---------------------
    observeEvent(input$run_plot_FIS, {
      res <- result_stats()
      validate(need(!is.null(res), "Run basic stats first."))
      validate(need("Fis (W&C)" %in% colnames(res), "Fis (W&C) not available."))
      
      missing_data <- missing_data_tbl()
      req(nrow(missing_data) > 0)
      
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
  })
}
