# server_general_stats.R 

filtered_data <- read.csv("data/data-2023-09-11 (2).csv", header = TRUE)

# Define the server logic
general_stats_server <- function(input, output, session) {
  
  # Function to run the basic.stats and render the result
  observeEvent(input$run_basic_stats, {
    selected_stats <- c(
      Ho = input$ho_checkbox,
      Hs = input$hs_checkbox,
      Ht = input$ht_checkbox,
      "Fis (W&C)" = input$fisw_checkbox,
      "Fst (W&C)" = input$fstw_checkbox,
      "Fis (Nei)" = input$fisn_checkbox,
      "Fst (Nei)" = input$fstn_checkbox
    )
    selected_stats <- selected_stats[selected_stats]
    if (length(selected_stats) > 0) {
      # Prepare the concatenated_data and df_format_1
      filtered_data <- data.frame(indv = paste(substr(filtered_data$Population,1,3), row.names(filtered_data), sep="."), filtered_data)
      
      # Create genind object
      population <- filtered_data$Population
      mydata_genind <- df2genind(
        X = filtered_data[,6:11],
        sep = "/",
        ncode = 6,
        ind.names = filtered_data$indv,
        pop = filtered_data$Population,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      # Convert to hierfstat
      mydata_hierfstat <- genind2hierfstat(mydata_genind)
      
      # Run basic.stats and render the result
      result <- basic.stats(mydata_hierfstat)
      df_resutl_basic<-as.data.frame(result$perloc)
      
      # Weir and Cockrham estimates of Fstatistics - FIS and FST 
      result_f_stats <- wc(mydata_hierfstat)
      result_f_stats <- as.data.frame(result_f_stats$per.loc)
      colnames(result_f_stats) <- c("Fis (W&C)", "Fst (W&C)")
      result_f_stats <- merge(result_f_stats, df_resutl_basic,by="row.names",all.x=TRUE)
      colnames(result_f_stats)[10] <- "Fst (Nei)"
      colnames(result_f_stats)[12] <- "Fis (Nei)"
      
      #subset the table
      select_df <- as.data.frame(selected_stats)
      selected_stats <- rownames(select_df)
      result_f_stats_selec <- result_f_stats %>% select(all_of(selected_stats))
      
      #  render the result
      output$basic_stats_result <- renderTable({
        req(result_f_stats_selec)
        return(result_f_stats_selec) 
      })
    }
  })
}
