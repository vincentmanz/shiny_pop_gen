# server_general_stats.R 

filtered_data <- read.csv("data/filtered_data.csv")

# Define the server logic
general_stats_server <- function(input, output, session) {
  
  # Function to run the basic.stats and render the result
  observeEvent(input$run_basic_stats, {
    selected_stats <- c(
      Ho = input$ho_checkbox,
      Hs = input$hs_checkbox,
      Ht = input$ht_checkbox,
      Dst = input$dst_checkbox,
      Htp = input$htp_checkbox,
      Dstp = input$dstp_checkbox,
      Fst = input$fst_checkbox,
      Fstp = input$fstp_checkbox,
      Fis = input$fis_checkbox,
      Dest = input$dest_checkbox
    )
    
    selected_stats <- selected_stats[selected_stats]
    if (length(selected_stats) > 0) {
      # Prepare the concatenated_data and df_format_1
      df_format_1 <- data.frame(indv = paste(substr(diploid_2C$Locality,1,3), row.names(diploid_2C), sep="."), filtered_data)
      # Create mydata_genind
      population <- df_format_1$Locality
      mydata_genind <- df2genind(
        X = concatenated_data,
        sep = "/",
        ncode = 6,
        ind.names = df_format_1$indv,
        pop = df_format_1$Locality,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      # Create mydata_hierfstat
      mydata_hierfstat <- genind2hierfstat(mydata_genind)
      # Run basic.stats and render the result
      result <- basic.stats(mydata_hierfstat)
      df_resutl_basic<-as.data.frame(result$perloc)
      list_col_selected <- row.names(as.data.frame(selected_stats))
      df_resutl_basic_selec <- df_resutl_basic %>% select(all_of(list_col_selected))
      output$basic_stats_result <- renderTable({
        req(df_resutl_basic_selec)
        return(df_resutl_basic_selec) 
      })
    }
  })
}