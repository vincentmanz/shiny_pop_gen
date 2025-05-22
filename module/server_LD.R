library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(shinybusy) # For progress bar

# Define server logic for Linkage Disequilibrium tab
server_LD <- function(input, output, session) {
  
  # Load data (this would be dynamically updated in the app)
  data <- read.csv("data/data-2023-09-11 (2).csv")
  loci <- c("B12", "C07", "D12", "D10", "A12", "C03")
  
  # Combined process with a progress bar
  summary_table_reactive <- reactive({
    req(input$run_LD) # Require the "Run" button to be pressed
    
    # Show progress bar
    show_modal_progress_line()
    
    # Step 1: Generate contingency tables
    update_modal_progress(0.1) # 10% progress
    include_missing <- input$include_missing
    contingency_tables <- create_contingency_tables(data, loci, include_missing) # From helper.R
    Sys.sleep(0.5) # Simulate delay for demonstration
    
    # Step 2: Generate observed G-statistics
    update_modal_progress(0.3) # 30% progress
    observed_g_stats <- add_g_stats(contingency_tables) # From helper.R
    Sys.sleep(0.5) # Simulate delay for demonstration
    
    # Step 3: Generate randomized G-statistics
    update_modal_progress(0.6) # 60% progress
    n_simulations <- input$n_iterations
    randomized_stats <- randomized_g_stats(data, loci, n_simulations, calculate_g_stat, include_missing = TRUE) # From helper.R
    Sys.sleep(0.5) # Simulate delay for demonstration
    
    # Step 4: Calculate p-values
    update_modal_progress(0.8) # 80% progress
    pvalues <- calculate_pvalues(observed_g_stats, randomized_stats) # From helper.R
    global_pvalues <- calculate_global_pvalues(observed_g_stats, randomized_stats) # From helper.R
    Sys.sleep(0.5) # Simulate delay for demonstration
    
    # Step 5: Create summary table
    update_modal_progress(1.0) # 100% progress
    summary_table <- create_summary_table(pvalues, global_pvalues) # From helper.R
    
    # Remove progress bar when done
    remove_modal_progress()
    
    return(summary_table)
  })
  
  # Output: Summary Table
  output$summary_output <- renderTable({
    req(summary_table_reactive())
    summary_table_reactive()
  })
  
  # Output: Download CSV
  output$download_gstats_csv <- downloadHandler(
    filename = function() {
      paste("LD_between_each_pair_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(summary_table_reactive())
      write.csv(summary_table_reactive(), file, row.names = FALSE)
    }
  )
}
