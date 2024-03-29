# helper.R


# Define a function for concatenate the alleles in columns (server_importdata)
concat_identical_cols <- function(df, ploidy) {
  col_names <- colnames(df)
  col_names <-
    sub("\\.\\d+$", "", col_names)  # Remove .1 or .2 from column names
  
  # Initialize result with one row from temp_df
  col1 <- col_names[1]
  temp_df <- data.frame(col1 = df[[col1]])
  colnames(temp_df) <- col1
  result <- temp_df
  i <- 1
  while (i <= ncol(df)) {
    col1 <- col_names[i]
    if (i + 1 <= ncol(df)) {
      col2 <- col_names[i + 1]
    } else {
      result <- rbind(result, df[[col1]])
      break
    }
    if (identical(col1, col2)) {
      concatenated <-
        ifelse(is.na(df[, i + 1]), as.character(df[, i]), paste(df[, i], df[, i + 1], sep = "/"))
      temp_df <- data.frame(col1 = concatenated)
      colnames(temp_df) <- col1
      result <- cbind(result, temp_df)
      i <- i + ploidy
    } else {
      result <- cbind(result, df[[col1]], df[[col2]])
      i <- i + ploidy
    }
  }
  result <- result[,-1]
  
  return(result)
}


# Define a function for rendering info boxes (server_importdata)
renderInfoBoxUI <- function(title, value, icon_name, color) {
  infoBox(
    title,
    value,
    icon = icon(icon_name),
    color = color,
    fill = TRUE
  )
}

# Define the function for the bootstrap (server_general_stats)
boot_fonction <- function(data, indices, columns) {
      subset_data <- as.data.frame(data[indices, columns, drop = FALSE])
      subset_data <- adegenet::df2genind(
        X = as.matrix(subset_data),
        sep = "/",
        ncode = 6,
        ind.names = data$indv,
        pop = data$Population,
        NA.char = "0/0",
        ploidy = 2,
        type = "codom",
        strata = NULL,
        hierarchy = NULL
      )
      fst_results <- as.data.frame(pegas::Fst(pegas::as.loci(subset_data)))
      results_mat <- fst_results %>%
        select(Fis) %>%
        as.matrix()
      return(results_mat)
    }