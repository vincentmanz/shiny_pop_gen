# helper.R

# Logo
header <- dashboardHeader(title = "GenoPop",
                            
                            tags$li(a(href = 'https://umr-intertryp.cirad.fr/en',
                                      img(src = 'INTERTRYP_logo.png',
                                          title = "InterTryp Home", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))


linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Create a data frame for the one example

data_one_col <- data.frame(
  Population = c("Boulouparis", "Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c("192/194", "200/200", "0/0", "145/145", "0/0"),
  C07 = c("145/192", "179/179", "92/100", "92/92", "92/92")
)

# Create a data frame for the two-column example
data_two_col <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  B12 = c(192, 200, 0, 145),
  B12_2 = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92),
  C07_2 = c(192, 179, 92, 100)
)

# Create a data frame for the latitude/longitude example
data_gps <- data.frame(
  Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
  Latitude = c(-21.86444444, -22.16805556, -22.10111111, -21.64111111),
  Longitude = c(166.0391667, 166.2694444, 166.3030556, 165.8461111),
  B12 = c(192, 200, 0, 145),
  "B12" = c(194, 200, 0, 145),
  C07 = c(145, 179, 92, 92),
  "C07" = c(192, 179, 92, 100)
)






# Define a function for concatenate the alleles in columns (serverimportdata)
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


# Define a function for rendering info boxes (serverimportdata)
renderInfoBoxUI <- function(title, value, icon_name, color) {
  infoBox(
    title,
    value,
    icon = icon(icon_name),
    color = color,
    fill = TRUE
  )
}