# Given data
data <- "
Boulouparis 3.7037037 0.0000000 0.000000 3.703704 0.000000 0.0000000 1.234568
Bourail 1.7094017 2.5641026 8.547009 11.111111 1.709402 0.0000000 4.273504
Canala 0.9433962 0.9433962 12.264151 8.490566 3.773585 0.0000000 4.402516
Gadji 5.6338028 12.6760563 2.816901 5.633803 7.042254 32.3943662 11.032864
LaFoa 0.0000000 0.0000000 26.250000 7.500000 1.250000 1.2500000 6.041667
Poquereux 0.0000000 1.8691589 26.168224 3.738318 2.803738 0.9345794 5.919003
"

# Convert the data into a dataframe
df <- read.table(text = data, header = FALSE)

# Set column names
colnames(df) <- c("Location", "A12", "B12", "C03", "C07", "D10", "D12", "Mean")

# Print the dataframe
print(df)
library(ggplot2)

# Assuming `Var1` and `Var2` are row and column names respectively
heatmap_data <- df[, -1]

# Melt the data for ggplot2
heatmap_data_melted <- reshape2::melt(df,value.name = "Location" )
colnames(heatmap_data_melted) <- c("location", "marker", "percent")

# Plot the heatmap
p1 <- ggplot(data = heatmap_data_melted, aes(x = location, y = marker, fill = percent)) + 
  geom_tile() +
  labs(x = "Column Names", y = "Row Names", fill = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility
plot(a)




mpg <- transform(mpg, cyl = factor(cyl), trans = factor(trans), drv = factor(drv), fl = factor(fl), class = factor(class))
levels(mpg$trans) <- c(rep("auto", 8), rep("manual", 2))
str(mpg)
p2 <- ggplot(data   = mpg,              # spécifier les données
       mapping = aes(x = displ,    # mapper 'displ' à l'axe des x
                     y = hwy)) +   # mapper 'hwy' à l'axe des y
  geom_point()           

p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p3

library(patchwork)

a <- p1 + p2 / p3 
a
















selected_plot_result <- NA

selected_plots <- c(
  "Heatmap with missing data per marker and population",
  "GST",
  "FIS and missing data"
)

# Check each element in the list
for (plot_name in selected_plots) {
  if (plot_name == "Heatmap with missing data per marker and population") {
    print("heatmap")
  } else if (plot_name == "GST") {
    print("GST")
  } else if (plot_name == "FIS and missing data") {
    print("FIS")
  }
}















if (length(num_plots_reactive()) > 0) {
  splitLayout(
    style = "border: 1px solid silver;",
    cellWidths = rep(100 / length(num_plots_reactive()), length(num_plots_reactive()),  # Divide equally based on the number of plots
                     lapply(1:length(num_plots_reactive()), function(i) {
                       plotOutput(paste0("plotgraph", i))
                     })
    )
  )
}