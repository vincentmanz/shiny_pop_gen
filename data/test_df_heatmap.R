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




fis <- data.frame(
  Fis_W_and_C = c(0.05262381, 0.09439829, 0.17029220, 0.12144043, 0.15500499, 0.18009170),
  row.names = c("A12", "B12", "C03", "C07", "D10", "D12")
)

# Print the fis data frame
print(fis)


# Create a data frame
missing_data <- data.frame(
  A12 = c(3.7037037, 1.7094017, 0.9433962, 5.6338028, 0.0000000, 0.0000000, 3.0120482, 0.0000000, 1.8624642),
  B12 = c(0.0000000, 2.5641026, 0.9433962, 12.6760563, 0.0000000, 1.8691589, 4.8192771, 0.0000000, 3.2951289),
  C03 = c(0.000000, 8.547009, 12.264151, 2.816901, 26.250000, 26.168224, 7.228916, 0.000000, 12.320917),
  C07 = c(3.703704, 11.111111, 8.490566, 5.633803, 7.500000, 3.738318, 5.421687, 0.000000, 6.590258),
  D10 = c(0.000000, 1.709402, 3.773585, 7.042254, 1.250000, 2.803738, 7.228916, 0.000000, 3.868195),
  D12 = c(0.0000000, 0.0000000, 0.0000000, 32.3943662, 1.2500000, 0.9345794, 10.8433735, 16.6666667, 6.7335244),
  Mean = c(1.234568, 4.273504, 4.402516, 11.032864, 6.041667, 5.919003, 6.425703, 2.777778, 5.778415)
)

# Add row names
rownames(missing_data) <- c(
  "Boulouparis", "Bourail", "Canala", "Gadji", "LaFoa", "Poquereux", "PortLaguerre", "Sarramea", "Total"
)

# Print the data frame
print(missing_data)

missing_data_transposed <- t(missing_data)

missing_data_transposed_total <- as.data.frame(missing_data_transposed) %>% select("Total") 
missing_data_transposed_total <- subset(missing_data_transposed_total, !rownames(missing_data_transposed_total) %in% "Mean")
colnames(missing_data_transposed_total) <- ('Missing %')
fis_missing_merged <- merge(fis, missing_data_transposed_total, by="row.names",all.x=TRUE) %>% column_to_rownames('Row.names')
