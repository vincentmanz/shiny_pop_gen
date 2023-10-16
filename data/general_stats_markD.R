---
title: "R Notebook"
output: html_notebook
---

# Parameters 

```{r}
filtered_data <- read.csv("data/data-2023-09-11 (2).csv", header = TRUE)

selected_stats <- c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)"
)

n_rep=10

sequence_length <- length(6:11) 
```


# data filtering convertion

```{r}
filtered_data <- filtered_data
filtered_data <- data.frame(indv = paste(substr(filtered_data$Population,1,3), row.names(filtered_data), sep="."), filtered_data)
# Create mydata_genind
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
mydata_genind
mydata_hierfstat <- genind2hierfstat(mydata_genind)
```



# Run basic.stats and render the result

```{r}
result <- basic.stats(mydata_hierfstat)
df_resutl_basic<-as.data.frame(result$perloc)

# Weir and Cockrham estimates of Fstatistics - FIS and FST 

result_f_stats <- Fst(as.loci(mydata_genind))
result_f_stats <- result_f_stats[,2:3]
colnames(result_f_stats) <- c("Fst (W&C)", "Fis (W&C)")
result_f_stats <- merge(result_f_stats, df_resutl_basic, by="row.names",all.x=TRUE)
colnames(result_f_stats)[10] <- "Fst (Nei)"
colnames(result_f_stats)[12] <- "Fis (Nei)"
result_f_stats <- result_f_stats %>% column_to_rownames(., var = 'Row.names')
result_f_stats_selec <- result_f_stats %>% select(all_of(selected_stats))

```

# Missing data

```{r}
missing_data <- info_table(mydata_genind, plot = FALSE)


# Libraries
library(heatmaply)



# Matrix format
mat <- as.matrix(missing_data)
# heatmap
p <- heatmaply(mat, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Population", "Marker", "Value"),
               fontsize_row = 8, fontsize_col = 8,
               labCol = colnames(mat),
               labRow = rownames(mat),
               heatmap_layers = theme(axis.line=element_blank())
)
p
```



#

```{r}


```



#

```{r}


```


#

```{r}


```
