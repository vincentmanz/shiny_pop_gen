# Create data frames for each table
Genepop <- data.frame(
  Locus_Pair = c("B12_C07", "B12_D12", "C07_D12", "B12_D10", "C07_D10", 
                 "D12_D10", "B12_A12", "C07_A12", "D12_A12", "D10_A12", 
                 "B12_C03", "C07_C03", "D12_C03", "D10_C03", "A12_C03"),
  Boulouparis = c(0.9934, 0.21257, 0.01211, 0.62175, 0.49897, 
             0.76072, 0.2183, 0.57408, 0.15942, 0.54963, 
             0.21906, 0.82709, 0.85742, 0.85362, 0.74259),
  Bourail = c(0.56965, 0.2621, 0.82058, 0.78897, 0.51097, 
             0.06956, 0.64645, 0.33168, 0.48177, 0.25419, 
             0.90757, 0.92849, 0.44508, 0.43291, 0.39563),
  Canala = c(0.57366, 0.43801, 0.09459, 0.3187, 0.92828, 
             0.88905, 0.65707, 0.84611, 0.09255, 0.70928, 
             0.87776, 0.05951, 0.79778, 0.95179, 0.0889),
  Gadji = c(0.10984, 0.86386, 0.85752, 0.418, 0.06945, 
            0.83364, 0.93543, 0.50072, 0.59337, 0.60581, 
            0.01792, 0.00418, 0.90088, 0.62584, 0.9018),
  LaFoa = c(0.10563, 0.93615, 0.11938, 0.72012, 0.49145, 
            0.67799, 0.85892, 0.87307, 0.75237, 0.79959, 
            0.3763, 0.63856, 0.72618, 0.01239, 0.10319),
  Poquereux = c(0.24002, 0.5799, 0.04589, 0.77246, 0.74833, 
             0.07139, 0.45777, 0.38815, 0.63774, 0.10068, 
             0.06036, 0.76398, 0.77918, 0.33958, 0.15868),
  PortLaguerre = c(0.50067, 0.15184, 0.39305, 0.36653, 0.58338, 
             0.53957, 0.28442, 0.76014, 0.89628, 0.26213, 
             0.29311, 0.50823, 0.49515, 0.48985, 0.10036),
  Sarramea  = c(0.97115, 0.91915, 0.14698, 0.97457, 0.77355, 
             0.37924, 0.62569, 0.91283, 1, 0.87964, 
             0.92333, 0.69225, 1, 0.26935, 0.76526)
  # Global_P_Value = c(0.46903, 0.49486, 0.39741, 0.84574, 0.02312, 
  #         0.42336, 0.57337, 0.77436, 0.14974, 0.8954, 
  #         0.78426, 0.51154, 0.75301, 0.22281, 0.00244)
)

Fstat <- data.frame(
  Locus_Pair = c("B12_C07", "B12_D12", "B12_D10", "B12_A12", "B12_C03", 
                 "C07_D12", "C07_D10", "C07_A12", "C07_C03", "D12_D10", 
                 "D12_A12", "D12_C03", "D10_A12", "D10_C03", "A12_C03"),
  Boulouparis = c(0.9926, 0.2956, 0.6106, 0.2515, 0.1959, 
             0.0397, 0.4409, 0.5848, 0.8428, 0.71, 
             0.357, 0.9276, 0.4602, 0.8555, 0.7319),
  Bourail = c(0.5548, 0.3006, 0.8696, 0.69, 0.8965, 
             0.832, 0.5598, 0.4805, 0.9132, 0.1365, 
             0.4331, 0.4969, 0.2109, 0.4529, 0.58),
  Canala = c(0.5362, 0.4338, 0.3137, 0.6785, 0.8504, 
             0.1626, 0.8777, 0.8633, 0.0398, 0.8218, 
             0.1518, 0.969, 0.6985, 0.987, 0.0792),
  Gadji = c(0.093, 0.9523, 0.601, 0.9054, 0.004, 
            0.9319, 0.164, 0.4536, 0.0143, 0.861, 
            0.4784, 0.675, 0.5673, 0.7661, 0.8436),
  LaFoa = c(0.1602, 0.932, 0.7118, 0.8348, 0.3964, 
            0.2117, 0.7229, 0.7961, 0.8415, 0.6937, 
            0.7286, 0.8848, 0.8227, 0.0634, 0.0952),
  Poquereux = c(0.2359, 0.5006, 0.8188, 0.5005, 0.0947, 
             0.0927, 0.6194, 0.3286, 0.7474, 0.1128, 
             0.585, 0.6967, 0.1136, 0.244, 0.3245),
  PortLaguerre = c(0.4969, 0.119, 0.6473, 0.4172, 0.3068, 
             0.3707, 0.5981, 0.6081, 0.5256, 0.7753, 
             0.8556, 0.2893, 0.2285, 0.7014, 0.0505),
  Sarramea = c(0.9632, 0.8987, 0.9817, 0.6317, 0.9188, 
             0.1096, 0.7657, 0.9158, 0.6679, 0.3441, 
             1, 1, 0.8923, 0.2615, 0.764)
  # Global_P_Value = c(0.3753, 0.556, 0.9352, 0.828, 0.1667, 
  #         0.1202, 0.7692, 0.7973, 0.444, 0.6161, 
  #         0.7226, 0.9392, 0.3343, 0.7117, 0.0766)
)

Vincent <- data.frame(
  Locus_Pair = c("B12.C07", "B12.D12", "B12.D10", "B12.A12", "B12.C03",
                 "C07.D12", "C07.D10", "C07.A12", "C07.C03", "D12.D10",
                 "D12.A12", "D12.C03", "D10.A12", "D10.C03", "A12.C03"),
  Boulouparis = c(0.9968, 0.2966, 0.6082, 0.359, 0.2026,
                  0.0393, 0.444, 0.47, 0.8458, 0.7117,
                  0.3448, 0.927, 0.456, 0.8575, 0.7383),
  Bourail = c(0.1841, 0.2232, 0.9081, 0.7173, 0.7834,
              0.6708, 0.2636, 0.3033, 0.4698, 0.1398,
              0.4456, 0.3766, 0.2394, 0.4844, 0.2729),
  Canala = c(0.4378, 0.4269, 0.2396, 0.6021, 0.8944,
             0.0637, 0.8455, 0.8955, 0.0209, 0.887,
             0.1515, 0.6263, 0.6798, 0.9438, 0.101),
  Gadji = c(0.0438, 0.7876, 0.3054, 0.7657, 0.004,
            0.9632, 0.0101, 0.1409, 0.0051, 0.847,
            0.7976, 0.8896, 0.5317, 0.6697, 0.8548),
  LaFoa = c(0.2461, 0.9291, 0.7667, 0.8364, 0.4464,
            0.0156, 0.2855, 0.6912, 0.0164, 0.707,
            0.7352, 0.7266, 0.8165, 0.0079, 0.181),
  Poquereux = c(0.1627, 0.4747, 0.6501, 0.4055, 0.1823,
                0.1, 0.6576, 0.4253, 0.1945, 0.0792,
                0.5861, 0.4637, 0.142, 0.2927, 0.1406),
  PortLaguerre = c(0.4064, 0.1013, 0.271, 0.2374, 0.284,
                   0.636, 0.8208, 0.5712, 0.1544, 0.2866,
                   0.8367, 0.507, 0.3058, 0.5939, 0.0644),
  Sarramea = c(0.9651, 0.9688, 0.9781, 0.634, 0.9178,
               0.259, 0.7767, 0.9169, 0.6688, 0.3579,
               0.9915, 0.9337, 0.893, 0.2641, 0.7719)
  # Global_P_Value = c(0.5483125, 0.5811625, 0.51265, 0.5293625, 0.5142375,
  #                    0.580675, 0.5939125, 0.6043625, 0.4959125, 0.6150875,
  #                    0.6054, 0.6251, 0.4514125, 0.49995, 0.47595)
)

# Combine tables into long format
long_data <- rbind(
  cbind(Genepop, Source = "Genepop"),
  cbind(Fstat, Source = "Fstat"),
  cbind(Vincent, Source = "Vincent")
)

# Reshape data for statistical testing
library(reshape2)
long_data_melted <- melt(long_data, id.vars = c("Locus_Pair", "Source"), variable.name = "Location", value.name = "Value")

# Perform ANOVA
anova_result <- aov(Value ~ Source * Location, data = long_data_melted)
summary(anova_result)

# Pairwise t-tests
pairwise_ttest <- pairwise.t.test(long_data_melted$Value, long_data_melted$Source, paired = TRUE, p.adjust.method = "bonferroni")
print(pairwise_ttest)



# Combine the data frames
library(tidyr)
library(dplyr)

# Add a column to each data frame to indicate the source table
Genepop$Source <- "Genepop"
Fstat$Source <- "Fstat"
Vincent$Source <- "Vincent"

# Combine all three tables
combined_df <- bind_rows(
  gather(Genepop, key = "Location", value = "Value", -Locus_Pair, -Source),
  gather(Fstat, key = "Location", value = "Value", -Locus_Pair, -Source),
  gather(Vincent, key = "Location", value = "Value", -Locus_Pair, -Source)
)

# View combined data
head(combined_df)

# Perform a regression to compare tables (using Genepop as reference)
regression_model <- lm(Value ~ Source, data = combined_df)
summary(regression_model)

# Pairwise comparisons
library(emmeans)
pairwise <- emmeans(regression_model, pairwise ~ Source)
print(pairwise)

gv <- combined_df %>% 
   filter(Source %in% c("Vincent", "Genepop"))
# Plot regression results
library(ggplot2)
ggplot(combined_df, aes(x = Source, y = Value, color = Source)) +
  geom_boxplot() +
  facet_wrap(~ Location, scales = "free") +
  labs(title = "Comparison of Values Across Tables", x = "Source Table", y = "Values") +
  theme_minimal()

# Scatter plot comparing Genepop vs Vincent
genepop_vincent <- combined_df %>% 
  filter(Source %in% c("Genepop", "Vincent")) %>% 
  spread(Source, Value)

ggplot(genepop_vincent, aes(x = Genepop, y = Vincent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "red") +
  labs(title = "Regression: Genepop vs Vincent", x = "Genepop", y = "Vincent") +
  theme_minimal()

