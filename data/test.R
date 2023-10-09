populationsLL = read.table(
  header = TRUE,text="
 Locality Longitude Latitude
 Boulouparis   166.0391667 -21.86444444
 Boulouparis   166.0391667 -21.86444444
 Boulouparis   166.0391667 -21.86444444
 Boulouparis   166.0391667 -21.86444444
 Boulouparis   166.0391667 -21.86444444
 Boulouparis   166.0391667 -21.86444444
 Bourail   165.4802778 -21.56722222
 Bourail   165.4802778 -21.56722222
 Bourail   165.4802778 -21.56722222
 Bourail   165.4802778 -21.56722222
 Poquereux   165.9280556 -21.71916667
 Poquereux   165.9280556 -21.71916667
 ") %>% as_tibble()
populationsLL
populationsLL_grouped <- populationsLL %>%
  group_by_all()%>%count()
populationsLL_grouped
colnames(populationsLL_grouped) <- c('Population', 'Longitude', 'Latitude', 'Population size')


colnames(populationsLL_grouped)

print(populationsLL_grouped$Population)
print(populationsLL_grouped$`Population size`)