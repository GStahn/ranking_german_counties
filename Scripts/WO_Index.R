## ---------------------------
##
## Script name: WO_Index
##
## Purpose of script: Create an index for the best Wohnort with new variables
##
## Author: Gerrit Stahn
##
## Date Created: 2025-11-20
## Last Update: 2025-11-20
##
## Copyright (c) Gerrit Stahn, 2025
## Email: gerrit.stahn@wiwi.uni-halle.de
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## -----------------------------------------------------------------------------
## Start
## -----------------------------------------------------------------------------

### Install packages (uncomment as required) ###
# install.packages("tidyverse")

### Load add-on packages ### 
library(tidyverse)

# clean start
rm(list = ls())

## set working directory and paths
setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties")      
path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"
path_graphs <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Graphs"
path_work <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Work"

## -----------------------------------------------------------------------------
## Prep. data
## -----------------------------------------------------------------------------

### Load data ###
normalized_data_GEM <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_gem_2023.rds")) %>%
  relocate(ID, ID_K, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_KRE <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_SK <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_LK <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)



### Weights ###
var_vec <- names(normalized_data_GEM)

weights <- c(
  "no2_avg" = -0.8,
  "pm25_avg" = -0.8,
  "pm10_avg" = -0.8,
  "co_avg" = -0.8,
  "so2_avg" = -0.8,
  "pb_avg" = -0.8,
  "Population" = 0.6,
  "GDP.perCapita" = 0.8,
  "Expert.Jobs" = 0.6,
  "Rent.NetAvg" = -0.8,
  "New.Housing.per.Capita" = 0.6,
  "Migration.Balance" = 0.5,
  "Purchasing.Power" = 0.9,
  "Recreation.Area.per.Capita" = 0.9,
  "Forest.Area" = 0.8,
  "Water.Area" = 0.7,
  "Investment.Allocations" = 0.6,
  "Population.Density" = -0.5,
  "Highway.Access" = 0.5,
  "Airport.Access" = 0.8,
  "Supermarket.Access" = 0.8,
  "Pharmacy.Access" = 0.5,
  "Broadband.100Mbps" = 0.8,
  "Public.Transport.Access" = 0.9,
  "Traffic.Accidents" = -0.4,
  "Child.Poverty" = -0.9,
  "Daycare" = 0.5,
  "Broadband.1000Mbps" = 0.7,
  "Settlement.Area.in.Flood.Zone" = -0.7,
  "Sealed.Area.per.Capita" = -0.8,
  ### New ###
  "Age.below.6" = 0, 
  "Age.6.18" = 0.5,
  "Age.65" = 0, 
  "Age.18.65" = 0.8, 
  "Permit.Housing.perCapita"  = 0.8, 
  "School.Primary" = 0, 
  "School.SpecialEdu" = 0, 
  "Highspeed.Rail.Access" = 0.8, 
  "Doc.GP" = 0.7, 
  "Broadband.50Mbps" = 0.6, 
  "Emp.Rate" = 0.8, 
  "Emp.Rate.Women" = 0.8, 
  "Unemp.Men" = -0.8,
)

weights <- names(data.frame(as.list(weights)))

### Delete later ###
`%nin%` = Negate(`%in%`)

names <- var_vec %nin% weights
names2 <- weights %nin% var_vec
(var_vec[names])
(weights[names2])

################################

### Test ###
# test <- tibble(a=seq(3,9, by=3), b=seq(2, 6, by=2), c=seq(1,3))
# weights <- c(
#   a <- -0.5,
#   b<- 0.3,
#   c<- 0.2
# )
# 
# index <- test %>%
#   rowwise() %>%
#   mutate(
#     Index = sum(c_across("a":"c") * unlist(weights)),
#     .keep = "unused"
#   ) %>%
#   ungroup()

# Ensures the correct order
order <- names(weights)

### All K: Create index data ###
n <- length(names(normalized_data))

names_all <- normalized_data %>%
  dplyr::select(Region, ID_K)

index <- normalized_data %>%
  relocate(order, .after="Region") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(6:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_all, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_all_n20.txt"), append=FALSE, split=TRUE)
index %>% print(n=20)
sink()

write_rds(index, file = paste0(path_data, "/Manipulated/index_all_2020.rds"))

### SK: Create index data ###
rm(list=setdiff(ls(), c("path_data", "path_work", "normalized_data", "normalized_data_SK", "normalized_data_LK", "weights", "order", lsf.str())))

n <- length(names(normalized_data_SK))

names_SK <- normalized_data_SK %>%
  dplyr::select(Region, ID_K)

index_SK <- normalized_data_SK %>%
  relocate(order, .after="Region") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(6:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_SK, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_SK_n20.txt"), append=FALSE, split=TRUE)
index_SK %>% print(n=20)
sink()

write_rds(index_SK, file = paste0(path_data, "/Manipulated/index_SK_2020.rds"))

### LK: Create index data ###
rm(list=setdiff(ls(), c("path_data", "path_work", "normalized_data", "normalized_data_SK", "normalized_data_LK", "weights", "order", lsf.str())))

n <- length(names(normalized_data_LK))

names_LK <- normalized_data_LK %>%
  dplyr::select(Region, ID_K)

index_LK <- normalized_data_LK %>%
  relocate(order, .after="Region") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(6:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_LK, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_LK_n20.txt"), append=FALSE, split=TRUE)
index_LK %>% print(n=20)
sink()

write_rds(index_LK, file = paste0(path_data, "/Manipulated/index_LK_2020.rds"))

## -----------------------------------------------------------------------------
## ggplots
## -----------------------------------------------------------------------------

rm(list=setdiff(ls(), c("path_data", "path_work", "weights", lsf.str())))

### Load data ###
index <- read_rds(file = paste0(path_data, "/Manipulated/index_all_2020.rds"))
index_SK <- read_rds(file = paste0(path_data, "/Manipulated/index_SK_2020.rds"))
index_LK <- read_rds(file = paste0(path_data, "/Manipulated/index_LK_2020.rds"))

### Verteilung ###
top20_kreise <- index %>%
  arrange(desc(Index)) %>%
  slice(1:20)

top_synth_kreis <- sum(1* unlist(weights))

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise, aes(x = reorder(Region, Index), y = Index)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept=top_synth_kreis)+
  coord_flip() +
  scale_y_continuous(limits = c(0, top_synth_kreis))+
  labs(title = "Top 20 Kreise nach Lebenswertigkeitsindex", x = "Kreis", y = "Index der Lebenswertigkeit") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex.png"), width = 10, height = 6)

top20_kreise_LK <- index_LK %>%
  arrange(desc(Index)) %>%
  slice(1:20)

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise_LK, aes(x = reorder(Region, Index), y = Index)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept=top_synth_kreis)+
  coord_flip() +
  scale_y_continuous(limits = c(0, top_synth_kreis))+
  labs(title = "Top 20 Kreise nach Lebenswertigkeitsindex", x = "Kreis", y = "Index der Lebenswertigkeit") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_LK.png"), width = 10, height = 6)

top20_kreise_SK <- index_SK %>%
  arrange(desc(Index)) %>%
  slice(1:20)

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise_SK, aes(x = reorder(Region, Index), y = Index)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept=top_synth_kreis)+
  coord_flip() +
  scale_y_continuous(limits = c(0, top_synth_kreis))+
  labs(title = "Top 20 Kreise nach Lebenswertigkeitsindex", x = "Kreis", y = "Index der Lebenswertigkeit") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_SK.png"), width = 10, height = 6)


## -----------------------------------------------------------------------------