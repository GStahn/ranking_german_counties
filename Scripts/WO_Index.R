## ---------------------------
##
## Script name: WO_Index
##
## Purpose of script: Create an index for the best Wohnort with new variables
##
## Author: Gerrit Stahn
##
## Date Created: 2025-11-20
## Last Update: 2026-01-26
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

### Load data ##################################################################
normalized_data_GEM <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_gem_2023.rds")) %>%
  relocate(ID, ID_K, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_KRE <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_SK <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)
normalized_data_LK <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds")) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name, Settlement.Area.in.Flood.Zone, Sealed.Area.per.Capita)



### Test with random Weights ###################################################
var_vec_GEM <- names(normalized_data_GEM)
var_vec_KRE <- names(normalized_data_KRE)

weights <- c(
  "Settlement.Area.in.Flood.Zone" =  0.13,
  "Sealed.Area.per.Capita"        = -0.42,
  "no2_avg"                       =  0.77,
  "pm25_avg"                      = -0.09,
  "pm10_avg"                      =  0.34,
  "co_avg"                        = -0.88,
  "so2_avg"                       =  0.51,
  "pb_avg"                        = -0.26,
  "Population"                    =  0.04,
  "New.Housing.per.Capita"        = -0.67,
  "Permit.Housing.perCapita"      =  0.58,
  "Age.below.6"                   = -0.12,
  "Age.6.18"                      =  0.91,
  "Age.65"                        = -0.31,
  "School.Primary"                =  0.22,
  "School.SpecialEdu"             = -0.74,
  "Migration.Balance"             =  0.66,
  "Purchasing.Power"              = -0.18,
  "Recreation.Area.per.Capita"    =  0.83,
  "Forest.Area"                   = -0.55,
  "Water.Area"                    =  0.07,
  "Population.Density"            = -0.93,
  "Highway.Access"                =  0.49,
  "Airport.Access"                = -0.36,
  "Highspeed.Rail.Access"         =  0.95,
  "Supermarket.Access"            = -0.21,
  "Doc.GP"                        =  0.62,
  "Pharmacy.Access"               = -0.47,
  "Broadband.50Mbps"              =  0.28,
  "Broadband.100Mbps"             = -0.81,
  "Broadband.1000Mbps"            =  0.69,
  "Public.Transport.Access"       = -0.14,
  "Traffic.Accidents"             =  0.57,
  "Child.Poverty"                 = -0.99,
  "Daycare"                       =  0.11,
  "Emp.Rate"                      = -0.63,
  "Emp.Rate.Women"                =  0.41,
  "Unemp.Men"                     = -0.06,
  "Investment.Allocations"        =  0.87,
  "Land.Price"                    = -0.39,
  "Emp.Primary"                   =  0.02,
  "Emp.Secundary"                 = -0.52,
  "Emp.Tertiary"                  =  0.73,
  "Emp.Creative"                  = -0.24,
  "Emp.AO.Academic"               =  0.90,
  "Emp.AO.Vocational"             = -0.65,
  "Emp.AO.NoTrain"                =  0.19,
  "Emp.Expert"                    = -0.84,
  "Emp.Specialist"                =  0.56,
  "Emp.Professional"              = -0.33,
  "Emp.Helper"                    =  0.08,
  "Charg.Points.per100EV"         = -0.71,
  "Share.Car.Hybrid"              =  0.27,
  "Share.Car.Electro"             = -0.16,
  "Apprent.Positions"             =  0.64,
  "Apprent"                       = -0.48,
  "Share.Women.Council"           =  0.35,
  "Emp.Rate.Foreign"              = -0.04,
  "Income.Median.Age25to54"       =  0.78,
  "Income.Median.Age55to64"       = -0.29,
  "Pay.Gap.Gender"                =  0.46,
  "GDP.perCapita"                 = -0.59,
  "Rent.NetAvg"                   =  0.12,
  "Age.18.65"                     = -0.91
)

### Vector with all variables and its weights in GEM data ###
weights_inGEM <- weights[names(weights) %in% var_vec_GEM]

# ### Delete later ##############################################################
# `%nin%` = Negate(`%in%`)
# 
# onlyKRE1 <- names(weights) %nin% var_vec_GEM
# (onlyKRE <- var_vec_KRE[onlyKRE1])
# 
# # Test 
# onlyKRE %in% names(weights) # Only LK_ID_Name and Name are missing - fine
# 
# names(weights) %nin% c(var_vec_KRE,var_vec_GEM) # FALSE for all - fine 

# Ensures the correct order later on
order <- names(weights)
order_GEM <- names(weights_inGEM)

weights_vars <- names(data.frame(as.list(weights)))
weights_vars_GEM <- names(data.frame(as.list(weights_inGEM)))

### All K: Create index data ###################################################
n <- length(names(normalized_data_KRE))

names_all <- normalized_data_KRE %>%
  dplyr::select(Name, ID_K)

index <- normalized_data_KRE %>%
  relocate(order, .after="Name") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(5:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_all, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_all_n20_randomweights.txt"), append=FALSE, split=TRUE)
index %>% print(n=20)
sink()

write_rds(index, path=paste0(path_data, "/Manipulated/index_allKRE_2025.rds"))

### SK: Create index data ######################################################
rm(list=setdiff(ls(), c("path_data", "path_work", "normalized_data_GEM", "normalized_data_KRE", "normalized_data_SK", "normalized_data_LK", "weights", "weights_inGEM", "order", "order_GEM", lsf.str())))

n <- length(names(normalized_data_SK))

names_SK <- normalized_data_SK %>%
  dplyr::select(Name, ID_K)

index_SK <- normalized_data_SK %>%
  relocate(order, .after="Name") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(5:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_SK, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_SK_n20_randomweights.txt"), append=FALSE, split=TRUE)
index_SK %>% print(n=20)
sink()

write_rds(index_SK, path=paste0(path_data, "/Manipulated/index_SK_2025.rds"))

### LK: Create index data ######################################################
rm(list=setdiff(ls(), c("path_data", "path_work", "normalized_data_GEM", "normalized_data_KRE", "normalized_data_SK", "normalized_data_LK", "weights", "weights_inGEM", "order", "order_GEM", lsf.str())))

n <- length(names(normalized_data_LK))

names_LK <- normalized_data_LK %>%
  dplyr::select(Name, ID_K)

index_LK <- normalized_data_LK %>%
  relocate(order, .after="Name") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(5:n) * unlist(weights)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID_K, Index) %>%
  left_join(names_LK, by="ID_K") %>%
  arrange(desc(Index))

sink(paste0(path_work, "/Index_output_LK_n20_randomweights.txt"), append=FALSE, split=TRUE)
index_LK %>% print(n=20)
sink()

write_rds(index_LK, path=paste0(path_data, "/Manipulated/index_LK_2025.rds"))

### All GEM: Create index data #################################################
rm(list=setdiff(ls(), c("path_data", "path_work", "normalized_data_GEM", "normalized_data_KRE", "normalized_data_SK", "normalized_data_LK", "weights", "weights_inGEM", "order", "order_GEM", lsf.str())))

n <- length(names(normalized_data_GEM))

names_all <- normalized_data_GEM %>%
  dplyr::select(Name, ID)

index <- normalized_data_GEM %>%
  relocate(order_GEM, .after="Name") %>%
  rowwise() %>%
  mutate(
    Index = sum(c_across(4:n) * unlist(weights_inGEM)),
    .keep = "unused"
  ) %>%
  ungroup() %>%
  dplyr::select(ID, Index) %>%
  left_join(names_all, by="ID") %>%
  arrange(desc(Index)) %>%
  mutate(ID_K = if_else(ID >= 10000000,
                        substr(format(ID, scientific = FALSE, trim=T), 1, 5),
                        substr(format(ID, scientific = FALSE, trim=T), 1, 4))) %>%
  mutate(ID_K = as.numeric(ID_K)) 

index <- index %>%
  arrange(ID_K, desc(Index)) %>%
  group_by(ID_K) %>%
  mutate(rn = row_number()) %>%
  ungroup()

sink(paste0(path_work, "/Index_output_allcounties_n20_randomweights.txt"), append=FALSE, split=TRUE)
index %>% print(n=20)
sink()

write_rds(index, path=paste0(path_data, "/Manipulated/index_allGEM_2025.rds"))


## -----------------------------------------------------------------------------
## ggplots
## -----------------------------------------------------------------------------

rm(list=setdiff(ls(), c("path_data", "path_work", "path_graphs", "weights", "weights_inGEM", lsf.str())))

### Load data ###
index <- read_rds(file = paste0(path_data, "/Manipulated/index_allKRE_2025.rds"))
index_SK <- read_rds(file = paste0(path_data, "/Manipulated/index_SK_2025.rds"))
index_LK <- read_rds(file = paste0(path_data, "/Manipulated/index_LK_2025.rds"))
index_GEM <- read_rds(file = paste0(path_data, "/Manipulated/index_allGEM_2025.rds"))

### All KRE ####################################################################
top20_kreise <- index %>%
  arrange(desc(Index)) %>%
  slice(1:20)

top_synth_kreis <- sum(1* unlist(weights[weights>0])) + sum(-1*unlist(weights[weights<=0]))

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise, aes(x = reorder(Name, Index), y = Index)) +
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

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_randomweights.png"), width = 10, height = 6)

### LK #########################################################################
top20_kreise_LK <- index_LK %>%
  arrange(desc(Index)) %>%
  slice(1:20)

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise_LK, aes(x = reorder(Name, Index), y = Index)) +
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

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_LK_randomweights.png"), width = 10, height = 6)

### SK #########################################################################
top20_kreise_SK <- index_SK %>%
  arrange(desc(Index)) %>%
  slice(1:20)

# Erstellen des ggplot-Diagramms
ggplot(top20_kreise_SK, aes(x = reorder(Name, Index), y = Index)) +
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

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_SK_randomweights.png"), width = 10, height = 6)

### All GEM #########################################################################
top20_GEM <- index_GEM %>%
  arrange(desc(Index)) %>%
  slice(1:20)

top_synth_GEM <- sum(1* unlist(weights_inGEM[weights_inGEM>0])) + sum(-1*unlist(weights_inGEM[weights_inGEM<=0]))

# Erstellen des ggplot-Diagramms
ggplot(top20_GEM, aes(x = reorder(Name, Index), y = Index)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept=top_synth_GEM )+
  coord_flip() +
  scale_y_continuous(limits = c(0, top_synth_GEM))+
  labs(title = "Top 20 Gemeinden nach Lebenswertigkeitsindex", x = "Gemeinde", y = "Index der Lebenswertigkeit") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(paste0(path_graphs, "/Top20_Kreise_Lebenswertigkeitsindex_allGEM_randomweights.png"), width = 10, height = 6)


## -----------------------------------------------------------------------------