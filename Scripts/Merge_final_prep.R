## ---------------------------
##
## Script name: Merge_final_prep
##
## Purpose of script: Merge all data and final prep. steps
##
## Author: Gerrit Stahn
##
## Date Created: 2025-11-11
## Last Update: 2025-11-11
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
library(readr)

### clean start ###
rm(list = ls())

### Function for better object management ### 
keep<-function(x){
  obj <- deparse(substitute(x))
  rm(list = setdiff(ls(envir = .GlobalEnv), c("path_data", "path_work", "path_graphs", 
  obj, lsf.str(envir = .GlobalEnv))), pos=1)
}

### set working directory and paths ###
setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties")      
path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"


## -----------------------------------------------------------------------------
## Merge all + prep
## -----------------------------------------------------------------------------

keep()

### GEM ########################################################################
inkar_gem <- read_rds(file = paste0(path_data, "/Manipulated/merged_gem_final.rds"))
ground_gem <- read_rds(file = paste0(path_data, "/Manipulated/ground_gem.rds")) %>%
  dplyr::select(!c("Name", "ID_K"))

data_gem <- inkar_gem %>%
  left_join(ground_gem, by="ID") %>%
  relocate(ID, ID_K, Name)

write_rds(data_gem, file = paste0(path_data, "/Manipulated/data_gem_2023.rds"))

### Normalize data ###
vars_to_normalize <- names(data_gem)[sapply(data_gem, is.numeric) & !names(data_gem) %in% c("ID", "ID_K")]

normalized_data_gem <- data_gem %>%
  mutate(across(.col=vars_to_normalize, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_gem, file = paste0(path_data, "/Manipulated/normalized_data_gem_2023.rds"))
write_csv(normalized_data_gem, file = paste0(path_data, "/Manipulated/normalized_data_gem_2023.csv"))

### KRE ########################################################################
keep()

pollution_kre <- read_rds(file = paste0(path_data, "/Manipulated/pollution_400_2023.rds")) %>%
  dplyr::select(!c("Year"))
inkar_kre <- read_rds(file = paste0(path_data, "/Manipulated/merged_kre_final.rds")) 
ground_kre <- read_rds(file = paste0(path_data, "/Manipulated/ground_kre.rds")) %>%
  dplyr::select(!c("Name", "Lfd"))

data_kre <- pollution_kre %>%
  left_join(inkar_kre, by="ID_K") %>%
  left_join(ground_kre, by="ID_K") %>%
  rename(LK_ID= Kreisfreie.Stadt...Landkreis..2023..Kennziffer) %>%
  rename(LK_ID_Name= Kreisfreie.Stadt...Landkreis..2023..Name) %>%
  relocate(ID_K, LK_ID, LK_ID_Name, Name)

write_rds(data_kre, file = paste0(path_data, "/Manipulated/data_kre_2023.rds"))

### Normalize data ###
vars_to_normalize <- names(data_kre)[sapply(data_kre, is.numeric) & !names(data_kre) %in% c("ID_K", "LK_ID")]
normalized_data_kre <- data_kre %>%
  mutate(across(.cols = vars_to_normalize, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_kre, file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds"))
write_csv(normalized_data_kre, file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.csv"))

## -----------------------------------------------------------------------------
## Split LK and SK
## -----------------------------------------------------------------------------

### SK ###
data_SK <- data_kre %>%
  filter(LK_ID==1)

write_rds(data_SK , file = paste0(path_data, "/Manipulated/data_SK_2023.rds"))
write_csv(data_SK , file = paste0(path_data, "/Manipulated/data_SK_2023.csv"))

normalized_data_SK <- data_SK %>%
  mutate(across(.col=vars_to_normalize, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_SK, file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds"))
write_csv(normalized_data_SK, file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.csv"))

### LK ###
data_LK <- data_kre %>%
  filter(LK_ID==2)

write_rds(data_LK , file = paste0(path_data, "/Manipulated/data_LK_2023.rds"))
write_csv(data_LK , file = paste0(path_data, "/Manipulated/data_LK_2023.csv"))

normalized_data_LK <- data_LK %>%
  mutate(across(.col=vars_to_normalize, ~ (. - min(.)) / (max(.) - min(.))))

write_rds(normalized_data_LK, file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds"))
write_csv(normalized_data_LK, file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.csv"))

## -----------------------------------------------------------------------------







## -----------------------------------------------------------------------------