## ---------------------------
##
## Script name: Data_inkar
##
## Purpose of script: Prep data from IOER database
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
# install.packages("tidyverse") # for dplyr and readr

### Load add-on packages ### 
library(dplyr)
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
## Data IOER
## -----------------------------------------------------------------------------

### Load data ##################################################################

### GEM ###

# Vector or relevant counties
counties <- read_rds(file=paste0(path_data, "/counties_2023.rds"))

# Data.frame with all relevant gems
gem_liste <- read_rds(file=paste0(path_data, "/gem_liste.rds"))

# Settlement Area in Flood Zone
R05RT <- read_csv2(paste0(path_data, "/IOER_Monitor/R05RT__2024_gem.csv"), skip=6,
                   trim_ws = T, locale = locale("de", decimal_mark = ","),
                   col_names = c("Lfd", "ID", "Name", 
                                              "Settlement.Area.in.Flood.Zone")) %>%
  filter(!(Name=="Deutschland")) %>%
  mutate(ID=as.numeric(ID)) %>%
  filter(ID %in% counties) %>%
  mutate(Settlement.Area.in.Flood.Zone = gsub(pattern=",", replacement='.', x=Settlement.Area.in.Flood.Zone)) %>%
  mutate(Settlement.Area.in.Flood.Zone = replace(Settlement.Area.in.Flood.Zone, Settlement.Area.in.Flood.Zone== "keine", NA)) %>%
  mutate(Settlement.Area.in.Flood.Zone = as.numeric(Settlement.Area.in.Flood.Zone)) %>%
  dplyr::select(ID, Settlement.Area.in.Flood.Zone)


# Sealed Area per Capita
B22MT <- read_csv2(paste0(path_data, "/IOER_Monitor/B22MT__2018_gem.csv"), skip=6,
                   trim_ws = T, col_names = c("Lfd", "ID", "Name", 
                                              "Sealed.Area.per.Capita")) %>%
  filter(!(Name=="Deutschland")) %>%
  mutate(ID=as.numeric(ID)) %>%
  filter(ID %in% counties) %>%
  mutate(Sealed.Area.per.Capita = replace(Sealed.Area.per.Capita, Sealed.Area.per.Capita== "keine Daten, nicht berechenbar", NA)) %>%
  mutate(Sealed.Area.per.Capita = as.numeric(Sealed.Area.per.Capita)) %>%
  dplyr::select(ID, Sealed.Area.per.Capita) 

ground_gem <- gem_liste %>%
  left_join(R05RT, by="ID") %>%
  left_join(B22MT, by="ID") %>%
  arrange(ID) %>%
  mutate(ID_K = if_else(ID >= 10000000,
                        substr(format(ID, scientific = FALSE, trim=T), 1, 5),
                        substr(format(ID, scientific = FALSE, trim=T), 1, 4))) %>%
  mutate(ID_K = as.numeric(ID_K))

summary(ground_gem)

### KRE ###

# Settlement Area in Flood Zone
R05RT_kre <- read_csv2(paste0(path_data, "/IOER_Monitor/R05RT__2024_kre.csv"), skip=6,
                   trim_ws = T, locale = locale("de", decimal_mark = ","),
                   col_names = c("Lfd", "ID_K", "Name", 
                                 "Settlement.Area.in.Flood.Zone")) %>%
  filter(!(Name=="Deutschland")) %>%
  mutate(ID_K=as.numeric(ID_K)) 


# Sealed Area per Capita
B22MT_kre <- read_csv2(paste0(path_data, "/IOER_Monitor/B22MT__2018_kre.csv"), skip=6,
                   trim_ws = T, col_names = c("Lfd", "ID_K", "Name", 
                                              "Sealed.Area.per.Capita")) %>%
  filter(!(Name=="Deutschland")) %>%
  mutate(ID_K=as.numeric(ID_K)) %>%
  dplyr::select(ID_K, Sealed.Area.per.Capita) 

ground_kre <- R05RT_kre %>%
  left_join(B22MT_kre, by="ID_K")

summary(ground_kre)

### Impute GEM NAs with KRE data ###############################################

vars_to_fill <- setdiff(names(ground_gem), c("ID", "ID_K"))

ground_filled <- ground_gem %>%
  left_join(
    ground_kre %>%
      dplyr::select(all_of(c("ID_K", vars_to_fill))) %>%
      rename_with(~ paste0(., "_kre"), vars_to_fill),
    by = "ID_K"
  ) %>%
  mutate(across(all_of(vars_to_fill),
                ~ ifelse(is.na(.x), get(paste0(cur_column(), "_kre")), .x)
  )) %>%
  dplyr::select(-ends_with("_kre"))

summary(ground_gem)
summary(ground_filled)

### Save data ##################################################################

write_rds(ground_filled, file = paste0(path_data, "/Manipulated/ground_gem.rds"))
write_rds(ground_kre, file = paste0(path_data, "/Manipulated/ground_kre.rds"))


## -----------------------------------------------------------------------------