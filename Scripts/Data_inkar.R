## ---------------------------
##
## Script name: Data_inkar
##
## Purpose of script: Prep data from INKAR database
##
## Author: Gerrit Stahn
##
## Date Created: 2025-10-21
## Last Update: 2026-03-06
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
# install.packages("devtools")
# library(devtools)
# install_github("sumtxt/bonn", force=TRUE)
# install.packages("tidyverse") # for dplyr, readxl, tidyr and purrr
# install.packages("collapse")
# install.packages("purrr")
# install.packages("writexl")

### Load add-on packages ### 
library(bonn)
library(dplyr)
library(readxl)
library(tidyr)
library(collapse)
library(purrr)
library(writexl)
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
path_work <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Work"

## -----------------------------------------------------------------------------
## INKAR Data
## -----------------------------------------------------------------------------

### Download ###################################################################

### Functions ####
inkar_multi <- function(var_vec, num_vec, geo) {
  
  if (length(var_vec) != length(num_vec)) {
    stop("Vector 'var_vec' und 'num_vec' doesn't have the same length.")
  }
  
  
  data_list <- list()
  meta <- data.frame()
  
  
  for (i in seq_along(var_vec)) {
    var <- var_vec[i]
    num <- num_vec[i]
    
    
    df <-  get_data(variable=num, geography = geo) %>%
      mutate(Time = as.numeric(Zeit)) %>%
      filter(Time==max(Time)) %>%
      mutate(ID = as.numeric(Schlüssel)) %>%
      rename({{ var }} := Wert) %>%
      select(c("ID", "Raumbezug", {{var}}, "Time"))
    data_list[[var]] <- df
    
    
    meta_append <- get_metadata(num)
    meta_append$var <- var
    meta_append$source <- "INKAR"
    meta <- rbind(meta, meta_append)
  }
  
  
  return(list(
    data = data_list,
    meta = meta
  ))
}

inkar_vote <- function(var_vec, num_vec) {
  
  if (length(var_vec) != length(num_vec)) {
    stop("Vector 'var_vec' und 'num_vec' doesn't have the same length.")
  }
  
  data_list <- list()
  meta <- data.frame()
  
  for (i in seq_along(var_vec)) {
    var <- var_vec[i]
    num <- num_vec[i]
    
    df <-  get_data(variable=num, geography = "KRE") %>%
      mutate(Time = as.numeric(Zeit)) %>%
      slice_max(order_by = Time, n=3, by=Schlüssel) %>%
      fgroup_by(Schlüssel) %>%
      fselect(Wert) %>%
      fmean() %>%
      fungroup() %>%
      mutate(ID = as.numeric(Schlüssel)) %>%
      rename({{ var }} := Wert)
    data_list[[var]] <- df
    
    meta_append <- get_metadata(num)
    meta_append$var <- var
    meta_append$source <- "INKAR"
    meta <- rbind(meta, meta_append)
  }
  
  return(list(
    data_vote = data_list,
    meta_vote = meta
  ))
}

### Geography: GEM (Gemeinden) ###
# get_geographies()
get_themes(geography = "GEM")
get_variables(theme="131", geography = "GEM")
get_metadata("363")

# Build vectors 
var_vec <- c(
  "Population", # Bevölkerung gesamt
#  "Elg_Workers", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "New_Housing_per_Capita", # Neubauwohnungen je Einwohner
  "Permit_Housing_perCapita", # Baugenehmigungen für Wohnungenje Einwohner
  "Age_below_6", # Einwohner unter 6 Jahren
  "Age_6_18", # Einwohner von 6 bis unter 18 Jahren
  "Age_65", # Einwohner 65 Jahre und älter
  "School_Primary", # Grundschulen
  "School_SpecialEdu", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "Migration_Balance", # Gesamtwanderungssaldo
  "Purchasing_Power", # Kaufkraft
  "Recreation_Area_per_Capita", # Erholungsfläche je Einwohner
  "Forest_Area", # Waldfläche
  "Water_Area", # Wasserfläche
  'Population_Density', # Einwohnerdichte
  'Highway_Access', # Erreichbarkeit von Autobahnen
  'Airport_Access', # Erreichbarkeit von Flughäfen
  'Highspeed_Rail_Access', # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  'Supermarket_Access', # Entfernung zum Supermarkt/Discounter
  'Doc_GP', # Entfernung zum Hausarzt
  "Pharmacy_Access", # Entfernung nächste Apotheke
  'Broadband_50Mbps', # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  'Broadband_100Mbps', # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  'Broadband_1000Mbps', # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
#  "New_Family_Houses", # Neue Ein- und Zweifamilienhäuser
  'Public_Transport_Access', # Entfernung zur ÖV Haltestelle
  'Traffic_Accidents', # Verunglückte im Straßenverkehr
  'Child_Poverty', # Kinderarmut
  'Daycare', # Anzahl Kindertagesstätten
# 'Students_18_to_25', # Studierende je 100 Einwohner 18 bis 25 Jahre
  "Emp_Rate", # Beschäftigtenquote
  "Emp_Rate_Women", # Beschäftigtenquote Frauen
  "Unemp_Men" # Anteil Arbeitslose Männer zu Gesamtarbeitslose
)

num_vec <- c(
  "2", # Bevölkerung gesamt
#  "6", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "53", # Neubauwohnungen je Einwohner
  "47", # Baugenehmigungen für Wohnungenje Einwohner
  "121", # Einwohner unter 6 Jahren
  "123", # Einwohner von 6 bis unter 18 Jahren
  "129", # Einwohner 65 Jahre und älter
  "502", # Grundschulen
  "511", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "162", # Gesamtwanderungssaldo
  "248", # Kaufkraft
  "258", # Erholungsfläche je Einwohner
  "264", # Waldfläche 
  "265", # Wasserfläche
  "320", # Einwohnerdichte
  "354", # Erreichbarkeit von Autobahnen
  "355", # Erreichbarkeit von Flughäfen
  "356", # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  "359", # Entfernung zum Supermarkt/Discounter
  "362", # Entfernung zum Hausarzt
  "363", # Entfernung nächste Apotheke
  "369", # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  "370", # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  "371", # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
#  "51", # Neue Ein- und Zweifamilienhäuser
  "367", # Entfernung zur ÖV Haltestelle
  "382", # Verunglückte im Straßenverkehr
  "344", # Kinderarmut
  "528", # Anzahl Kindertagesstätten
# "218", # Studierende je 100 Einwohner 18 bis 25 Jahre
  "67", # Beschäftigtenquote
  "68", # Beschäftigtenquote Frauen
  "16" # Anteil Arbeitslose Männer zu Gesamtarbeitslose
)

# Execute inkar_multi
gem_list <- inkar_multi(var_vec, num_vec, geo="GEM")

### Geography: KRE (Kreis) ###

# get_geographies()
get_themes(geography = "KRE")
get_variables(theme="131", geography = "KRE")
get_metadata("496")

# Build vectors
var_vec <- c(
  ### Same variables as for GEM ################################################
  "Population", # Bevölkerung gesamt
  #  "Elg_Workers", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "New_Housing_per_Capita", # Neubauwohnungen je Einwohner
  "Permit_Housing_perCapita", # Baugenehmigungen für Wohnungenje Einwohner
  "Age_below_6", # Einwohner unter 6 Jahren
  "Age_6_18", # Einwohner von 6 bis unter 18 Jahren
  "Age_65", # Einwohner 65 Jahre und älter
  "School_Primary", # Grundschulen
  "School_SpecialEdu", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "Migration_Balance", # Gesamtwanderungssaldo
  "Purchasing_Power", # Kaufkraft
  "Recreation_Area_per_Capita", # Erholungsfläche je Einwohner
  "Forest_Area", # Waldfläche
  "Water_Area", # Wasserfläche
  'Population_Density', # Einwohnerdichte
  'Highway_Access', # Erreichbarkeit von Autobahnen
  'Airport_Access', # Erreichbarkeit von Flughäfen
  'Highspeed_Rail_Access', # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  'Supermarket_Access', # Entfernung zum Supermarkt/Discounter
  'Doc_GP', # Entfernung zum Hausarzt
  "Pharmacy_Access", # Entfernung nächste Apotheke
  'Broadband_50Mbps', # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  'Broadband_100Mbps', # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  'Broadband_1000Mbps', # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
#  "New_Family_Houses", # Neue Ein- und Zweifamilienhäuser
  'Public_Transport_Access', # Entfernung zur ÖV Haltestelle
  'Traffic_Accidents', # Verunglückte im Straßenverkehr
  'Child_Poverty', # Kinderarmut
  'Daycare', # Anzahl Kindertagesstätten
  # 'Students_18_to_25', # Studierende je 100 Einwohner 18 bis 25 Jahre
  #  'Doctors', # Hausärzte
  "Emp_Rate", # Beschäftigtenquote
  "Emp_Rate_Women", # Beschäftigtenquote Frauen
  "Unemp_Men", # Anteil Arbeitslose Männer zu Gesamtarbeitslose
  ### Only District-level ######################################################
  "Investment_Allocations", # Zuweisungen für Investitionsfördermaßnahmen
  "Land_Price", # Baulandpreise
  "Emp_Primary", # Beschäftigte Pimärer Sektor
  "Emp_Secundary", # Beschäftigte Sekundärer Sektor
  "Emp_Tertiary", # Beschäftigte Tertiärer Sektor
  "Emp_Creative", # Beschäftigte in Kreativbranchen
  "Emp_AO_Academic", # Beschäftigte am AO mit akademischem Berufsabschluss
  "Emp_AO_Vocational", # Beschäftigte am AO mit Berufsabschluss
  "Emp_AO_NoTrain", # Beschäftigte am AO ohne Berufsabschluss
  "Emp_Expert", # Beschäftigte mit Anforderungsniveau Experte
  "Emp_Specialist", # Beschäftigte mit Anforderungsniveau Spezialist
  "Emp_Professional", # Beschäftigte mit Anforderungsniveau Fachkraft
  "Emp_Helper", # Beschäftigte mit Anforderungsniveau Helfer 
  "Charg_Points_per100EV", # Ladepunkte je 100 Elektrofahrzeuge
  "Share_Car_Hybrid", # Pkw Hybrid insgesamt
  "Share_Car_Electro", # Pkw Elektro
  "Apprent_Positions", # Ausbildungsplätze
  "Apprent", # Auszubildende
  "Share_Women_Council", # Frauenanteil im Stadtrat bzw. Kreistag
  "Emp_Rate_Foreign", # Beschäftigtenquote Ausländer
  "Income_Median_Age25to54", # Medianeinkommen 25 bis unter 55-Jährige
  "Income_Median_Age55to64", # Medianeinkommen 55 bis unter 65-Jährige
  "Pay_Gap_Gender", # Verdienstabstand zwischen Frauen und Männern
  "GDP_perCapita", # Bruttoinlandsprodukt je Einwohner
  "Land_Price" # Baulandpreise
)

num_vec <- c(
  ### Same variables as for GEM ################################################
  "1", # Bevölkerung gesamt
  #  "6", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "53", # Neubauwohnungen je Einwohner
  "47", # Baugenehmigungen für Wohnungenje Einwohner
  "121", # Einwohner unter 6 Jahren
  "122", # Einwohner von 6 bis unter 18 Jahren
  "129", # Einwohner 65 Jahre und älter
  "502", # Grundschulen
  "511", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "162", # Gesamtwanderungssaldo
  "248", # Kaufkraft
  "258", # Erholungsfläche je Einwohner
  "264", # Waldfläche
  "265", # Wasserfläche
  '320', # Einwohnerdichte
  "354", # Erreichbarkeit von Autobahnen
  "355", # Erreichbarkeit von Flughäfen
  "356", # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  "359", # Entfernung zum Supermarkt/Discounter
  "362", # Entfernung zum Hausarzt
  "363", # Entfernung nächste Apotheke
  "369", # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  "370", # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  "371", # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
  #  "51", # Neue Ein- und Zweifamilienhäuser
  "367", # Entfernung zur ÖV Haltestelle
  "382", # Verunglückte im Straßenverkehr
  "344", # Kinderarmut
  "528", # Anzahl Kindertagesstätten
  # "218", # Studierende je 100 Einwohner 18 bis 25 Jahre
  "67", # Beschäftigtenquote
  "68", # Beschäftigtenquote Frauen
  "16", # Anteil Arbeitslose Männer zu Gesamtarbeitslose
  ### Only District-level ######################################################
  "303", # Zuweisungen für Investitionsfördermaßnahmen
  "46", # Baulandpreise
  "103", # Beschäftigte Pimärer Sektor
  "104", # Beschäftigte Sekundärer Sektor
  "105", # Beschäftigte Tertiärer Sektor
  "109", # Beschäftigte in Kreativbranchen
  "80", # Beschäftigte am AO mit akademischem Berufsabschluss
  "81", # Beschäftigte am AO mit Berufsabschluss
  "82", # Beschäftigte am AO ohne Berufsabschluss
  "86", # Beschäftigte mit Anforderungsniveau Experte
  "87", # Beschäftigte mit Anforderungsniveau Spezialist
  "88", # Beschäftigte mit Anforderungsniveau Fachkraft
  "89", # Beschäftigte mit Anforderungsniveau Helfer 
  "380", # Ladepunkte je 100 Elektrofahrzeuge
  "374", # Pkw Hybrid insgesamt
  "373", # Pkw Elektro
  "207", # Ausbildungsplätze
  "208", # Auszubildende
  "543", # Frauenanteil im Stadtrat bzw. Kreistag
  "70", # Beschäftigtenquote Ausländer
  "239", # Medianeinkommen 25 bis unter 55-Jährige
  "240", # Medianeinkommen 55 bis unter 65-Jährige
  "243", # Verdienstabstand zwischen Frauen und Männern
  "398", # Bruttoinlandsprodukt je Einwohner
  "46" # Baulandpreise
)

# Execute inkar_multi
kre_list <- inkar_multi(var_vec, num_vec, geo="KRE")

### Voting percentages last three elections ###

# Build vectors
var_vec <- c(
  "Vote_Share_UNION", # Stimmenanteile CDU/CSU
  "Vote_Share_SPD", # Stimmenanteile SPD
  "Vote_Share_Gruene", # Stimmenanteile Grüne
  "Vote_Share_FDP", # Stimmenanteile FDP
  "Vote_Share_Other", # Stimmenanteile Sonstige Parteien
  "Vote_Share_LINKE", # Stimmenanteile Die Linke
  "Vote_Share_AFD" # Stimmenanteile AfD
)

num_vec <- c(
 "195", # Stimmenanteile CDU/CSU
 "196", # Stimmenanteile SPD
 "197", # Stimmenanteile Grüne
 "198", # Stimmenanteile FDP
 "199", # Stimmenanteile Sonstige Parteien
 "200", # Stimmenanteile Die Linke
 "201"  # Stimmenanteile AfD
)

kre_list_vote <- inkar_vote(var_vec, num_vec)

### Create one kre list 
kre_list_final <- c(kre_list, kre_list_vote)

rm(kre_list_vote)

### Get KRE names + additional data ############################################

kre_add <- read_xlsx(path=paste0(path_data, "/INKAR/BBSR_Raumgliederungen_Referenzen_2023.xlsx"), 
                                   sheet="Kreisreferenz", skip=1, .name_repair = "universal") %>%
  mutate(Kreise..2023..Kennziffer = as.numeric(Kreise..2023..Kennziffer)) %>%
  mutate(ID_K = if_else(Kreise..2023..Kennziffer >= 10000000,
                        substr(format(Kreise..2023..Kennziffer, scientific = FALSE, trim=T), 1, 5),
                        substr(format(Kreise..2023..Kennziffer, scientific = FALSE, trim=T), 1, 4))) %>%
  mutate(ID_K = as.numeric(ID_K)) %>%
  rename(Name=Kreise..2023..Name) %>%
  select(ID_K, Name, Kreisfreie.Stadt...Landkreis..2023..Kennziffer, Kreisfreie.Stadt...Landkreis..2023..Name) %>%
  relocate(Name, ID_K)

### Get county identifier ######################################################

# I've downloaded the "Anschriftenverzeichnis 2023" from https://www.statistikportal.de/de/veroeffentlichungen/anschriftenverzeichnis
# and adjusted the worksheet "Anschriften_31_01_2023"
# Data includes areas which are not counties -> Housing data includes all counties

housing_id <- gem_list$data$New_Housing_per_Capita %>%
  select(ID) %>% mutate(ID = as.numeric(ID)) %>% pull()

county_data <- read_excel(path = paste0(path_data, "/Destatis/31122023_Auszug_GV.xlsx"), 
                        sheet = "Onlineprodukt_Gemeinden3112_adj") %>%
  filter(!(is.na(Gem))) %>%
  unite(ID_C, c("Land", "RB", "Kreis", "Gem"), sep="") %>%
  mutate(ID_C = as.numeric(ID_C)) %>%
  mutate(wtf=ifelse(ID_C %in% housing_id,1,0)) %>%
  filter(wtf==1) %>%
  select(!wtf)

### Get GEM names ##############################################################

gem_add <- read_xlsx(path=paste0(path_data, "/INKAR/BBSR_Raumgliederungen_Referenzen_2023.xlsx"), 
                     sheet="Gemeindereferenz (inkl. Kreise)", skip=1, .name_repair = "universal") %>%
  rename(ID= Gemeinden..2023..Kennziffer) %>%
  rename(Name=Gemeinden..2023..Name) %>%
  select(ID, Name) %>%
  mutate(ID = as.numeric(ID)) %>%
  filter(ID %in% housing_id)

### Get rent data ##############################################################

### GEM ###
gem_Rents <- read_xlsx(path = paste0(path_data, "/Destatis/4000W-0004_de.xlsx"),
                       sheet = "4000W-0004_adjusted") %>%
  rename(Rent_NetAvg=Rent.NetAvg) %>%
  mutate(Rent_NetAvg = replace(Rent_NetAvg, Rent_NetAvg=="-", NA)) %>%
  mutate(Rent_NetAvg = replace(Rent_NetAvg, Rent_NetAvg==".", NA)) %>%
  mutate(ID = as.numeric(gsub("^(.{5}).{4}(.*)$", "\\1\\2", ID))) %>%
  filter(ID %in% housing_id) %>%
  select(!(Name))

### Add meta data ###
gem_list$meta <- rbind(
  gem_list$meta,
  data.frame(
    Name = "Durchschnittliche Nettokaltmiete",
    Kurzname = NA,
    Algorithmus = NA,
    Quelle = NA,
    Anmerkungen = NA,
    ID = NA,
    var = "Rent_NetAvg",
    source = "Zensus 2022",
    stringsAsFactors = FALSE
  )
)

### KRE ####
kre_Rents <- read_xlsx(path = paste0(path_data, "/Destatis/4000W-0004_de_kre.xlsx"),
                       sheet = "4000W-0004_adjusted") %>%
  rename(Rent_NetAvg = Rent.NetAvg) %>%
  mutate(ID = as.numeric(ID)) %>%
  select(!(Name))

# Add meta data 
kre_list$meta <- rbind(
  kre_list$meta,
  data.frame(
    Name = "Durchschnittliche Nettokaltmiete",
    Kurzname = NA,
    Algorithmus = NA,
    Quelle = NA,
    Anmerkungen = NA,
    ID = NA,
    var = "Rent_NetAvg",
    source = "Zensus 2022",
    stringsAsFactors = FALSE
  )
)

### Merge INKAR ################################################################

### GEM; Gemeinden ###

# Retrieve meta data
meta_gem <- gem_list$meta
write_xlsx(meta_gem, path=paste0(path_work, "/Data_Info/Meta_gem.xlsx"))
# rm(meta_gem)

# Check number of obs. per list 
(obs_gem_list <- sapply(gem_list$data, nrow))

merged_gem <- gem_list$data %>%
  reduce(left_join, by="ID") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) %>%
  filter(ID %in% housing_id) %>%
  full_join(gem_Rents, by="ID") %>%
  arrange(ID) %>%
  mutate(ID_K = if_else(ID >= 10000000,
                        substr(format(ID, scientific = FALSE, trim=T), 1, 5),
                        substr(format(ID, scientific = FALSE, trim=T), 1, 4))) %>%
  mutate(ID_K = as.numeric(ID_K))

### KRE; Kreise ###

# Retrieve meta data
meta_kre <- kre_list_final$meta
meta_kre_vote <- kre_list_final$meta_vote
meta_kre <- rbind(meta_kre, meta_kre_vote)
write_xlsx(meta_kre, path=paste0(path_work, "/Data_Info/Meta_kre.xlsx"))
rm(meta_kre_vote, meta_kre)

# Check number of obs. per list
(obs_kre_list_final <- sapply(kre_list_final$data, nrow))
(obs_kre_list_final_vote <- sapply(kre_list_final$data_vote, nrow))

# Merge all
merged_kre <- kre_list_final$data %>%
  reduce(left_join, by="ID") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) %>%
  left_join(kre_Rents, by="ID") %>%
  rename(ID_K = ID) %>%
  arrange(ID_K)

# Replace all NAs with zero 
merged_kre[is.na(merged_kre)] <- 0

summary(merged_kre)

### Adjust INKAR data ##########################################################

### Create Share of people equal to 18 and below 65 ###
merged_kre <- merged_kre %>%
  mutate(Age_18_65 = 100 - Age_below_6 - Age_6_18 - Age_65)

merged_gem <- merged_gem %>%
  mutate(Age_18_65 = 100 - Age_below_6 - Age_6_18 - Age_65)

### Impute GEM NAs with KRE data ###

vars_to_fill <- setdiff(names(merged_gem), c("ID", "ID_K"))

merged_filled <- merged_gem %>%
  left_join(
    merged_kre %>%
      select(all_of(c("ID_K", vars_to_fill))) %>%
      rename_with(~ paste0(., "_kre"), vars_to_fill),
    by = "ID_K"
  ) %>%
  mutate(across(all_of(vars_to_fill),
                ~ ifelse(is.na(.x), get(paste0(cur_column(), "_kre")), .x)
  )) %>%
  select(-ends_with("_kre"))

summary(merged_gem)
summary(merged_filled)

### Merge with _add data ###

# GEM data
merged_gem_final <- merged_filled %>%
  left_join(gem_add, by="ID") %>%
  relocate(ID, ID_K, Name)


### Hinweis: Es gab 2023 664 Gemeinden, die sich den Namen mit einer anderen Gemeinde teilten 
n_occur <- data.frame(table(merged_gem$Name))

n_occur[n_occur$Freq > 1,]

# test <- merged_gem[merged_gem$Name %in% n_occur$Var1[n_occur$Freq > 1],] %>%
#  arrange(Name)

summary(merged_gem)

# KRE data
merged_kre_final <- merged_kre %>%
  left_join(kre_add, by="ID_K") %>%
  relocate(ID_K, Name)

### Save INKAR data ############################################################

write_rds(merged_gem_final, file=paste0(path_data, "/Manipulated/merged_gem_final.rds"))
write_csv(merged_gem_final, file=paste0(path_data, "/Manipulated/merged_gem_final.csv"))

write_rds(merged_kre_final, file=paste0(path_data, "/Manipulated/merged_kre_final.rds"))
write_csv(merged_kre_final, file=paste0(path_data, "/Manipulated/merged_kre_final.csv"))

merged_gem_final <- read_rds(paste0(path_data, "/Manipulated/merged_gem_final.rds"))
# merged_kre_final <- read_rds(paste0(path_data, "/Manipulated/merged_kre_final.rds"))


## -----------------------------------------------------------------------------