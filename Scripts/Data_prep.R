## ---------------------------
##
## Script name: Data_prep
##
## Purpose of script: Prep data for ranking_german_counties project
##
## Author: Gerrit Stahn
##
## Date Created: 2025-10-21
## Last Update: 2025-10-21
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
## Download INKAR data
## -----------------------------------------------------------------------------

### Functions ##################################################################
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

### Geography: GEM (Gemeinden) #################################################
# get_geographies()
get_themes(geography = "GEM")
get_variables(theme="131", geography = "GEM")
get_metadata("363")

### Build vectors ###
var_vec <- c(
  "Population", # Bevölkerung gesamt
#  "Elg.Workers", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "New.Housing.per.Capita", # Neubauwohnungen je Einwohner
  "Permit.Housing.perCapita", # Baugenehmigungen für Wohnungenje Einwohner
  "Age.below.6", # Einwohner unter 6 Jahren
  "Age.6.18", # Einwohner von 6 bis unter 18 Jahren
  "Age.65", # Einwohner 65 Jahre und älter
  "School.Primary", # Grundschulen
  "School.SpecialEdu", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "Migration.Balance", # Gesamtwanderungssaldo
  "Purchasing.Power", # Kaufkraft
  "Recreation.Area.per.Capita", # Erholungsfläche je Einwohner
  "Forest.Area", # Waldfläche
  "Water.Area", # Wasserfläche
  'Population.Density', # Einwohnerdichte
  'Highway.Access', # Erreichbarkeit von Autobahnen
  'Airport.Access', # Erreichbarkeit von Flughäfen
  'Highspeed.Rail.Access', # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  'Supermarket.Access', # Entfernung zum Supermarkt/Discounter
  'Doc.GP', # Entfernung zum Hausarzt
  "Pharmacy.Access", # Entfernung nächste Apotheke
  'Broadband.50Mbps', # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  'Broadband.100Mbps', # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  'Broadband.1000Mbps', # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
#  "New.Family.Houses", # Neue Ein- und Zweifamilienhäuser
  'Public.Transport.Access', # Entfernung zur ÖV Haltestelle
  'Traffic.Accidents', # Verunglückte im Straßenverkehr
  'Child.Poverty', # Kinderarmut
  'Daycare', # Anzahl Kindertagesstätten
# 'Students.18.to.25', # Studierende je 100 Einwohner 18 bis 25 Jahre
  "Emp.Rate", # Beschäftigtenquote
  "Emp.Rate.Women", # Beschäftigtenquote Frauen
  "Unemp.Men" # Anteil Arbeitslose Männer zu Gesamtarbeitslose
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

### Execute inkar_multi ###
gem_list <- inkar_multi(var_vec, num_vec, geo="GEM")

### Geography: KRE (Kreis) #####################################################

# get_geographies()
get_themes(geography = "KRE")
get_variables(theme="131", geography = "KRE")
get_metadata("496")

### Build vectors ###
var_vec <- c(
  ### Same variables as for GEM ################################################
  "Population", # Bevölkerung gesamt
  #  "Elg.Workers", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "New.Housing.per.Capita", # Neubauwohnungen je Einwohner
  "Permit.Housing.perCapita", # Baugenehmigungen für Wohnungenje Einwohner
  "Age.below.6", # Einwohner unter 6 Jahren
  "Age.6.18", # Einwohner von 6 bis unter 18 Jahren
  "Age.65", # Einwohner 65 Jahre und älter
  "School.Primary", # Grundschulen
  "School.SpecialEdu", # Allgemeinbildende Schulen mit Förderschwerpunkt
  "Migration.Balance", # Gesamtwanderungssaldo
  "Purchasing.Power", # Kaufkraft
  "Recreation.Area.per.Capita", # Erholungsfläche je Einwohner
  "Forest.Area", # Waldfläche
  "Water.Area", # Wasserfläche
  'Population.Density', # Einwohnerdichte
  'Highway.Access', # Erreichbarkeit von Autobahnen
  'Airport.Access', # Erreichbarkeit von Flughäfen
  'Highspeed.Rail.Access', # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  'Supermarket.Access', # Entfernung zum Supermarkt/Discounter
  'Doc.GP', # Entfernung zum Hausarzt
  "Pharmacy.Access", # Entfernung nächste Apotheke
  'Broadband.50Mbps', # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  'Broadband.100Mbps', # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  'Broadband.1000Mbps', # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
#  "New.Family.Houses", # Neue Ein- und Zweifamilienhäuser
  'Public.Transport.Access', # Entfernung zur ÖV Haltestelle
  'Traffic.Accidents', # Verunglückte im Straßenverkehr
  'Child.Poverty', # Kinderarmut
  'Daycare', # Anzahl Kindertagesstätten
  # 'Students.18.to.25', # Studierende je 100 Einwohner 18 bis 25 Jahre
  #  'Doctors', # Hausärzte
  "Emp.Rate", # Beschäftigtenquote
  "Emp.Rate.Women", # Beschäftigtenquote Frauen
  "Unemp.Men", # Anteil Arbeitslose Männer zu Gesamtarbeitslose
  ### Only District-level ######################################################
  "Investment.Allocations", # Zuweisungen für Investitionsfördermaßnahmen
  "Land.Price", # Baulandpreise
  "Emp.Primary", # Beschäftigte Pimärer Sektor
  "Emp.Secundary", # Beschäftigte Sekundärer Sektor
  "Emp.Tertiary", # Beschäftigte Tertiärer Sektor
  "Emp.Creative", # Beschäftigte in Kreativbranchen
  "Emp.AO.Academic", # Beschäftigte am AO mit akademischem Berufsabschluss
  "Emp.AO.Vocational", # Beschäftigte am AO mit Berufsabschluss
  "Emp.AO.NoTrain", # Beschäftigte am AO ohne Berufsabschluss
  "Emp.Expert", # Beschäftigte mit Anforderungsniveau Experte
  "Emp.Specialist", # Beschäftigte mit Anforderungsniveau Spezialist
  "Emp.Professional", # Beschäftigte mit Anforderungsniveau Fachkraft
  "Emp.Helper", # Beschäftigte mit Anforderungsniveau Helfer 
  "Charg.Points.per100EV", # Ladepunkte je 100 Elektrofahrzeuge
  "Share.Car.Hybrid", # Pkw Hybrid insgesamt
  "Share.Car.Electro", # Pkw Elektro
  "Apprent.Positions", # Ausbildungsplätze
  "Apprent", # Auszubildende
  "Share.Women.Council", # Frauenanteil im Stadtrat bzw. Kreistag
  "Emp.Rate.Foreign", # Beschäftigtenquote Ausländer
  "Income.Median.Age25to54", # Medianeinkommen 25 bis unter 55-Jährige
  "Income.Median.Age55to64", # Medianeinkommen 55 bis unter 65-Jährige
  "Pay.Gap.Gender", # Verdienstabstand zwischen Frauen und Männern
  "GDP.perCapita", # Bruttoinlandsprodukt je Einwohner
  "Land.Price" # Baulandpreise
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

### Execute inkar_multi ###
kre_list <- inkar_multi(var_vec, num_vec, geo="KRE")

### Stimmenanteile der letzten drei Wahlen #####################################

### Build vectors ###
var_vec <- c(
  "Vote.Share.UNION", # Stimmenanteile CDU/CSU
  "Vote.Share.SPD", # Stimmenanteile SPD
  "Vote.Share.Gruene", # Stimmenanteile Grüne
  "Vote.Share.FDP", # Stimmenanteile FDP
  "Vote.Share.Other", # Stimmenanteile Sonstige Parteien
  "Vote.Share.LINKE", # Stimmenanteile Die Linke
  "Vote.Share.AFD" # Stimmenanteile AfD
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

## -----------------------------------------------------------------------------
## Get KRE names + additional data
## -----------------------------------------------------------------------------

kre_add <- read_xlsx(path=paste0(path_data, "/INKAR/BBSR_Raumgliederungen_Referenzen_2022.xlsx"), 
                                   sheet="Kreisreferenz", skip=1, .name_repair = "universal")%>%
  rename(ID= Kreise..2022..Kennziffer) %>%
  rename(Name=Kreise..2022..Name) %>%
  mutate(ID = as.numeric(ID))

## -----------------------------------------------------------------------------
## Get county identifier
## -----------------------------------------------------------------------------

# I've downloaded the "Anschriftenverzeichnis 2023" from https://www.statistikportal.de/de/veroeffentlichungen/anschriftenverzeichnis
# and adjusted the worksheet "Anschriften_31_01_2023"
# Data includes areas which are not counties -> Housing data includes all counties

housing_id <- gem_list$data$New.Housing.per.Capita %>%
  select(ID) %>% mutate(ID = as.numeric(ID)) %>% pull()

county_data <- read_excel(path = paste0(path_data, "/Destatis/31122023_Auszug_GV.xlsx"), 
                        sheet = "Onlineprodukt_Gemeinden3112_adj") %>%
  filter(!(is.na(Gem))) %>%
  unite(ID_C, c("Land", "RB", "Kreis", "Gem"), sep="") %>%
  mutate(ID_C = as.numeric(ID_C)) %>%
  mutate(wtf=ifelse(ID_C %in% housing_id,1,0)) %>%
  filter(wtf==1) %>%
  select(!wtf)

## -----------------------------------------------------------------------------
## Get GEM names + additional data
## -----------------------------------------------------------------------------

gem_add <- read_xlsx(path=paste0(path_data, "/INKAR/BBSR_Raumgliederungen_Referenzen_2022.xlsx"), 
                     sheet="Gemeindereferenz (inkl. Kreise)", skip=1, .name_repair = "universal")%>%
  rename(ID= Gemeinden..2022..Kennziffer) %>%
  rename(Name=Gemeinden..2022..Name) %>%
  select(!(Gemeinden..2022..Regionalschlüssel)) %>%
  mutate(ID = as.numeric(ID)) %>%
  filter(ID %nin% housing_id)

## -----------------------------------------------------------------------------
## Get rent data
## -----------------------------------------------------------------------------

### GEM ########################################################################
gem_Rents <- read_xlsx(path = paste0(path_data, "/Destatis/4000W-0004_de.xlsx"),
                       sheet = "4000W-0004_adjusted") %>%
  mutate(Rent.NetAvg = replace(Rent.NetAvg, Rent.NetAvg=="-", NA)) %>%
  mutate(Rent.NetAvg = replace(Rent.NetAvg, Rent.NetAvg==".", NA)) %>%
  mutate(ID = as.numeric(gsub("^(.{5}).{4}(.*)$", "\\1\\2", ID))) %>%
  filter(ID %in% housing_id) %>%
  select(!(Name))

#### Nächster Step: 
#   - alles mergen 
#   - bei GEM Daten die ID der Kreise extrahieren (siehe Wikipedia)
#####

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
    var = "Rent.NetAvg",
    source = "Zensus 2022",
    stringsAsFactors = FALSE
  )
)

### KRE ########################################################################
kre_Rents <- read_xlsx(path = paste0(path_data, "/Destatis/4000W-0004_de_kre.xlsx"),
                       sheet = "4000W-0004_adjusted") %>%
  mutate(ID = as.numeric(ID)) %>%
  select(!(Name))

### Add meta data ###
kre_list$meta <- rbind(
  kre_list$meta,
  data.frame(
    Name = "Durchschnittliche Nettokaltmiete",
    Kurzname = NA,
    Algorithmus = NA,
    Quelle = NA,
    Anmerkungen = NA,
    ID = NA,
    var = "Rent.NetAvg",
    source = "Zensus 2022",
    stringsAsFactors = FALSE
  )
)

## -----------------------------------------------------------------------------
## Merge
## -----------------------------------------------------------------------------

### GEM; Gemeinden #############################################################

### Retrieve meta data ###
meta_gem <- gem_list$meta
write_xlsx(meta_gem, path=paste0(path_work, "/Data_Info/Meta_gem.xlsx"))
# rm(meta_gem)

### Check number of obs. per list ###
(obs_gem_list <- sapply(gem_list$data, nrow))

### Folgendes eventuell löschen ################################################
# ### Merge all with the same number of obs. ###
# common_n <- names(obs_gem_list[obs_gem_list == 10775])
# lst_same <- gem_list$data[common_n]
# 
# merged_gem_d1 <- lst_same %>% 
#   reduce(left_join, by="ID") %>%
#   select(-ends_with(".y")) %>%
#   select(-ends_with(".x")) 
# 
# ### Merge all with number of obs. bigger than 10775 ###
# bigger_n <- names(obs_gem_list[obs_gem_list > 10775])
# lst_bigger <- gem_list$data[bigger_n]
# 
# # Keep only obs. from housing_id    
# lst_bigger <- lapply(lst_bigger, function(df) {
#   df %>% filter(ID %in% housing_id)
# })
# 
# #common_n <- names(obs_gem_list[obs_gem_list == 10775])
# #lst_same <- gem_list$data[common_n]
# 
# merged_gem_d2 <- lst_bigger %>% 
#   reduce(left_join, by="ID") %>%
#   select(-ends_with(".y")) %>%
#   select(-ends_with(".x"))
# 
# ### Merge gem data together ###
# merged_gem <- merged_gem_d1 %>%
#   left_join(merged_gem_d2, by="ID")
# 
# summary(merged_gem)
# 
# ### Merge all with number of obs. lower than 10775 ###
# lower_n <- names(obs_gem_list[obs_gem_list < 10775])
# lst_lower <- gem_list$data[lower_n]
################################################################################

merged_gem <- gem_list$data %>%
  reduce(left_join, by="ID") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) %>%
  filter(ID %in% housing_id) %>%
  full_join(gem_Rents, by="ID") 

summary(merged_gem)

### KRE; Kreise (Code funktioniert nicht) ######################################

# Retrieve meta data
meta_kre <- kre_list_final$meta
meta_kre_vote <- kre_list_final$meta_vote
meta_kre <- rbind(meta_kre, meta_kre_vote)
write_xlsx(meta_kre, path=paste0(path_work, "/Data_Info/Meta_kre.xlsx"))
rm(meta_kre_vote, meta_kre)

### Check number of obs. per list ###
(obs_kre_list_final <- sapply(kre_list_final$data, nrow))
(obs_kre_list_final_vote <- sapply(kre_list_final$data_vote, nrow))

### Merge all ###
merged_kre <- kre_list_final$data %>%
  reduce(left_join, by="ID") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x"))

test <- merged_kre %>%
  filter(is.na(Emp.Helper))

summary(merged_kre)


merged_kre_df_d2 <- lst_same %>% 
  reduce(left_join, by="ID") %>%
  select(-ends_with(".y")) %>%
  select(-ends_with(".x")) 

merged_kre_df <- full_join(merged_kre_df_d1, merged_kre_df_d2, by="ID")
rm(merged_kre_df_d1, merged_kre_df_d2)

## -----------------------------------------------------------------------------
## Adjust
## -----------------------------------------------------------------------------

# Create Share of people equal to 18 and below 65

## -----------------------------------------------------------------------------