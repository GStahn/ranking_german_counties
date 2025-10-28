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
# install.packages("tidyverse") # for dplyr, readxl and tidyr
# install.packages("collapse")
# install.packages("purr")

### Load add-on packages ### 
library(bonn)
library(dplyr)
library(readxl)
library(tidyr)
library(collapse)
library(purr)

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
## Download INKAR data
## -----------------------------------------------------------------------------

### Functions ###
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
      rename(ID=Schlüssel) %>%
      rename({{ var }} := Wert) %>%
      select(c("ID", "Raumbezug", {{var}}, "Time"))
    data_list[[var]] <- df
    
    
    meta_append <- get_metadata(num)
    meta_append$var <- var
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
      rename(ID=Schlüssel) %>%
      rename({{ var }} := Wert)
    data_list[[var]] <- df
    
    meta_append <- get_metadata(num)
    meta_append$var <- var
    meta <- rbind(meta, meta_append)
  }
  
  return(list(
    data = data_list,
    meta = meta
  ))
}

### Geography: GEM (Gemeinden) #################################################
# get_geographies()
get_themes(geography = "GEM")
get_variables(theme="021", geography = "GEM")
get_metadata("51")

### Build vectors ###
var_vec <- c(
  "Population", # Bevölkerung gesamt
  "Elg.Workers", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "New.Housing.per.Capita", # Neubauwohnungen je Einwohner
  "Elderly.Population", # Einwohner 65 Jahre und älter
  "Migration.Balance", # Gesamtwanderungssaldo
  "Purchasing.Power", # Kaufkraft
  "Recreation.Area.per.Capita", # Erholungsfläche je Einwohner
  "Forest.Area", # Waldfläche
  "Water.Area", # Wasserfläche
  "Investment.Allocations", # Zuweisungen für Investitionsfördermaßnahmen
  'Population.Density', # Einwohnerdichte
  'Highway.Access', # Erreichbarkeit von Autobahnen
  'Airport.Access', # Erreichbarkeit von Flughäfen
  'Highspeed.Rail.Access', # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  'Supermarket.Access', # Entfernung zum Supermarkt/Discounter
  'Pharmacy.Access', # Entfernung zur Apotheke
  'Broadband.50Mbps', # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  'Broadband.100Mbps', # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  'Broadband.1000Mbps', # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
  "New.Family.Houses", # Neue Ein- und Zweifamilienhäuser
  'Public.Transport.Access', # Entfernung zur ÖV Haltestelle
  'Traffic.Accidents', # Verunglückte im Straßenverkehr
  'Child.Poverty', # Kinderarmut
  'Daycare', # Anzahl Kindertagesstätten
  'Students.18.to.25', # Studierende je 100 Einwohner 18 bis 25 Jahre
  'Doctors', # Hausärzte
  "Emp.Rate", # Beschäftigtenquote
  "Emp.Rate.Women", # Beschäftigtenquote Frauen
  "Unemp.Men" # Anteil Arbeitslose Männer zu Gesamtarbeitslose
)

num_vec <- c(
  "2", # Bevölkerung gesamt
  "6", # Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre) 
  "53", # Neubauwohnungen je Einwohner
  "129", # Einwohner 65 Jahre und älter
  "162", # Gesamtwanderungssaldo
  "248", # Kaufkraft
  "258", # Erholungsfläche je Einwohner
  "264", # Waldfläche 
  "265", # Wasserfläche
  "303", # Zuweisungen für Investitionsfördermaßnahmen
  "320", # Einwohnerdichte
  "354", # Erreichbarkeit von Autobahnen
  "355", # Erreichbarkeit von Flughäfen
  "356", # Erreichbarkeit von IC/EC/ICE-Bahnhöfen
  "359", # Entfernung zum Supermarkt/Discounter
  "362", # Entfernung zur Apotheke
  "369", # Bandbreitenverfügbarkeit mindestens 50 Mbit/s
  "370", # Bandbreitenverfügbarkeit mindestens 100 Mbit/s
  "371", # Bandbreitenverfügbarkeit mindestens 1000 Mbit/s
  "51", # Neue Ein- und Zweifamilienhäuser
  "367", # Entfernung zur ÖV Haltestelle
  "382", # Verunglückte im Straßenverkehr
  "344", # Kinderarmut
  "528", # Anzahl Kindertagesstätten
  "218", # Studierende je 100 Einwohner 18 bis 25 Jahre
  "276", # Hausärzte
  "67", # Beschäftigtenquote
  "68", # Beschäftigtenquote Frauen
  "16" # Anteil Arbeitslose Männer zu Gesamtarbeitslose
)

### Execute inkar_multi ###
gem_list <- inkar_multi(var_vec, num_vec, geo="GEM")

### Geography: KRE (Kreis) #####################################################

# get_geographies()
get_themes(geography = "KRE")
get_variables(theme="045", geography = "KRE")
get_metadata("492")

### Build vectors ###
var_vec <- c(
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
## Merge
## -----------------------------------------------------------------------------

### Check number of obs. per list ###
(obs_gem_list <- sapply(gem_list$data, nrow))
obs_kre_list <- sapply(kre_list$data, nrow)
obs_kre_list_vote <- sapply(kre_list_vote$data, nrow)

## -----------------------------------------------------------------------------