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
# install.packages("dplyr")

### Load add-on packages ### 
library(bonn)
library(dplyr)

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

### Overview ###
# get_geographies()
get_themes(geography = "GEM")
get_variables(theme="155", geography = "GEM")
get_metadata("492")

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
  'Doctors' # Hausärzte
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
  "492" # Hausärzte
)

### Execute inkar_multi ###
data_list <- inkar_multi(var_vec, num_vec, geo="GEM")

## -----------------------------------------------------------------------------