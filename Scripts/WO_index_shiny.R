## ---------------------------
##
## Script name: WO_index_shiny
##
## Purpose of script: Creates Shiny application for ranking based on user's preferences of 
##                    German districts as the first step and in the second presents the best counties of the best districts 
##
## Author: Gerrit Stahn
##
## Date Created: 2026-02-23
## Last Update: 2026-03-05
##
## Copyright (c) Gerrit Stahn, 2026
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
# install.packages("shiny")

### Load add-on packages ### 
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyalert)

### clean start ###
rm(list = ls())

### set working directory and paths ###
setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties")      
path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"
path_graphs <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Graphs"
path_work <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Work"

## -----------------------------------------------------------------------------
## User interface
## -----------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("RegioIndex"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # --- CATEGORY: AIR QUALITY ---
      sliderInput("air", "Air Pollution (Main)", -10, 0, 0),
      checkboxInput("show_air", "Show Air Pollution Details", FALSE),
      conditionalPanel(
        condition = "input.show_air == true",
        wellPanel(
          sliderInput("no2_avg", "NO2", -10, 0, 0),
          sliderInput("pm25_avg", "PM2.5", -10, 0, 0),
          sliderInput("pm10_avg", "PM10", -10, 0, 0),
          sliderInput("co_avg", "CO", -10, 0, 0),
          sliderInput("so2_avg", "SO2", -10, 0, 0),
          sliderInput("pb_avg", "Lead (Pb)", -10, 0, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: GREEN AREAS ---
      sliderInput("green", "Green Areas (Main)", -10, 10, 0),
      checkboxInput("show_green", "Show Green Area Details", FALSE),
      conditionalPanel(
        condition = "input.show_green == true",
        wellPanel(
          sliderInput("Recreation.Area.per.Capita", "Recreation Area per Capita", -10, 10, 0),
          sliderInput("Forest.Area", "Forest Area", -10, 10, 0),
          sliderInput("Water.Area", "Water Area", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: MOBILITY TRANSITION ---
      sliderInput("mob_trans", "Mobility Transition (Main)", -10, 10, 0),
      checkboxInput("show_mob_trans", "Show Mobility Details", FALSE),
      conditionalPanel(
        condition = "input.show_mob_trans == true",
        wellPanel(
          sliderInput("Charg.Points.per100EV", "Charging Points per 100 EV", -10, 10, 0),
          sliderInput("Share.Car.Hybrid", "Share of Hybrid Cars", -10, 10, 0),
          sliderInput("Share.Car.Electro", "Share of Electric Cars", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: AREAL ASPECTS ---
      sliderInput("areal", "Areal Aspects (Main)", -10, 10, 0),
      checkboxInput("show_areal", "Show Areal Details", FALSE),
      conditionalPanel(
        condition = "input.show_areal == true",
        wellPanel(
          sliderInput("Settlement.Area.in.Flood.Zone", "Settlement in Flood Zone", -10, 10, 0),
          sliderInput("Sealed.Area.per.Capita", "Sealed Area per Capita", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: POPULATION ---
      sliderInput("pop", "Population (Main)", -10, 10, 0),
      checkboxInput("show_pop", "Show Population Details", FALSE),
      conditionalPanel(
        condition = "input.show_pop == true",
        wellPanel(
          sliderInput("Population", "Total Population", -10, 10, 0),
          sliderInput("Population.Density", "Population Density", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: AGE STRUCTURE ---
          sliderInput("Age.below.6", "Age < 6", -10, 10, 0),
          sliderInput("Age.6.18", "Age 6-18", -10, 10, 0),
          sliderInput("Age.18.65", "Age 18-65", -10, 10, 0),
          sliderInput("Age.65", "Age > 65", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: HOUSING & LAND MARKET ---
          sliderInput("New.Housing.per.Capita", "New Housing per Capita", -10, 10, 0),
          sliderInput("Permit.Housing.perCapita", "Housing Permits", -10, 10, 0),
          sliderInput("Land.Price", "Land Price", -10, 10, 0),
          sliderInput("Rent.NetAvg", "Average Net Rent", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: TRANSPORT INFRASTRUCTURE ---
      sliderInput("infra", "Transport Infrastructure (Main)", 0, 10, 0),
      checkboxInput("show_infra", "Show Transport Details", FALSE),
      conditionalPanel(
        condition = "input.show_infra == true",
        wellPanel(
          sliderInput("Highway.Access", "Highway Access", -10, 10, 0),
          sliderInput("Airport.Access", "Airport Access", -10, 10, 0),
          sliderInput("Highspeed.Rail.Access", "Highspeed Rail Access", -10, 10, 0),
          sliderInput("Public.Transport.Access", "Public Transport Access", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: DIGITAL INFRASTRUCTURE ---
      sliderInput("digital", "Digital Infrastructure (Main)", 0, 10, 0),
      checkboxInput("show_digital", "Show Digital Details", FALSE),
      conditionalPanel(
        condition = "input.show_digital == true",
        wellPanel(
          sliderInput("Broadband.50Mbps", "Broadband 50Mbps", 0, 10, 0),
          sliderInput("Broadband.100Mbps", "Broadband 100Mbps", 0, 10, 0),
          sliderInput("Broadband.1000Mbps", "Broadband 1000Mbps", 0, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: RETAIL & BASIC SERVICES ---
      sliderInput("retail", "Retail & Services (Main)", -10, 10, 0),
      checkboxInput("show_retail", "Show Service Details", FALSE),
      conditionalPanel(
        condition = "input.show_retail == true",
        wellPanel(
          sliderInput("Supermarket.Access", "Supermarket Access", -10, 10, 0),
          sliderInput("Doc.GP", "General Practitioners", -10, 10, 0),
          sliderInput("Pharmacy.Access", "Pharmacy Access", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: EDUCATION INFRASTRUCTURE ---
      sliderInput("edu", "Education (Main)", -10, 10, 0),
      checkboxInput("show_edu", "Show Education Details", FALSE),
      conditionalPanel(
        condition = "input.show_edu == true",
        wellPanel(
          sliderInput("School.Primary", "Primary Schools", -10, 10, 0),
          sliderInput("School.SpecialEdu", "Special Education", -10, 10, 0),
          sliderInput("Daycare", "Daycare Quote", -10, 10, 0),
          sliderInput("Apprent.Positions", "Apprenticeship Positions", -10, 10, 0),
          sliderInput("Apprent", "Apprentices", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: SOCIAL STRUCTURE ---
      sliderInput("social", "Social Structure (Main)", -10, 10, 0),
      checkboxInput("show_social", "Show Social Details", FALSE),
      conditionalPanel(
        condition = "input.show_social == true",
        wellPanel(
          sliderInput("Share.Women.Council", "Women in Council", -10, 10, 0),
          sliderInput("Migration.Balance", "Migration Balance", -10, 10, 0),
          sliderInput("Emp.Rate.Women", "Female Employment Rate", -10, 10, 0),
          sliderInput("Emp.Rate.Foreign", "Foreign Employment Rate", -10, 10, 0)
        )
      ),
      sliderInput("Pay.Gap.Gender", "Gender Pay Gap", -10, 10, 0),
      sliderInput("Child.Poverty", "Child Poverty", -10, 0, 0),
      hr(),
      
      # --- CATEGORY: EMPLOYMENT RATES ---
      sliderInput("Emp.Rate", "Total Employment Rate", 0, 10, 0),
      sliderInput("Unemp.Men", "Unemployment Men", -10, 0, 0),
      hr(),
      
      # --- CATEGORY: SECTORAL EMPLOYMENT ---
      sliderInput("sector", "Economic Sectors (Main)", -10, 10, 0),
      checkboxInput("show_sector", "Show Sector Details", FALSE),
      conditionalPanel(
        condition = "input.show_sector == true",
        wellPanel(
          sliderInput("Emp.Primary", "Primary Sector", -10, 10, 0),
          sliderInput("Emp.Secundary", "Secondary Sector", -10, 10, 0),
          sliderInput("Emp.Tertiary", "Tertiary Sector", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: QUALIFICATION STRUCTURE ---
          sliderInput("Emp.AO.Academic", "Academic", -10, 10, 0),
          sliderInput("Emp.AO.Vocational", "Vocational Training", -10, 10, 0),
          sliderInput("Emp.AO.NoTrain", "No Training", -10, 10, 0),
          sliderInput("Emp.Expert", "Experts", -10, 10, 0),
          sliderInput("Emp.Specialist", "Specialists", -10, 10, 0),
          sliderInput("Emp.Professional", "Professionals", -10, 10, 0),
          sliderInput("Emp.Helper", "Helpers", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: ECONOMIC PERFORMANCE ---
      sliderInput("economy", "Economic Performance (Main)", -10, 10, 0),
      checkboxInput("show_economy", "Show Economy Details", FALSE),
      conditionalPanel(
        condition = "input.show_economy == true",
        wellPanel(
          sliderInput("GDP.perCapita", "GDP per Capita", -10, 10, 0),
          sliderInput("Purchasing.Power", "Purchasing Power", -10, 10, 0),
          sliderInput("Income.Median.Age25to54", "Income (25-54)", -10, 10, 0),
          sliderInput("Income.Median.Age55to64", "Income (55-64)", -10, 10, 0),
          sliderInput("Investment.Allocations", "Investment Allocations", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: OTHER ---
          sliderInput("Traffic.Accidents", "Traffic Accidents", -10, 0, 0),
          sliderInput("Emp.Creative", "Creative Industry", -10, 10, 0)
      ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  ),
  
  ### Implement buttons for each specification ###
  actionButton("all", "Alle Kreise"),
  actionButton("sk", "Nur Stadtkreise"), 
  actionButton("lk", "Nur Landkreise"), 
  hr()
  
)

## -----------------------------------------------------------------------------
## Server
## -----------------------------------------------------------------------------

# Nur ein Beispiel für die Server Logik - hier muss codiert werden, was 
# vom Server berechnet werden muss und ausgegeben werden muss. 

# # Define server logic required to draw a Bar chart
server <- function(input, output, session) {
  
  shinyalert("Welcome", "Welcome to RegioIndex - the dashboard helping you to find your favorite place in Germany!", type = "info", showCancelButton = T, confirmButtonText = "Let's start.", cancelButtonText = "How does it work?", imageUrl = "/Users/apxww/Desktop/GitHub/ranking_german_counties/modal_pic.png")
  
  v <- reactiveValues(data = NULL)
  title <- reactiveValues(data = NULL)
  
  # Helper function to update sub-sliders when main slider changes
  bulk_update <- function(main_input, sub_ids) {
    observe({
      for (id in sub_ids) {
        updateSliderInput(session, id, value = main_input())
      }
    }) |> bindEvent(main_input())
  }
  
  # Map main sliders to sub-variables
  bulk_update(reactive(input$air), c("no2_avg", "pm25_avg", "pm10_avg", "co_avg", "so2_avg", "pb_avg"))
  bulk_update(reactive(input$green), c("Recreation.Area.per.Capita", "Forest.Area", "Water.Area"))
  bulk_update(reactive(input$mob_trans), c("Charg.Points.per100EV", "Share.Car.Hybrid", "Share.Car.Electro"))
  bulk_update(reactive(input$areal), c("Settlement.Area.in.Flood.Zone", "Sealed.Area.per.Capita"))
  bulk_update(reactive(input$pop), c("Population", "Population.Density"))
  bulk_update(reactive(input$age), c("Age.below.6", "Age.6.18", "Age.18.65", "Age.65"))
  bulk_update(reactive(input$housing), c("New.Housing.per.Capita", "Permit.Housing.perCapita", "Land.Price", "Rent.NetAvg"))
  bulk_update(reactive(input$infra), c("Highway.Access", "Airport.Access", "Highspeed.Rail.Access", "Public.Transport.Access"))
  bulk_update(reactive(input$digital), c("Broadband.50Mbps", "Broadband.100Mbps", "Broadband.1000Mbps"))
  bulk_update(reactive(input$retail), c("Supermarket.Access", "Doc.GP", "Pharmacy.Access"))
  bulk_update(reactive(input$edu), c("School.Primary", "School.SpecialEdu", "Daycare", "Apprent.Positions", "Apprent"))
  bulk_update(reactive(input$social), c("Child.Poverty", "Share.Women.Council", "Pay.Gap.Gender", "Migration.Balance", "Emp.Rate.Women", "Emp.Rate.Foreign"))
  bulk_update(reactive(input$empl_rate), c("Emp.Rate", "Unemp.Men"))
  bulk_update(reactive(input$sector), c("Emp.Primary", "Emp.Secundary", "Emp.Tertiary"))
  bulk_update(reactive(input$qual), c("Emp.AO.Academic", "Emp.AO.Vocational", "Emp.AO.NoTrain", "Emp.Expert", "Emp.Specialist", "Emp.Professional", "Emp.Helper"))
  bulk_update(reactive(input$economy), c("GDP.perCapita", "Purchasing.Power", "Income.Median.Age25to54", "Income.Median.Age55to64", "Investment.Allocations"))
  bulk_update(reactive(input$other), c("Traffic.Accidents", "Emp.Creative"))
  
  ### All districts ###
  observeEvent(input$all, {
    # Load data
    normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds"))
    
    # Weights
    weights <- c(
      # --- Air Pollution (Environmental Quality) ---
      "no2_avg"          =  (input$no2_avg) / 10,
      "pm25_avg"         =  (input$pm25_avg) / 10,
      "pm10_avg"         =  (input$pm10_avg) / 10,
      "co_avg"           =  (input$co_avg) / 10,
      "so2_avg"          =  (input$so2_avg) / 10,
      "pb_avg"           =  (input$pb_avg) / 10,
      
      # --- Green Areas ---
      "Recreation.Area.per.Capita" = (input$Recreation.Area.per.Capita) / 10,
      "Forest.Area"                = (input$Forest.Area) / 10,
      "Water.Area"                 = (input$Water.Area) / 10,
      
      # --- Mobility Transition ---
      "Charg.Points.per100EV" = (input$Charg.Points.per100EV) / 10,
      "Share.Car.Hybrid"      = (input$Share.Car.Hybrid) / 10,
      "Share.Car.Electro"     = (input$Share.Car.Electro) / 10,
      
      # --- Areal Aspects ---
      "Settlement.Area.in.Flood.Zone" =  (input$Settlement.Area.in.Flood.Zone) / 10,
      "Sealed.Area.per.Capita"        =  (input$Sealed.Area.per.Capita) / 10,
      
      # --- Population Size ---
      "Population"         = (input$Population) / 10,
      "Population.Density" =  (input$Population.Density) / 10,
      
      # --- Age Structure ---
      "Age.below.6" = (input$Age.below.6) / 10,
      "Age.6.18"    = (input$Age.6.18) / 10,
      "Age.18.65"   = (input$Age.18.65) / 10,
      "Age.65"      =  (input$Age.65) / 10, # Negative weight common for high dependency ratio
      
      # --- Housing & Land Market ---
      "New.Housing.per.Capita"    = (input$New.Housing.per.Capita) / 10,
      "Permit.Housing.perCapita"  = (input$Permit.Housing.perCapita) / 10,
      "Land.Price"                =  (input$Land.Price) / 10, # Often seen as a cost/barrier
      "Rent.NetAvg"               =  (input$Rent.NetAvg) / 10, # Higher rent usually lowers index score
      
      # --- Transport Infrastructure ---
      "Highway.Access"         = (input$Highway.Access) / 10,
      "Airport.Access"         = (input$Airport.Access) / 10,
      "Highspeed.Rail.Access"  = (input$Highspeed.Rail.Access) / 10,
      "Public.Transport.Access"= (input$Public.Transport.Access) / 10,
      
      # --- Digital Infrastructure ---
      "Broadband.50Mbps"   = (input$Broadband.50Mbps) / 10,
      "Broadband.100Mbps"  = (input$Broadband.100Mbps) / 10,
      "Broadband.1000Mbps" = (input$Broadband.1000Mbps) / 10,
      
      # --- Retail & Basic Services ---
      "Supermarket.Access" = (input$Supermarket.Access) / 10,
      "Doc.GP"             = (input$Doc.GP) / 10,
      "Pharmacy.Access"    = (input$Pharmacy.Access) / 10,
      
      # --- Education Infrastructure ---
      "School.Primary"     = (input$School.Primary) / 10,
      "School.SpecialEdu"  = (input$School.SpecialEdu) / 10,
      "Daycare"            = (input$Daycare) / 10,
      "Apprent.Positions"  = (input$Apprent.Positions) / 10,
      "Apprent"            = (input$Apprent) / 10,
      
      # --- Social Structure ---
      "Child.Poverty"      =  (input$Child.Poverty) / 10,
      "Share.Women.Council" = (input$Share.Women.Council) / 10,
      "Pay.Gap.Gender"     =  (input$Pay.Gap.Gender) / 10,
      "Migration.Balance"  = (input$Migration.Balance) / 10,
      "Emp.Rate.Women"     = (input$Emp.Rate.Women) / 10,
      "Emp.Rate.Foreign"   = (input$Emp.Rate.Foreign) / 10,
      
      # --- Employment Rates ---
      "Emp.Rate"   = (input$Emp.Rate) / 10,
      "Unemp.Men"  =  (input$Unemp.Men) / 10,
      
      # --- Sectoral Employment ---
      "Emp.Primary"   = (input$Emp.Primary) / 10,
      "Emp.Secundary" = (input$Emp.Secundary) / 10,
      "Emp.Tertiary"  = (input$Emp.Tertiary) / 10,
      
      # --- Qualification Structure ---
      "Emp.AO.Academic"   = (input$Emp.AO.Academic) / 10,
      "Emp.AO.Vocational" = (input$Emp.AO.Vocational) / 10,
      "Emp.AO.NoTrain"    =  (input$Emp.AO.NoTrain) / 10,
      "Emp.Expert"        = (input$Emp.Expert) / 10,
      "Emp.Specialist"    = (input$Emp.Specialist) / 10,
      "Emp.Professional"  = (input$Emp.Professional) / 10,
      "Emp.Helper"        = (input$Emp.Helper) / 10,
      
      # --- Economic Performance ---
      "GDP.perCapita"            = (input$GDP.perCapita) / 10,
      "Purchasing.Power"         = (input$Purchasing.Power) / 10,
      "Income.Median.Age25to54"  = (input$Income.Median.Age25to54) / 10,
      "Income.Median.Age55to64"  = (input$Income.Median.Age55to64) / 10,
      "Investment.Allocations"   = (input$Investment.Allocations) / 10,
      
      # --- Other ---
      "Traffic.Accidents" =  (input$Traffic.Accidents) / 10,
      "Emp.Creative"      = (input$Emp.Creative) / 10
    )
    
    top_synth_kreis <- sum(1* unlist(weights))
    
    # Ensures the correct order
    order <- names(weights)
    
    # All K: Create index data
    n <- length(names(normalized_data))
    
    names_all <- normalized_data %>%
      dplyr::select(c("Name", "ID_K"))
    
    index <- normalized_data %>%
      relocate(all_of(order), .after="Name") %>%
      rowwise() %>%
      mutate(
        Index = round(((sum(c_across(5:n) * unlist(weights)))/top_synth_kreis)*100),
        .keep = "unused"
      ) %>%
      ungroup() %>%
      dplyr::select(c("ID_K", "Index")) %>%
      left_join(names_all, by="ID_K") %>%
      arrange(desc(Index))
    
    v$data <- index %>%
      arrange(desc(Index)) %>%
      slice(1:20)
    
    title$data <- "Top 20 Kreise"
  })
  
  ### Only Stadtkreise ###
  observeEvent(input$sk, {
    # Load data
    normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds"))
    
    ### Weights ###
    weights <- c(
      # --- Air Pollution (Environmental Quality) ---
      "no2_avg"          =  (input$no2_avg) / 10,
      "pm25_avg"         =  (input$pm25_avg) / 10,
      "pm10_avg"         =  (input$pm10_avg) / 10,
      "co_avg"           =  (input$co_avg) / 10,
      "so2_avg"          =  (input$so2_avg) / 10,
      "pb_avg"           =  (input$pb_avg) / 10,
      
      # --- Green Areas ---
      "Recreation.Area.per.Capita" = (input$Recreation.Area.per.Capita) / 10,
      "Forest.Area"                = (input$Forest.Area) / 10,
      "Water.Area"                 = (input$Water.Area) / 10,
      
      # --- Mobility Transition ---
      "Charg.Points.per100EV" = (input$Charg.Points.per100EV) / 10,
      "Share.Car.Hybrid"      = (input$Share.Car.Hybrid) / 10,
      "Share.Car.Electro"     = (input$Share.Car.Electro) / 10,
      
      # --- Areal Aspects ---
      "Settlement.Area.in.Flood.Zone" =  (input$Settlement.Area.in.Flood.Zone) / 10,
      "Sealed.Area.per.Capita"        =  (input$Sealed.Area.per.Capita) / 10,
      
      # --- Population Size ---
      "Population"         = (input$Population) / 10,
      "Population.Density" =  (input$Population.Density) / 10,
      
      # --- Age Structure ---
      "Age.below.6" = (input$Age.below.6) / 10,
      "Age.6.18"    = (input$Age.6.18) / 10,
      "Age.18.65"   = (input$Age.18.65) / 10,
      "Age.65"      =  (input$Age.65) / 10, # Negative weight common for high dependency ratio
      
      # --- Housing & Land Market ---
      "New.Housing.per.Capita"    = (input$New.Housing.per.Capita) / 10,
      "Permit.Housing.perCapita"  = (input$Permit.Housing.perCapita) / 10,
      "Land.Price"                =  (input$Land.Price) / 10, # Often seen as a cost/barrier
      "Rent.NetAvg"               =  (input$Rent.NetAvg) / 10, # Higher rent usually lowers index score
      
      # --- Transport Infrastructure ---
      "Highway.Access"         = (input$Highway.Access) / 10,
      "Airport.Access"         = (input$Airport.Access) / 10,
      "Highspeed.Rail.Access"  = (input$Highspeed.Rail.Access) / 10,
      "Public.Transport.Access"= (input$Public.Transport.Access) / 10,
      
      # --- Digital Infrastructure ---
      "Broadband.50Mbps"   = (input$Broadband.50Mbps) / 10,
      "Broadband.100Mbps"  = (input$Broadband.100Mbps) / 10,
      "Broadband.1000Mbps" = (input$Broadband.1000Mbps) / 10,
      
      # --- Retail & Basic Services ---
      "Supermarket.Access" = (input$Supermarket.Access) / 10,
      "Doc.GP"             = (input$Doc.GP) / 10,
      "Pharmacy.Access"    = (input$Pharmacy.Access) / 10,
      
      # --- Education Infrastructure ---
      "School.Primary"     = (input$School.Primary) / 10,
      "School.SpecialEdu"  = (input$School.SpecialEdu) / 10,
      "Daycare"            = (input$Daycare) / 10,
      "Apprent.Positions"  = (input$Apprent.Positions) / 10,
      "Apprent"            = (input$Apprent) / 10,
      
      # --- Social Structure ---
      "Child.Poverty"      =  (input$Child.Poverty) / 10,
      "Share.Women.Council" = (input$Share.Women.Council) / 10,
      "Pay.Gap.Gender"     =  (input$Pay.Gap.Gender) / 10,
      "Migration.Balance"  = (input$Migration.Balance) / 10,
      "Emp.Rate.Women"     = (input$Emp.Rate.Women) / 10,
      "Emp.Rate.Foreign"   = (input$Emp.Rate.Foreign) / 10,
      
      # --- Employment Rates ---
      "Emp.Rate"   = (input$Emp.Rate) / 10,
      "Unemp.Men"  =  (input$Unemp.Men) / 10,
      
      # --- Sectoral Employment ---
      "Emp.Primary"   = (input$Emp.Primary) / 10,
      "Emp.Secundary" = (input$Emp.Secundary) / 10,
      "Emp.Tertiary"  = (input$Emp.Tertiary) / 10,
      
      # --- Qualification Structure ---
      "Emp.AO.Academic"   = (input$Emp.AO.Academic) / 10,
      "Emp.AO.Vocational" = (input$Emp.AO.Vocational) / 10,
      "Emp.AO.NoTrain"    =  (input$Emp.AO.NoTrain) / 10,
      "Emp.Expert"        = (input$Emp.Expert) / 10,
      "Emp.Specialist"    = (input$Emp.Specialist) / 10,
      "Emp.Professional"  = (input$Emp.Professional) / 10,
      "Emp.Helper"        = (input$Emp.Helper) / 10,
      top_synth_kreis <- sum(1* unlist(weights))
    )
      
      # Ensures the correct order
      order <- names(weights)
      
      # Create index data
      n <- length(names(normalized_data))
      
      names_all <- normalized_data %>%
        dplyr::select(c("Name", "ID_K"))
      
      index <- normalized_data %>%
        relocate(all_of(order), .after="Name") %>%
        rowwise() %>%
        mutate(
          Index = round(((sum(c_across(5:n) * unlist(weights)))/top_synth_kreis)*100),
          .keep = "unused"
        ) %>%
        ungroup() %>%
        dplyr::select(c("ID_K", "Index")) %>%
        left_join(names_all, by="ID_K") %>%
        arrange(desc(Index))
      
      v$data <- index %>%
        arrange(desc(Index)) %>%
        slice(1:20)
      
      title$data <- "Top 20 Urban Districts"
  })
    
    ### Only Rural Districts ###
    observeEvent(input$lk, {
      
      # Load data
      normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds"))
      
      # Weights
      weights <- c(
        # --- Air Pollution (Environmental Quality) ---
        "no2_avg"          =  (input$no2_avg) / 10,
        "pm25_avg"         =  (input$pm25_avg) / 10,
        "pm10_avg"         =  (input$pm10_avg) / 10,
        "co_avg"           =  (input$co_avg) / 10,
        "so2_avg"          =  (input$so2_avg) / 10,
        "pb_avg"           =  (input$pb_avg) / 10,
        
        # --- Green Areas ---
        "Recreation.Area.per.Capita" = (input$Recreation.Area.per.Capita) / 10,
        "Forest.Area"                = (input$Forest.Area) / 10,
        "Water.Area"                 = (input$Water.Area) / 10,
        
        # --- Mobility Transition ---
        "Charg.Points.per100EV" = (input$Charg.Points.per100EV) / 10,
        "Share.Car.Hybrid"      = (input$Share.Car.Hybrid) / 10,
        "Share.Car.Electro"     = (input$Share.Car.Electro) / 10,
        
        # --- Areal Aspects ---
        "Settlement.Area.in.Flood.Zone" =  (input$Settlement.Area.in.Flood.Zone) / 10,
        "Sealed.Area.per.Capita"        =  (input$Sealed.Area.per.Capita) / 10,
        
        # --- Population Size ---
        "Population"         = (input$Population) / 10,
        "Population.Density" =  (input$Population.Density) / 10,
        
        # --- Age Structure ---
        "Age.below.6" = (input$Age.below.6) / 10,
        "Age.6.18"    = (input$Age.6.18) / 10,
        "Age.18.65"   = (input$Age.18.65) / 10,
        "Age.65"      =  (input$Age.65) / 10, # Negative weight common for high dependency ratio
        
        # --- Housing & Land Market ---
        "New.Housing.per.Capita"    = (input$New.Housing.per.Capita) / 10,
        "Permit.Housing.perCapita"  = (input$Permit.Housing.perCapita) / 10,
        "Land.Price"                =  (input$Land.Price) / 10, # Often seen as a cost/barrier
        "Rent.NetAvg"               =  (input$Rent.NetAvg) / 10, # Higher rent usually lowers index score
        
        # --- Transport Infrastructure ---
        "Highway.Access"         = (input$Highway.Access) / 10,
        "Airport.Access"         = (input$Airport.Access) / 10,
        "Highspeed.Rail.Access"  = (input$Highspeed.Rail.Access) / 10,
        "Public.Transport.Access"= (input$Public.Transport.Access) / 10,
        
        # --- Digital Infrastructure ---
        "Broadband.50Mbps"   = (input$Broadband.50Mbps) / 10,
        "Broadband.100Mbps"  = (input$Broadband.100Mbps) / 10,
        "Broadband.1000Mbps" = (input$Broadband.1000Mbps) / 10,
        
        # --- Retail & Basic Services ---
        "Supermarket.Access" = (input$Supermarket.Access) / 10,
        "Doc.GP"             = (input$Doc.GP) / 10,
        "Pharmacy.Access"    = (input$Pharmacy.Access) / 10,
        
        # --- Education Infrastructure ---
        "School.Primary"     = (input$School.Primary) / 10,
        "School.SpecialEdu"  = (input$School.SpecialEdu) / 10,
        "Daycare"            = (input$Daycare) / 10,
        "Apprent.Positions"  = (input$Apprent.Positions) / 10,
        "Apprent"            = (input$Apprent) / 10,
        
        # --- Social Structure ---
        "Child.Poverty"      =  (input$Child.Poverty) / 10,
        "Share.Women.Council" = (input$Share.Women.Council) / 10,
        "Pay.Gap.Gender"     =  (input$Pay.Gap.Gender) / 10,
        "Migration.Balance"  = (input$Migration.Balance) / 10,
        "Emp.Rate.Women"     = (input$Emp.Rate.Women) / 10,
        "Emp.Rate.Foreign"   = (input$Emp.Rate.Foreign) / 10,
        
        # --- Employment Rates ---
        "Emp.Rate"   = (input$Emp.Rate) / 10,
        "Unemp.Men"  =  (input$Unemp.Men) / 10,
        
        # --- Sectoral Employment ---
        "Emp.Primary"   = (input$Emp.Primary) / 10,
        "Emp.Secundary" = (input$Emp.Secundary) / 10,
        "Emp.Tertiary"  = (input$Emp.Tertiary) / 10,
        
        # --- Qualification Structure ---
        "Emp.AO.Academic"   = (input$Emp.AO.Academic) / 10,
        "Emp.AO.Vocational" = (input$Emp.AO.Vocational) / 10,
        "Emp.AO.NoTrain"    =  (input$Emp.AO.NoTrain) / 10,
        "Emp.Expert"        = (input$Emp.Expert) / 10,
        "Emp.Specialist"    = (input$Emp.Specialist) / 10,
        "Emp.Professional"  = (input$Emp.Professional) / 10,
        "Emp.Helper"        = (input$Emp.Helper) / 10,
        
        # --- Economic Performance ---
        "GDP.perCapita"            = (input$GDP.perCapita) / 10,
        "Purchasing.Power"         = (input$Purchasing.Power) / 10,
        "Income.Median.Age25to54"  = (input$Income.Median.Age25to54) / 10,
        "Income.Median.Age55to64"  = (input$Income.Median.Age55to64) / 10,
        "Investment.Allocations"   = (input$Investment.Allocations) / 10,
        
        # --- Other ---
        "Traffic.Accidents" =  (input$Traffic.Accidents) / 10,
        "Emp.Creative"      = (input$Emp.Creative) / 10
      )
      
      top_synth_kreis <- sum(1* unlist(weights))
      
      # Ensures the correct order
      order <- names(weights)
      
      # Create index data
      n <- length(names(normalized_data))
      
      names_all <- normalized_data %>%
        dplyr::select(c("Name", "ID_K"))
      
      index <- normalized_data %>%
        relocate(all_of(order), .after="Name") %>%
        rowwise() %>%
        mutate(
          Index = round(((sum(c_across(5:n) * unlist(weights)))/top_synth_kreis)*100),
          .keep = "unused"
        ) %>%
        ungroup() %>%
        dplyr::select(c("ID_K", "Index")) %>%
        left_join(names_all, by="ID_K") %>%
        arrange(desc(Index))
      
      v$data <- index %>%
        arrange(desc(Index)) %>%
        slice(1:20)
      
      title$data <- "Top 20 Rural Districts"
    })
    
    output$barPlot <- renderPlot({
      
      if (is.null(v$data)) return()
      
      ggplot(v$data, aes(x = reorder(Name, Index), y = Index)) +
        geom_col(fill = "blue") +
        geom_hline(yintercept=100)+
        coord_flip() +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), 
                           labels=c("0", "25", "50", "75", "100\n (Your ideal district)"),
                           expand=expansion(mult=c(0,0.15)))+
        labs(title = title$data, x = "District", y = "Your Quality of Life Index") +
        theme_minimal(base_size = 16) +
        theme(
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black")
        )
      
    })
    
}

## -----------------------------------------------------------------------------
## Call to shinyApp
## -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

## -----------------------------------------------------------------------------