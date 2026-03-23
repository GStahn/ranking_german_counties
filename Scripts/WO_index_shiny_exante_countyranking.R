## ---------------------------
##
## Script name: WO_index_shiny_exante_countyranking
##
## Purpose of script: Creates a Shiny application for ranking German districts
##                    based on the user's preferences. In a first step, the app
##                    ranks districts, and in a second step presents the best
##                    municipalities within the best-ranked districts.
##
## exante_countyranking: The county data is already normalized across all 
##                       German counties.
##
## Author: Gerrit Stahn
##
## Date Created: 2026-02-23
## Last Update: 2026-03-20
##
## Copyright (c) Gerrit Stahn, 2026
## Email: gerrit.stahn@wiwi.uni-halle.de
##
## ---------------------------
##
## Notes:
##
## The variable "Apprent" (apprentices per 1,000 employees subject to social
## insurance contributions) was collected and coded, but commented out because
## it overlaps conceptually with "Apprent_Positions" (ratio of apprenticeship
## seekers to open apprenticeship positions).
##
## "Unemp_Men" (share of men among all unemployed persons) was also collected
## and coded, but commented out, since it does not really translate in life quality
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
library(shinyhelper)
library(shinyBS)
library(plotly)

### Clean start ###
rm(list = ls())

### Set working directory and paths ###
setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties")
path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"
path_graphs <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Graphs"
path_work <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Work"

## -----------------------------------------------------------------------------
## User interface
## -----------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .tooltip-inner {
        max-width: 400px;
        width: 400px;
        text-align: left;
        white-space: normal;
      }
    "))
  ),
  
  titlePanel("RegioIndex"),
  
  hr(),
  
  ### Button for the app explanation ###
  actionButton("how", "How does RegioIndex work?",
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # --- CATEGORY: AIR QUALITY ---
      sliderInput("air", "Air Pollution", -10, 0, 0),
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
          sliderInput("Recreation_Area_per_Capita", "Recreation Area per Capita", -10, 10, 0),
          sliderInput("Forest_Area", "Forest Area", -10, 10, 0),
          sliderInput("Water_Area", "Water Area", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: MOBILITY TRANSITION ---
      sliderInput("mob_trans", "Mobility Transition (Main)", -10, 10, 0),
      checkboxInput("show_mob_trans", "Show Mobility Details", FALSE),
      conditionalPanel(
        condition = "input.show_mob_trans == true",
        wellPanel(
          sliderInput("Charg_Points_per100EV", "Charging Points per 100 EV", -10, 10, 0),
          sliderInput("Share_Car_Hybrid", "Share of Hybrid Cars", -10, 10, 0),
          sliderInput("Share_Car_Electro", "Share of Electric Cars", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: SPATIAL ASPECTS ---
      sliderInput("areal", "Spatial Aspects (Main)", -10, 10, 0),
      checkboxInput("show_areal", "Show Spatial Details", FALSE),
      conditionalPanel(
        condition = "input.show_areal == true",
        wellPanel(
          sliderInput("Settlement_Area_in_Flood_Zone", "Settlement Area in Flood Zone", -10, 10, 0),
          sliderInput("Sealed_Area_per_Capita", "Sealed Area per Capita", -10, 10, 0)
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
          sliderInput("Population_Density", "Population Density", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: AGE STRUCTURE ---
      sliderInput("Age_below_6", "Age < 6", -10, 10, 0),
      sliderInput("Age_6_18", "Age 6–18", -10, 10, 0),
      sliderInput("Age_18_65", "Age 18–65", -10, 10, 0),
      sliderInput("Age_65", "Age > 65", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: HOUSING & LAND MARKET ---
      sliderInput("New_Housing_per_Capita", "New Housing per Capita", -10, 10, 0),
      sliderInput("Permit_Housing_perCapita", "Housing Permits", -10, 10, 0),
      sliderInput("Land_Price", "Land Price", -10, 10, 0),
      sliderInput("Rent_NetAvg", "Average Net Rent", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: TRANSPORT INFRASTRUCTURE ---
      sliderInput("infra", "Transport Infrastructure (Main)", 0, 10, 0),
      checkboxInput("show_infra", "Show Transport Details", FALSE),
      conditionalPanel(
        condition = "input.show_infra == true",
        wellPanel(
          sliderInput("Highway_Access", "Highway Access", -10, 10, 0),
          sliderInput("Airport_Access", "Airport Access", -10, 10, 0),
          sliderInput("Highspeed_Rail_Access", "High-Speed Rail Access", -10, 10, 0),
          sliderInput("Public_Transport_Access", "Public Transport Access", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: DIGITAL INFRASTRUCTURE ---
      sliderInput("digital", "Digital Infrastructure (Main)", 0, 10, 0),
      checkboxInput("show_digital", "Show Digital Details", FALSE),
      conditionalPanel(
        condition = "input.show_digital == true",
        wellPanel(
          sliderInput("Broadband_50Mbps", "Broadband 50 Mbps", 0, 10, 0),
          sliderInput("Broadband_100Mbps", "Broadband 100 Mbps", 0, 10, 0),
          sliderInput("Broadband_1000Mbps", "Broadband 1000 Mbps", 0, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: RETAIL & BASIC SERVICES ---
      sliderInput("retail", "Retail & Services (Main)", -10, 10, 0),
      checkboxInput("show_retail", "Show Service Details", FALSE),
      conditionalPanel(
        condition = "input.show_retail == true",
        wellPanel(
          sliderInput("Supermarket_Access", "Supermarket Access", -10, 10, 0),
          sliderInput("Doc_GP", "General Practitioners", -10, 10, 0),
          sliderInput("Pharmacy_Access", "Pharmacy Access", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: EDUCATION INFRASTRUCTURE ---
      sliderInput("edu", "Education (Main)", -10, 10, 0),
      checkboxInput("show_edu", "Show Education Details", FALSE),
      conditionalPanel(
        condition = "input.show_edu == true",
        wellPanel(
          sliderInput("School_Primary", "Primary Schools", -10, 10, 0),
          sliderInput("School_SpecialEdu", "Special Education Schools", -10, 10, 0),
          sliderInput("Daycare", "Daycare Coverage", -10, 10, 0),
          sliderInput("Apprent_Positions", "Apprenticeship Positions", -10, 10, 0) #,
          # sliderInput("Apprent", "Apprentices", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: SOCIAL STRUCTURE ---
      sliderInput("social", "Social Structure (Main)", -10, 10, 0),
      checkboxInput("show_social", "Show Social Details", FALSE),
      conditionalPanel(
        condition = "input.show_social == true",
        wellPanel(
          sliderInput("Share_Women_Council", "Women in Local Councils", -10, 10, 0),
          sliderInput("Migration_Balance", "Net Migration", -10, 10, 0),
          sliderInput("Emp_Rate_Women", "Female Employment Rate", -10, 10, 0),
          sliderInput("Emp_Rate_Foreign", "Foreign Employment Rate", -10, 10, 0)
        )
      ),
      sliderInput("Pay_Gap_Gender", "Gender Pay Gap", -10, 10, 0),
      sliderInput("Child_Poverty", "Child Poverty", -10, 0, 0),
      hr(),
      
      # --- CATEGORY: EMPLOYMENT RATES ---
      sliderInput("Emp_Rate", "Overall Employment Rate", 0, 10, 0),
      # sliderInput("Unemp_Men", "Male Unemployment", -10, 0, 0),
      hr(),
      
      # --- CATEGORY: SECTORAL EMPLOYMENT ---
      sliderInput("sector", "Economic Sectors (Main)", -10, 10, 0),
      checkboxInput("show_sector", "Show Sector Details", FALSE),
      conditionalPanel(
        condition = "input.show_sector == true",
        wellPanel(
          sliderInput("Emp_Primary", "Primary Sector", -10, 10, 0),
          sliderInput("Emp_Secundary", "Secondary Sector", -10, 10, 0),
          sliderInput("Emp_Tertiary", "Tertiary Sector", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: QUALIFICATION STRUCTURE ---
      sliderInput("Emp_AO_Academic", "Academic Qualification", -10, 10, 0),
      sliderInput("Emp_AO_Vocational", "Vocational Training", -10, 10, 0),
      sliderInput("Emp_AO_NoTrain", "No Formal Training", -10, 10, 0),
      sliderInput("Emp_Expert", "Experts", -10, 10, 0),
      sliderInput("Emp_Specialist", "Specialists", -10, 10, 0),
      sliderInput("Emp_Professional", "Skilled Professionals", -10, 10, 0),
      sliderInput("Emp_Helper", "Helpers", -10, 10, 0),
      hr(),
      
      # --- CATEGORY: ECONOMIC PERFORMANCE ---
      sliderInput("economy", "Economic Performance (Main)", -10, 10, 0),
      checkboxInput("show_economy", "Show Economic Details", FALSE),
      conditionalPanel(
        condition = "input.show_economy == true",
        wellPanel(
          sliderInput("GDP_perCapita", "GDP per Capita", -10, 10, 0),
          sliderInput("Purchasing_Power", "Purchasing Power", -10, 10, 0),
          sliderInput("Income_Median_Age25to54", "Income (Age 25–54)", -10, 10, 0),
          sliderInput("Income_Median_Age55to64", "Income (Age 55–64)", -10, 10, 0),
          sliderInput("Investment_Allocations", "Investment Funding", -10, 10, 0)
        )
      ),
      hr(),
      
      # --- CATEGORY: OTHER ---
      sliderInput("Traffic_Accidents", "Traffic Accidents", -10, 0, 0),
      sliderInput("Emp_Creative", "Creative Industries", -10, 10, 0),
      
      hr(),
      
      # --- Buttons for district selection ---
      actionButton("all", "All Districts"),
      actionButton("sk", "Urban Districts Only"),
      actionButton("lk", "Rural Districts Only"),
      
      # --- bsTooltips ---
      bsTooltip("Age_18_65",
                "Share of the population aged 18 to 65.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Age_6_18",
                "Children and teenagers: share of the population aged 6 to 18.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Age_65",
                "Older population: Share of residents aged 65 and above.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Age_below_6",
                "Young children: Share of the population under age 6.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Airport_Access",
                "Average travel time by car to the nearest international airport in Germany, measured in minutes. Negative weights favor districts with a smaller average travel time.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("all",
                "This button calculates the RegioIndex across all German rural and urban districts.",
                placement = "right",
                trigger = "hover"),
      
      # bsTooltip("Apprent",
      #          "Apprentices per 1,000 employees subject to social insurance contributions: This indicator captures entry opportunities into the labour market through vocational training.",
      #          placement = "right", trigger = "hover"),
      
      bsTooltip("Apprent_Positions",
                "Total number of company-based apprenticeship positions per 100 apprenticeship seekers. This variable indicates how easy it is for young people to find a training place.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("areal",
                "Captures (usually) less favourable spatial conditions, such as land sealing or settlement areas located in flood-prone zones. negative weights for this category (and its variables) increasingly favors districts with relatively smaller amounts of such area.”",
                placement = "right", trigger = "hover"),
      
      bsTooltip("air",
                "Evaluates air quality in a region. A negative weight for this category (and each variable within) favours districts with cleaner air and lower pollution levels.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Broadband_1000Mbps",
                "Share of households with internet access of at least 1000 Mbps.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Broadband_100Mbps",
                "Share of households with internet access of at least 100 Mbps.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Broadband_50Mbps",
                "Share of households with internet access of at least 50 Mbps.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Charg_Points_per100EV",
                "Charging points per 100 electric vehicles: Shows how well charging infrastructure is developed relative to the local electric vehicle stock.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Child_Poverty",
                "Share of children living in households receiving basic income support (Bürgergeld/Grundsicherung).",
                placement = "right", trigger = "hover"),
      
      bsTooltip("co_avg",
                "Average carbon monoxide concentration (CO) in the air. Negative weights value ditricts with a lesser environmental burden through CO",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Daycare",
                "Daycare provision: Share of children with access to a childcare place.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("digital",
                "Evaluates the digital infrastructure of a region, especially broadband coverage. A Higher weight for this category (and each variable within) favors better digital connectivity.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Doc_GP",
                "Number of General practitioners relative to the population.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("edu",
                "Describes the availability of educational infrastructure such as schools, daycare facilities, and apprenticeship opportunities. A higher weight for this category (and each variable within) signals a stronger preference educational infrastructure.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("economy",
                "Measures the economic performance of a region, including purchasing power, income, GDP, and investment. Higher values indicate stronger economic performance.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_AO_Academic",
                "Share of employees with a university degree among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_AO_NoTrain",
                "Share of employees without formal qualifications among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_AO_Vocational",
                "Share of employees with completed vocational training among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Creative",
                "Share of employees working in creative sectors among all employees subject to social insurance contributions. This serves as a proxy for the cultural offer of a district.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Expert",
                "Share of employees in highly qualified occupations among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Helper",
                "Share of employees in low-skilled occupations among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Primary",
                "Share of employees working in agriculture, forestry, and fishing among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Professional",
                "Share of employees in occupations requiring intermediate qualification levels among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Rate",
                "Overall employment rate: Share of employed persons in the working-age population.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Rate_Foreign",
                "Share of employed persons with foreign citizenship.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Rate_Women",
                "Share of women who are employed.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Secundary",
                "Share of employees working in manufacturing and construction among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Specialist",
                "Share of employees in specialised occupations among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Emp_Tertiary",
                "Share of employees working in services among all employees subject to social insurance contributions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Forest_Area",
                "Share of forest land in the total area of the region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("GDP_perCapita",
                "Economic output per resident.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("green",
                "Describes access to green, forest, and water areas. A higher weight for this category (and each variable within) indicate greater preference for recreational potential and more nature-based quality of life.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Highspeed_Rail_Access",
                "Average travel time by car to the nearest intercity or high-speed rail station, measured in minutes.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Highway_Access",
                "Average travel time by car to the nearest motorway access point, measured in minutes.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Income_Median_Age25to54",
                "Median income, age 25–54: Typical income level of the core working-age population.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Income_Median_Age55to64",
                "Median income, age 55–64: Typical income level closer to retirement.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("infra",
                "Measures access to transport infrastructure such as motorways, airports, rail, and public transport. A negative weight for this category (and each variable within) corresponds to districts being favoris with lower travel times by car.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Investment_Allocations",
                "This variable measures how much public investment funding a district receives per resident.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Land_Price",
                "Average price per square metre of building land.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("lk",
                "This button calculates the RegioIndex for all German rural districts only, excluding urban districts.",
                placement = "right",
                trigger = "hover"),
      
      bsTooltip("Migration_Balance",
                "Net migration: difference between in-migration and out-migration in a region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("mob_trans",
                "Captures aspects of the mobility transition, such as electric mobility and charging infrastructure. A higher weight fo rthis category (and each variable within) favors region being further advanced in this transition.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("New_Housing_per_Capita",
                "This variable shows how many new homes are built relative to the population and serves as a proxy for housing supply in a region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("no2_avg",
                "Average nitrogen dioxide (NO2) concentration in the air. Higher weights value ditricts with a lesser environmental burden through NO2.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("pb_avg",
                "Average lead concentration in the air. Negative weights value ditricts with a lesser environmental burden through lead.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Pay_Gap_Gender",
                "Median income of full-time employed women relative to that of full-time employed men. This variable measures the gender pay gap and serves as an indicator of economic gender equality.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Permit_Housing_perCapita",
                "Housing permits per capita: Approved dwellings relative to the population, serving as an indicator of future construction activity and therefore housing supply.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Pharmacy_Access",
                "Average travel time by car to the nearest pharmacy.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("pm10_avg",
                "Average particulate matter concentration (PM10). Negative weights value ditricts with a lesser environmental burden through PM10.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("pm25_avg",
                "Average finer particulate matter concentration (PM2.5). Negative weights value ditricts with a lesser environmental burden through PM2.5.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("pop",
                "Accounts for the size and density of the population. Higher weights tend to favour more urbanised and densely populated areas.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Population",
                "Total number of residents in a region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Population_Density",
                "Residents per square kilometre. Indicates how densely settled a region is.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Public_Transport_Access",
                "Average travel time by car to the nearest public transport stop.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Purchasing_Power",
                "Disposable income per resident available for consumption and saving after taxes, social security contributions, and other deductions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Recreation_Area_per_Capita",
                "Space available for leisure and recreation per capita. A higher weight means better access to green and leisure spaces.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Rent_NetAvg",
                "Average net cold rent for residential housing.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("retail",
                "Captures local basic services, such as access to supermarkets, doctors, and pharmacies.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("School_Primary",
                "Number of primary schools in a region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("School_SpecialEdu",
                "Number of schools with a special educational focus.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("sector",
                "Shows the economic structure of a region based on employment across different sectors.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Sealed_Area_per_Capita",
                "Describes the amount of sealed land per resident. Lower weights favor dsirtcits with less land sealing.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Settlement_Area_in_Flood_Zone",
                "Share of built-up land located in potentially flood-prone areas. Lower weights favor dsirtcits with flood-prone areas.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Share_Car_Electro",
                "Share of fully electric passenger cars in the total vehicle stock.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Share_Car_Hybrid",
                "Share of hybrid cars in the total passenger car stock.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Share_Women_Council",
                "Share of women in local councils: This variable reflects political representation and gender equality in a region.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("sk",
                "This button calculates the RegioIndex for urban districts only.",
                placement = "right",
                trigger = "hover"),
      
      bsTooltip("social",
                "Captures social structures such as equal opportunities, integration, and social participation. Higher weights generally favor districts with stronger social conditions.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("so2_avg",
                "Average sulphur dioxide SO2 concentration in the air. Negative weights increasingly value ditricts with a lesser environmental burden through SO2",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Supermarket_Access",
                "Average travel time by car to the nearest supermarket.",
                placement = "right", trigger = "hover"),
      
      bsTooltip("Traffic_Accidents",
                "Traffic casualties per 100,000 residents: This variable primarily reflects road safety in a region.",
                placement = "right", trigger = "hover"),
      
      # bsTooltip("Unemp_Men",
      #           "Share of unemployed men among all unemployed persons, in percent.",
      #           placement = "right", trigger = "hover"),
      
      bsTooltip("Water_Area",
                "Share of water bodies in the total area of a region.",
                placement = "right", trigger = "hover")
    ),
    
    mainPanel(
      plotlyOutput("barPlot", height = "700px"),
      hr(),
      h4(textOutput("county_title")),
      plotOutput("countyPlot", height = "700px")
    )
  )
)

## -----------------------------------------------------------------------------
## Server
## -----------------------------------------------------------------------------

# # Define server logic required to draw a bar chart
server <- function(input, output, session) {
  
  shinyalert("Welcome", "Welcome to RegioIndex - the dashboard that helps you find your favourite place in Germany!", showCancelButton = F, confirmButtonText = "Let's go!",
             imageUrl = "https://raw.githubusercontent.com/GStahn/ranking_german_counties/refs/heads/main/modal_pic_small.png",
             imageWidth=375, imageHeight = 250, animation = "slide-from-bottom", closeOnEsc= F)
  
  observe_helpers()
  
  selected_district <- reactiveVal(NULL)
  
  # observeEvent(event_data("plotly_click", source = "district_click"), {
  #   click_data <- event_data("plotly_click", source = "district_click")
  #   
  #   if (is.null(click_data)) return()
  #   
  #   clicked_name <- as.character(click_data$y)
  #   
  #   selected_row <- v$data %>%
  #     dplyr::filter(Name == clicked_name) %>%
  #     slice(1)
  #   
  #   if (nrow(selected_row) == 1) {
  #     selected_district(selected_row)
  #   }
  # })
  
  observeEvent(input$how, {
    
    shinyalert(
      title = "How does RegioIndex work?",
      text = HTML("
      <div style='text-align:left; line-height:1.4;'>
        <p><b>RegioIndex</b> helps you compare rural and urban cities in Germany based on
        <b>how well they match your personal preferences</b>. You decide which factors matter most to you,
        and the app calculates an individually tailored ranking.</p>

        <hr/>

        <h4 style='margin-bottom:6px;'>1) How to use the app</h4>
        <ul>
          <li>On the left, use the <b>sliders</b> to set how important different topics are to you (e.g. environment, infrastructure, economy).</li>
          <li>Scale: <b>-10</b> (the less, the better), <b>0</b> (not relevant), <b>+10</b> (very important).</li>
          <li>With <b>Show Details</b>, you can adjust the variables of a category separately (e.g. NO<sub>2</sub>, PM2.5, public transport, broadband).</li>
          <li>At the top, choose whether you want to compare <b>All Districts</b>, only <b>Urban Districts</b>, or only <b>Rural Districts</b>.</li>
        </ul>

        <h4 style='margin-bottom:6px;'>2) What you get as output</h4>
        <ul>
          <li>A bar chart showing the <b>Top 20 regions</b> that best match your preferences.</li>
          <li>Each region receives a <b>RegioIndex score</b> between <b>0</b> and <b>100</b>.</li>
          <li><b>100</b> represents a theoretical “ideal” district that would perfectly match your preferred profile.</li>
        </ul>

        <h4 style='margin-bottom:6px;'>3) How the RegioIndex is roughly calculated</h4>
        <ol>
          <li>All indicators are first <b>normalised</b> so they can be compared.</li>
          <li>Your slider values are used as <b>weights</b> (from -10 to +10, internally rescaled).</li>
          <li>A <b>weighted sum</b> is then calculated for each region and scaled to a <b>0–100</b> range.</li>
        </ol>

        <p style='margin-top:10px;'>
          <i>Tip:</i> If you leave many sliders at 0, those factors will not affect the ranking.
          Try different weights to quickly see which regions fit different lifestyles.
        </p>
      </div>
    "),
      confirmButtonText = "Got it – let’s begin.",
      closeOnEsc = TRUE,
      html = T
    )
    
  })
  
  v <- reactiveValues(data = NULL)
  title <- reactiveValues(data = NULL)
  
  weights <- reactive({
    c(
      # --- Air Pollution (Environmental Quality) ---
      "no2_avg"          = (input$no2_avg) / 10,
      "pm25_avg"         = (input$pm25_avg) / 10,
      "pm10_avg"         = (input$pm10_avg) / 10,
      "co_avg"           = (input$co_avg) / 10,
      "so2_avg"          = (input$so2_avg) / 10,
      "pb_avg"           = (input$pb_avg) / 10,
      
      # --- Green Areas ---
      "Recreation_Area_per_Capita" = (input$Recreation_Area_per_Capita) / 10,
      "Forest_Area"                = (input$Forest_Area) / 10,
      "Water_Area"                 = (input$Water_Area) / 10,
      
      # --- Mobility Transition ---
      "Charg_Points_per100EV" = (input$Charg_Points_per100EV) / 10,
      "Share_Car_Hybrid"      = (input$Share_Car_Hybrid) / 10,
      "Share_Car_Electro"     = (input$Share_Car_Electro) / 10,
      
      # --- Spatial Aspects ---
      "Settlement_Area_in_Flood_Zone" = (input$Settlement_Area_in_Flood_Zone) / 10,
      "Sealed_Area_per_Capita"        = (input$Sealed_Area_per_Capita) / 10,
      
      # --- Population Size ---
      "Population"         = (input$Population) / 10,
      "Population_Density" = (input$Population_Density) / 10,
      
      # --- Age Structure ---
      "Age_below_6" = (input$Age_below_6) / 10,
      "Age_6_18"    = (input$Age_6_18) / 10,
      "Age_18_65"   = (input$Age_18_65) / 10,
      "Age_65"      = (input$Age_65) / 10,
      
      # --- Housing & Land Market ---
      "New_Housing_per_Capita"   = (input$New_Housing_per_Capita) / 10,
      "Permit_Housing_perCapita" = (input$Permit_Housing_perCapita) / 10,
      "Land_Price"               = (input$Land_Price) / 10,
      "Rent_NetAvg"              = (input$Rent_NetAvg) / 10,
      
      # --- Transport Infrastructure ---
      "Highway_Access"          = (input$Highway_Access) / 10,
      "Airport_Access"          = (input$Airport_Access) / 10,
      "Highspeed_Rail_Access"   = (input$Highspeed_Rail_Access) / 10,
      "Public_Transport_Access" = (input$Public_Transport_Access) / 10,
      
      # --- Digital Infrastructure ---
      "Broadband_50Mbps"   = (input$Broadband_50Mbps) / 10,
      "Broadband_100Mbps"  = (input$Broadband_100Mbps) / 10,
      "Broadband_1000Mbps" = (input$Broadband_1000Mbps) / 10,
      
      # --- Retail & Basic Services ---
      "Supermarket_Access" = (input$Supermarket_Access) / 10,
      "Doc_GP"             = (input$Doc_GP) / 10,
      "Pharmacy_Access"    = (input$Pharmacy_Access) / 10,
      
      # --- Education Infrastructure ---
      "School_Primary"    = (input$School_Primary) / 10,
      "School_SpecialEdu" = (input$School_SpecialEdu) / 10,
      "Daycare"           = (input$Daycare) / 10,
      "Apprent_Positions" = (input$Apprent_Positions) / 10,
      
      # --- Social Structure ---
      "Child_Poverty"       = (input$Child_Poverty) / 10,
      "Share_Women_Council" = (input$Share_Women_Council) / 10,
      "Pay_Gap_Gender"      = (input$Pay_Gap_Gender) / 10,
      "Migration_Balance"   = (input$Migration_Balance) / 10,
      "Emp_Rate_Women"      = (input$Emp_Rate_Women) / 10,
      "Emp_Rate_Foreign"    = (input$Emp_Rate_Foreign) / 10,
      
      # --- Employment Rates ---
      "Emp_Rate" = (input$Emp_Rate) / 10,
      
      # --- Sectoral Employment ---
      "Emp_Primary"   = (input$Emp_Primary) / 10,
      "Emp_Secundary" = (input$Emp_Secundary) / 10,
      "Emp_Tertiary"  = (input$Emp_Tertiary) / 10,
      
      # --- Qualification Structure ---
      "Emp_AO_Academic"   = (input$Emp_AO_Academic) / 10,
      "Emp_AO_Vocational" = (input$Emp_AO_Vocational) / 10,
      "Emp_AO_NoTrain"    = (input$Emp_AO_NoTrain) / 10,
      "Emp_Expert"        = (input$Emp_Expert) / 10,
      "Emp_Specialist"    = (input$Emp_Specialist) / 10,
      "Emp_Professional"  = (input$Emp_Professional) / 10,
      "Emp_Helper"        = (input$Emp_Helper) / 10,
      
      # --- Economic Performance ---
      "GDP_perCapita"           = (input$GDP_perCapita) / 10,
      "Purchasing_Power"        = (input$Purchasing_Power) / 10,
      "Income_Median_Age25to54" = (input$Income_Median_Age25to54) / 10,
      "Income_Median_Age55to64" = (input$Income_Median_Age55to64) / 10,
      "Investment_Allocations"  = (input$Investment_Allocations) / 10,
      
      # --- Other ---
      "Traffic_Accidents" = (input$Traffic_Accidents) / 10,
      "Emp_Creative"      = (input$Emp_Creative) / 10
    )
  })
  
  # Helper function to update sub-sliders when the main slider changes
  bulk_update <- function(main_input, sub_ids) {
    observe({
      for (id in sub_ids) {
        updateSliderInput(session, id, value = main_input())
      }
    }) |> bindEvent(main_input())
  }
  
  # Map main sliders to sub-variables
  bulk_update(reactive(input$air), c("no2_avg", "pm25_avg", "pm10_avg", "co_avg", "so2_avg", "pb_avg"))
  bulk_update(reactive(input$green), c("Recreation_Area_per_Capita", "Forest_Area", "Water_Area"))
  bulk_update(reactive(input$mob_trans), c("Charg_Points_per100EV", "Share_Car_Hybrid", "Share_Car_Electro"))
  bulk_update(reactive(input$areal), c("Settlement_Area_in_Flood_Zone", "Sealed_Area_per_Capita"))
  bulk_update(reactive(input$pop), c("Population", "Population_Density"))
  bulk_update(reactive(input$age), c("Age_below_6", "Age_6_18", "Age_18_65", "Age_65"))
  bulk_update(reactive(input$housing), c("New_Housing_per_Capita", "Permit_Housing_perCapita", "Land_Price", "Rent_NetAvg"))
  bulk_update(reactive(input$infra), c("Highway_Access", "Airport_Access", "Highspeed_Rail_Access", "Public_Transport_Access"))
  bulk_update(reactive(input$digital), c("Broadband_50Mbps", "Broadband_100Mbps", "Broadband_1000Mbps"))
  bulk_update(reactive(input$retail), c("Supermarket_Access", "Doc_GP", "Pharmacy_Access"))
  bulk_update(reactive(input$edu), c("School_Primary", "School_SpecialEdu", "Daycare", "Apprent_Positions")) #, "Apprent"))
  bulk_update(reactive(input$social), c("Child_Poverty", "Share_Women_Council", "Pay_Gap_Gender", "Migration_Balance", "Emp_Rate_Women", "Emp_Rate_Foreign"))
  bulk_update(reactive(input$empl_rate), c("Emp_Rate")) #, "Unemp_Men"))
  bulk_update(reactive(input$sector), c("Emp_Primary", "Emp_Secundary", "Emp_Tertiary"))
  bulk_update(reactive(input$qual), c("Emp_AO_Academic", "Emp_AO_Vocational", "Emp_AO_NoTrain", "Emp_Expert", "Emp_Specialist", "Emp_Professional", "Emp_Helper"))
  bulk_update(reactive(input$economy), c("GDP_perCapita", "Purchasing_Power", "Income_Median_Age25to54", "Income_Median_Age55to64", "Investment_Allocations"))
  bulk_update(reactive(input$other), c("Traffic_Accidents", "Emp_Creative"))
  
  ### All districts ###
  observeEvent(input$all, {
    normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds")) %>%
      select(-Unemp_Men, -Apprent)
    
    current_weights <- weights()
    top_synth_kreis <- sum(unlist(current_weights))
    order_vars <- names(current_weights)
    n <- length(names(normalized_data))
    
    names_all <- normalized_data %>%
      dplyr::select(Name, ID_K)
    
    index <- normalized_data %>%
      relocate(all_of(order_vars), .after = "Name") %>%
      rowwise() %>%
      mutate(
        Index = round(((sum(c_across(5:n) * unlist(current_weights))) / top_synth_kreis) * 100),
        .keep = "unused"
      ) %>%
      ungroup() %>%
      dplyr::select(ID_K, Index) %>%
      left_join(names_all, by = "ID_K") %>%
      arrange(desc(Index))
    
    v$data <- index %>%
      slice(1:20)
    
    title$data <- "Top 20 Districts"
  })
  
  ### Only urban districts ###
  observeEvent(input$sk, {
    normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds")) %>%
      select(-Unemp_Men, -Apprent)
    
    current_weights <- weights()
    top_synth_kreis <- sum(unlist(current_weights))
    order_vars <- names(current_weights)
    n <- length(names(normalized_data))
    
    names_all <- normalized_data %>%
      dplyr::select(Name, ID_K)
    
    index <- normalized_data %>%
      relocate(all_of(order_vars), .after = "Name") %>%
      rowwise() %>%
      mutate(
        Index = round(((sum(c_across(5:n) * unlist(current_weights))) / top_synth_kreis) * 100),
        .keep = "unused"
      ) %>%
      ungroup() %>%
      dplyr::select(ID_K, Index) %>%
      left_join(names_all, by = "ID_K") %>%
      arrange(desc(Index))
    
    v$data <- index %>%
      slice(1:20)
    
    title$data <- "Top 20 Urban Districts"
  })
  
  ### Only rural districts ###
  observeEvent(input$lk, {
    normalized_data <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds")) %>%
      select(-Unemp_Men, -Apprent)
    
    current_weights <- weights()
    top_synth_kreis <- sum(unlist(current_weights))
    order_vars <- names(current_weights)
    n <- length(names(normalized_data))
    
    names_all <- normalized_data %>%
      dplyr::select(Name, ID_K)
    
    index <- normalized_data %>%
      relocate(all_of(order_vars), .after = "Name") %>%
      rowwise() %>%
      mutate(
        Index = round(((sum(c_across(5:n) * unlist(current_weights))) / top_synth_kreis) * 100),
        .keep = "unused"
      ) %>%
      ungroup() %>%
      dplyr::select(ID_K, Index) %>%
      left_join(names_all, by = "ID_K") %>%
      arrange(desc(Index))
    
    v$data <- index %>%
      slice(1:20)
    
    title$data <- "Top 20 Rural Districts"
  })
  
 ### GEM data ###
  county_data_reactive <- reactive({
    
    req(selected_district())
    
    normalized_county_data <- read_rds(
      file = paste0(path_data, "/Manipulated/normalized_data_gem_2023.rds")) %>%
      select(-Unemp_Men)
    
    district_id <- selected_district()$ID_K
    
    county_data <- normalized_county_data %>%
      dplyr::filter(ID_K == district_id)
    
    weights <- c(
      "Population"                    = (input$Population) / 10,
      "New_Housing_per_Capita"        = (input$New_Housing_per_Capita) / 10,
      "Permit_Housing_perCapita"      = (input$Permit_Housing_perCapita) / 10,
      "Age_below_6"                   = (input$Age_below_6) / 10,
      "Age_6_18"                      = (input$Age_6_18) / 10,
      "Age_65"                        = (input$Age_65) / 10,
      "School_Primary"                = (input$School_Primary) / 10,
      "School_SpecialEdu"             = (input$School_SpecialEdu) / 10,
      "Migration_Balance"             = (input$Migration_Balance) / 10,
      "Purchasing_Power"              = (input$Purchasing_Power) / 10,
      "Recreation_Area_per_Capita"    = (input$Recreation_Area_per_Capita) / 10,
      "Forest_Area"                   = (input$Forest_Area) / 10,
      "Water_Area"                    = (input$Water_Area) / 10,
      "Population_Density"            = (input$Population_Density) / 10,
      "Highway_Access"                = (input$Highway_Access) / 10,
      "Airport_Access"                = (input$Airport_Access) / 10,
      "Highspeed_Rail_Access"         = (input$Highspeed_Rail_Access) / 10,
      "Supermarket_Access"            = (input$Supermarket_Access) / 10,
      "Doc_GP"                        = (input$Doc_GP) / 10,
      "Pharmacy_Access"               = (input$Pharmacy_Access) / 10,
      "Broadband_50Mbps"              = (input$Broadband_50Mbps) / 10,
      "Broadband_100Mbps"             = (input$Broadband_100Mbps) / 10,
      "Broadband_1000Mbps"            = (input$Broadband_1000Mbps) / 10,
      "Public_Transport_Access"       = (input$Public_Transport_Access) / 10,
      "Traffic_Accidents"             = (input$Traffic_Accidents) / 10,
      "Child_Poverty"                 = (input$Child_Poverty) / 10,
      "Daycare"                       = (input$Daycare) / 10,
      "Emp_Rate"                      = (input$Emp_Rate) / 10,
      "Emp_Rate_Women"                = (input$Emp_Rate_Women) / 10,
     # "Unemp_Men"                     = (input$Unemp_Men) / 10,
      "Rent_NetAvg"                   = (input$Rent_NetAvg) / 10,
      "Age_18_65"                     = (input$Age_18_65) / 10,
      "Settlement_Area_in_Flood_Zone" = (input$Settlement_Area_in_Flood_Zone) / 10,
      "Sealed_Area_per_Capita"        = (input$Sealed_Area_per_Capita) / 10
    )
    
    top_synth_county <- sum(unlist(weights))
    order_vars <- names(weights)
    n <- length(names(county_data))
    
    county_index <- county_data %>%
      relocate(all_of(order_vars), .after = "Name") %>%
      rowwise() %>%
      mutate(
        Index = round(((sum(c_across(4:n) * unlist(weights))) / top_synth_county) * 100),
        .keep = "unused"
      ) %>%
      ungroup() %>%
      dplyr::select(Name, ID, ID_K, Index) %>%
      arrange(desc(Index))
    
    county_index
})
  
  output$barPlot <- renderPlotly({
    
    req(v$data)
    
    p <- ggplot(v$data, aes(
      x = reorder(Name, Index),
      y = Index,
      key = ID_K,
      text = paste0(
        "<b>District:</b> ", Name,
        "<br><b>Index:</b> ", Index,
        "<br><b>District ID:</b> ", ID_K
      )
    )) +
      geom_col(fill = "blue") +
      geom_hline(yintercept = 100) +
      coord_flip() +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = c(0, 25, 50, 75, 100),
        labels = c("0", "25", "50", "75", "100\n Your ideal district"),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        title = title$data,
        x = "",
        y = "Your Quality of Life Index"
      ) +
      theme_minimal(base_size = 16)
    
    p_plotly <- ggplotly(p, tooltip = "text", source = "district_click")
    p_plotly <- plotly::event_register(p_plotly, "plotly_click")
    
    p_plotly
  })
  
  # clicked <- reactive({
  #   event_data("plotly_click", source = "district_click", priority = "event")
  # })
  
  observeEvent(
    event_data("plotly_click", source = "district_click", priority = "event"),
    {
      req(v$data)
      
      click <- event_data("plotly_click", source = "district_click", priority = "event")
      req(click)
      req(click$key)
      
      selected_row <- v$data %>%
        dplyr::filter(ID_K == click$key) %>%
        slice(1)
      
      req(nrow(selected_row) == 1)
      
      selected_district(selected_row)
      
      output$county_title <- renderText({
        paste0("Top municipalities in: ", selected_district()$Name)
      })
      
      output$countyPlot <- renderPlot({
        req(county_data_reactive())
        
        ggplot(county_data_reactive(), aes(x = reorder(Name, Index), y = Index)) +
          geom_col(fill = "darkgreen") +
          coord_flip() +
          scale_y_continuous(
            limits = c(0, 100),
            breaks = c(0, 25, 50, 75, 100),
            expand = expansion(mult = c(0, 0.05))
          ) +
          labs(
            title = NULL,
            x = "",
            y = "Quality of Life Index"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            axis.title = element_text(face = "bold"),
            axis.text = element_text(color = "black")
          )
      })
    },
    ignoreInit = TRUE
  )
}

## -----------------------------------------------------------------------------
## Call to shinyApp
## -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

## -----------------------------------------------------------------------------