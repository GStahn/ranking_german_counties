## ---------------------------
##
## Script name: WO_index_shiny_expost_countyranking_twostages
##
## Purpose of script: Creates a Shiny application for ranking German districts
##                    based on the user's preferences. In a first step, the app
##                    ranks districts, and in a second step presents the best
##                    municipalities within the best-ranked districts.
##
## Author: Gerrit Stahn
##
## Date Created: 2026-03-23
## Last Update: 2026-03-23
##
## ---------------------------

## -----------------------------------------------------------------------------
## Start
## -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyalert)
library(shinyhelper)
library(shinyBS)
library(plotly)

rm(list = ls())

setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties")
path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"

## -----------------------------------------------------------------------------
## Reusable UI blocks
## -----------------------------------------------------------------------------

district_buttons_ui <- function(center = FALSE) {
  div(
    class = if (center) "stage1-buttons" else NULL,
    style = if (!center) "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:10px;" else NULL,
    actionButton("all", "All Districts"),
    actionButton("sk", "Urban Districts Only"),
    actionButton("lk", "Rural Districts Only")
  )
}

controls_ui <- tagList(
  
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
  
  sliderInput("green", "Green Areas (Category)", -10, 10, 0),
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
  
  sliderInput("mob_trans", "Mobility Transition (Category)", -10, 10, 0),
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
  
  sliderInput("areal", "Spatial Aspects (Category)", -10, 10, 0),
  checkboxInput("show_areal", "Show Spatial Details", FALSE),
  conditionalPanel(
    condition = "input.show_areal == true",
    wellPanel(
      sliderInput("Settlement_Area_in_Flood_Zone", "Settlement Area in Flood Zone", -10, 10, 0),
      sliderInput("Sealed_Area_per_Capita", "Sealed Area per Capita", -10, 10, 0)
    )
  ),
  hr(),
  
  sliderInput("pop", "Population (Category)", -10, 10, 0),
  checkboxInput("show_pop", "Show Population Details", FALSE),
  conditionalPanel(
    condition = "input.show_pop == true",
    wellPanel(
      sliderInput("Population", "Total Population", -10, 10, 0),
      sliderInput("Population_Density", "Population Density", -10, 10, 0)
    )
  ),
  hr(),
  
  sliderInput("Age_below_6", "Age < 6", -10, 10, 0),
  sliderInput("Age_6_18", "Age 6–18", -10, 10, 0),
  sliderInput("Age_18_65", "Age 18–65", -10, 10, 0),
  sliderInput("Age_65", "Age > 65", -10, 10, 0),
  hr(),
  
  sliderInput("New_Housing_per_Capita", "New Housing per Capita", -10, 10, 0),
  sliderInput("Permit_Housing_perCapita", "Housing Permits", -10, 10, 0),
  sliderInput("Land_Price", "Land Price", -10, 10, 0),
  sliderInput("Rent_NetAvg", "Average Net Rent", -10, 10, 0),
  hr(),
  
  sliderInput("infra", "Transport Infrastructure (Category)", 0, 10, 0),
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
  
  sliderInput("digital", "Digital Infrastructure (Category)", 0, 10, 0),
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
  
  sliderInput("retail", "Retail & Services (Category)", -10, 10, 0),
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
  
  sliderInput("edu", "Education (Category)", -10, 10, 0),
  checkboxInput("show_edu", "Show Education Details", FALSE),
  conditionalPanel(
    condition = "input.show_edu == true",
    wellPanel(
      sliderInput("School_Primary", "Primary Schools", -10, 10, 0),
      sliderInput("School_SpecialEdu", "Special Education Schools", -10, 10, 0),
      sliderInput("Daycare", "Daycare Coverage", -10, 10, 0),
      sliderInput("Apprent_Positions", "Apprenticeship Positions", -10, 10, 0)
    )
  ),
  hr(),
  
  sliderInput("social", "Social Structure (Category)", -10, 10, 0),
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
  
  sliderInput("Emp_Rate", "Overall Employment Rate", 0, 10, 0),
  hr(),
  
  sliderInput("sector", "Economic Sectors (Category)", -10, 10, 0),
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
  
  sliderInput("Emp_AO_Academic", "Academic Qualification", -10, 10, 0),
  sliderInput("Emp_AO_Vocational", "Vocational Training", -10, 10, 0),
  sliderInput("Emp_AO_NoTrain", "No Formal Training", -10, 10, 0),
  sliderInput("Emp_Expert", "Experts", -10, 10, 0),
  sliderInput("Emp_Specialist", "Specialists", -10, 10, 0),
  sliderInput("Emp_Professional", "Skilled Professionals", -10, 10, 0),
  sliderInput("Emp_Helper", "Helpers", -10, 10, 0),
  hr(),
  
  sliderInput("economy", "Economic Performance (Category)", -10, 10, 0),
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
  
  sliderInput("Traffic_Accidents", "Traffic Accidents", -10, 0, 0),
  sliderInput("Emp_Creative", "Creative Industries", -10, 10, 0),
  hr()
)

tooltips_ui <- tagList(
  bsTooltip("Age_18_65",
            "Share of the population aged 18 to 65.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_6_18",
            "Children and teenagers: share of the population aged 6 to 18.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_65",
            "Older population: Share of residents aged 65 and above.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_below_6",
            "Young children: Share of the population under age 6.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Airport_Access",
            "Average travel time by car to the nearest international airport in Germany, measured in minutes. Negative weights favor districts with a smaller average travel time.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("all",
            "This button calculates the RegioIndex across all German rural and urban districts.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Apprent_Positions",
            "Total number of company-based apprenticeship positions per 100 apprenticeship seekers. This variable indicates how easy it is for young people to find a training place.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("areal",
            "Captures usually less favourable spatial conditions, such as land sealing or settlement areas located in flood-prone zones. Negative weights for this category increasingly favor districts with relatively smaller amounts of such area.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("air",
            "Evaluates air quality in a region. A negative weight for this category and its variables favours districts with cleaner air and lower pollution levels.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_1000Mbps",
            "Share of households with internet access of at least 1000 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_100Mbps",
            "Share of households with internet access of at least 100 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_50Mbps",
            "Share of households with internet access of at least 50 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Charg_Points_per100EV",
            "Charging points per 100 electric vehicles: Shows how well charging infrastructure is developed relative to the local electric vehicle stock.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Child_Poverty",
            "Share of children living in households receiving basic income support (Bürgergeld/Grundsicherung).",
            placement = "top", trigger = "hover"),
  
  bsTooltip("co_avg",
            "Average carbon monoxide concentration in the air. Negative weights value districts with a lower environmental burden through CO.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Daycare",
            "Daycare provision: Share of children with access to a childcare place.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("digital",
            "Evaluates the digital infrastructure of a region, especially broadband coverage. A higher weight for this category favors better digital connectivity.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Doc_GP",
            "Number of general practitioners relative to the population.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("edu",
            "Describes the availability of educational infrastructure such as schools, daycare facilities, and apprenticeship opportunities.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("economy",
            "Measures the economic performance of a region, including purchasing power, income, GDP, and investment. Higher values indicate stronger economic performance.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Academic",
            "Share of employees with a university degree among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_NoTrain",
            "Share of employees without formal qualifications among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Vocational",
            "Share of employees with completed vocational training among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Creative",
            "Share of employees working in creative sectors among all employees subject to social insurance contributions. This serves as a proxy for the cultural offer of a district.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Expert",
            "Share of employees in highly qualified occupations among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Helper",
            "Share of employees in low-skilled occupations among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Primary",
            "Share of employees working in agriculture, forestry, and fishing among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Professional",
            "Share of employees in occupations requiring intermediate qualification levels among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate",
            "Overall employment rate: Share of employed persons in the working-age population.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Foreign",
            "Share of employed persons with foreign citizenship.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Women",
            "Share of women who are employed.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Secundary",
            "Share of employees working in manufacturing and construction among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Specialist",
            "Share of employees in specialised occupations among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Tertiary",
            "Share of employees working in services among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Forest_Area",
            "Share of forest land in the total area of the region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("GDP_perCapita",
            "Economic output per resident.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("green",
            "Describes access to green, forest, and water areas. A higher weight indicates greater preference for recreational potential and more nature-based quality of life.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highspeed_Rail_Access",
            "Average travel time by car to the nearest intercity or high-speed rail station, measured in minutes.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highway_Access",
            "Average travel time by car to the nearest motorway access point, measured in minutes.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age25to54",
            "Median income, age 25–54: Typical income level of the core working-age population.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age55to64",
            "Median income, age 55–64: Typical income level closer to retirement.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("infra",
            "Measures access to transport infrastructure such as motorways, airports, rail, and public transport. Lower travel times are more strongly favored when weights are set accordingly.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Investment_Allocations",
            "This variable measures how much public investment funding a district receives per resident.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Land_Price",
            "Average price per square metre of building land.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("lk",
            "This button calculates the RegioIndex for rural districts only.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Migration_Balance",
            "Net migration: difference between in-migration and out-migration in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("mob_trans",
            "Captures aspects of the mobility transition, such as electric mobility and charging infrastructure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("New_Housing_per_Capita",
            "This variable shows how many new homes are built relative to the population and serves as a proxy for housing supply in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("no2_avg",
            "Average nitrogen dioxide concentration in the air.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pb_avg",
            "Average lead concentration in the air.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pay_Gap_Gender",
            "Median income of full-time employed women relative to that of full-time employed men. This variable measures the gender pay gap.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Permit_Housing_perCapita",
            "Housing permits per capita: Approved dwellings relative to the population, serving as an indicator of future construction activity and housing supply.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pharmacy_Access",
            "Average travel time by car to the nearest pharmacy.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm10_avg",
            "Average particulate matter concentration (PM10).",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm25_avg",
            "Average finer particulate matter concentration (PM2.5).",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pop",
            "Accounts for the size and density of the population. Higher weights tend to favour more urbanised and densely populated areas.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population",
            "Total number of residents in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population_Density",
            "Residents per square kilometre. Indicates how densely settled a region is.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Public_Transport_Access",
            "Average travel time by car to the nearest public transport stop.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Purchasing_Power",
            "Disposable income per resident available for consumption and saving after taxes and social contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Recreation_Area_per_Capita",
            "Space available for leisure and recreation per capita.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Rent_NetAvg",
            "Average net cold rent for residential housing.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("retail",
            "Captures local basic services, such as access to supermarkets, doctors, and pharmacies.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_Primary",
            "Number of primary schools in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_SpecialEdu",
            "Number of schools with a special educational focus.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sector",
            "Shows the economic structure of a region based on employment across different sectors.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Sealed_Area_per_Capita",
            "Describes the amount of sealed land per resident.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Settlement_Area_in_Flood_Zone",
            "Share of built-up land located in potentially flood-prone areas.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Electro",
            "Share of fully electric passenger cars in the total vehicle stock.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Hybrid",
            "Share of hybrid cars in the total passenger car stock.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Women_Council",
            "Share of women in local councils: This variable reflects political representation and gender equality in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sk",
            "This button calculates the RegioIndex for urban districts only.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("social",
            "Captures social structures such as equal opportunities, integration, and social participation.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("so2_avg",
            "Average sulphur dioxide concentration in the air.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Supermarket_Access",
            "Average travel time by car to the nearest supermarket.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Traffic_Accidents",
            "Traffic casualties per 100,000 residents: This variable primarily reflects road safety in a region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Water_Area",
            "Share of water bodies in the total area of a region.",
            placement = "top", trigger = "hover")
)

## -----------------------------------------------------------------------------
## UI
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
      .stage1-outer {
        display: flex;
        justify-content: center;
      }
      .stage1-wrapper {
        width: 75vw;
        max-width: 1400px;
        min-width: 320px;
        margin: 0 auto 40px auto;
        background: #fafafa;
        border: 1px solid #e6e6e6;
        border-radius: 14px;
        padding: 28px 36px 28px 36px;
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.06);
      }
      .stage1-header {
        text-align: center;
        margin-bottom: 20px;
      }
      .stage1-subtitle {
        text-align: center;
        color: #666;
        margin-bottom: 24px;
        font-size: 16px;
      }
      .stage1-buttons {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        justify-content: center;
        margin-top: 24px;
        margin-bottom: 10px;
      }
      .stage1-wrapper .form-group {
        width: 100%;
      }
      .stage1-wrapper .well {
        background-color: #ffffff;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setStage', function(value) {
        document.body.setAttribute('data-stage', value);
      });
    "))
  ),
  
  titlePanel("RegioIndex"),
  hr(),
  
  conditionalPanel(
    condition = "!output.is_stage2",
    div(
      class = "stage1-outer",
      div(
        class = "stage1-wrapper",
        div(
          class = "stage1-header",
          actionButton(
            "how",
            "How does RegioIndex work?",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
          )
        ),
        div(
          class = "stage1-subtitle",
          "Set your preferences first, then choose whether you want to compare all districts, only urban districts, or only rural districts."
        ),
        controls_ui,
        district_buttons_ui(center = TRUE) ,
        tooltips_ui
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.is_stage2",
    tagList(
      actionButton(
        "how",
        "How does RegioIndex work?",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      hr(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          district_buttons_ui(),
          hr(),
          controls_ui,
          tooltips_ui
        ),
        mainPanel(
          plotlyOutput("barPlot", height = "700px"),
          hr(),
          h4(textOutput("county_title")),
          plotOutput("countyPlot", height = "3200px")
        )
      )
    )
  )
)

## -----------------------------------------------------------------------------
## Server
## -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(TRUE, {
    shinyalert(
      "Welcome",
      "Welcome to RegioIndex - the dashboard that helps you find your favourite place in Germany!",
      showCancelButton = FALSE,
      confirmButtonText = "Let's go!",
      imageUrl = "https://raw.githubusercontent.com/GStahn/ranking_german_counties/refs/heads/main/modal_pic_small.png",
      imageWidth = 375,
      imageHeight = 250,
      animation = "slide-from-bottom",
      closeOnEsc = FALSE
    )
  }, once = TRUE)
  
  observe_helpers()
  
  stage <- reactiveVal(1)
  district_type <- reactiveVal(NULL)
  selected_district <- reactiveVal(NULL)
  
  output$is_stage2 <- reactive({
    stage() == 2
  })
  outputOptions(output, "is_stage2", suspendWhenHidden = FALSE)
  
  normalized_data_all <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_kre_2023.rds")) %>%
    select(-Unemp_Men, -Apprent)
  
  normalized_data_sk <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_SK_2023.rds")) %>%
    select(-Unemp_Men, -Apprent)
  
  normalized_data_lk <- read_rds(file = paste0(path_data, "/Manipulated/normalized_data_LK_2023.rds")) %>%
    select(-Unemp_Men, -Apprent)
  
  raw_county_data <- read_rds(file = paste0(path_data, "/Manipulated/data_gem_2023.rds")) %>%
    select(-Unemp_Men)
  
  observeEvent(input$how, {
    shinyalert(
      title = "How does RegioIndex work?",
      text = HTML("
        <div style='text-align:left; line-height:1.4;'>
          <p><b>RegioIndex</b> helps you compare rural and urban cities in Germany based on
          <b>how well they match your personal preferences</b>.</p>
          <hr/>
          <ul>
            <li>Use the sliders to set your preferences.</li>
            <li>Click All, Urban, or Rural to generate the first ranking.</li>
            <li>After that, the charts update automatically when sliders change.</li>
            <li>Click a district in the bar chart to see municipalities within it.</li>
          </ul>
        </div>
      "),
      confirmButtonText = "Got it",
      closeOnEsc = TRUE,
      html = TRUE
    )
  })
  
  weights <- reactive({
    c(
      "no2_avg"                    = input$no2_avg / 10,
      "pm25_avg"                   = input$pm25_avg / 10,
      "pm10_avg"                   = input$pm10_avg / 10,
      "co_avg"                     = input$co_avg / 10,
      "so2_avg"                    = input$so2_avg / 10,
      "pb_avg"                     = input$pb_avg / 10,
      "Recreation_Area_per_Capita" = input$Recreation_Area_per_Capita / 10,
      "Forest_Area"                = input$Forest_Area / 10,
      "Water_Area"                 = input$Water_Area / 10,
      "Charg_Points_per100EV"      = input$Charg_Points_per100EV / 10,
      "Share_Car_Hybrid"           = input$Share_Car_Hybrid / 10,
      "Share_Car_Electro"          = input$Share_Car_Electro / 10,
      "Settlement_Area_in_Flood_Zone" = input$Settlement_Area_in_Flood_Zone / 10,
      "Sealed_Area_per_Capita"        = input$Sealed_Area_per_Capita / 10,
      "Population"                 = input$Population / 10,
      "Population_Density"         = input$Population_Density / 10,
      "Age_below_6"                = input$Age_below_6 / 10,
      "Age_6_18"                   = input$Age_6_18 / 10,
      "Age_18_65"                  = input$Age_18_65 / 10,
      "Age_65"                     = input$Age_65 / 10,
      "New_Housing_per_Capita"     = input$New_Housing_per_Capita / 10,
      "Permit_Housing_perCapita"   = input$Permit_Housing_perCapita / 10,
      "Land_Price"                 = input$Land_Price / 10,
      "Rent_NetAvg"                = input$Rent_NetAvg / 10,
      "Highway_Access"             = input$Highway_Access / 10,
      "Airport_Access"             = input$Airport_Access / 10,
      "Highspeed_Rail_Access"      = input$Highspeed_Rail_Access / 10,
      "Public_Transport_Access"    = input$Public_Transport_Access / 10,
      "Broadband_50Mbps"           = input$Broadband_50Mbps / 10,
      "Broadband_100Mbps"          = input$Broadband_100Mbps / 10,
      "Broadband_1000Mbps"         = input$Broadband_1000Mbps / 10,
      "Supermarket_Access"         = input$Supermarket_Access / 10,
      "Doc_GP"                     = input$Doc_GP / 10,
      "Pharmacy_Access"            = input$Pharmacy_Access / 10,
      "School_Primary"             = input$School_Primary / 10,
      "School_SpecialEdu"          = input$School_SpecialEdu / 10,
      "Daycare"                    = input$Daycare / 10,
      "Apprent_Positions"          = input$Apprent_Positions / 10,
      "Child_Poverty"              = input$Child_Poverty / 10,
      "Share_Women_Council"        = input$Share_Women_Council / 10,
      "Pay_Gap_Gender"             = input$Pay_Gap_Gender / 10,
      "Migration_Balance"          = input$Migration_Balance / 10,
      "Emp_Rate_Women"             = input$Emp_Rate_Women / 10,
      "Emp_Rate_Foreign"           = input$Emp_Rate_Foreign / 10,
      "Emp_Rate"                   = input$Emp_Rate / 10,
      "Emp_Primary"                = input$Emp_Primary / 10,
      "Emp_Secundary"              = input$Emp_Secundary / 10,
      "Emp_Tertiary"               = input$Emp_Tertiary / 10,
      "Emp_AO_Academic"            = input$Emp_AO_Academic / 10,
      "Emp_AO_Vocational"          = input$Emp_AO_Vocational / 10,
      "Emp_AO_NoTrain"             = input$Emp_AO_NoTrain / 10,
      "Emp_Expert"                 = input$Emp_Expert / 10,
      "Emp_Specialist"             = input$Emp_Specialist / 10,
      "Emp_Professional"           = input$Emp_Professional / 10,
      "Emp_Helper"                 = input$Emp_Helper / 10,
      "GDP_perCapita"              = input$GDP_perCapita / 10,
      "Purchasing_Power"           = input$Purchasing_Power / 10,
      "Income_Median_Age25to54"    = input$Income_Median_Age25to54 / 10,
      "Income_Median_Age55to64"    = input$Income_Median_Age55to64 / 10,
      "Investment_Allocations"     = input$Investment_Allocations / 10,
      "Traffic_Accidents"          = input$Traffic_Accidents / 10,
      "Emp_Creative"               = input$Emp_Creative / 10
    )
  })
  
  bulk_update <- function(main_input, sub_ids) {
    observeEvent(main_input(), {
      for (id in sub_ids) {
        updateSliderInput(session, id, value = main_input())
      }
    }, ignoreInit = TRUE)
  }
  
  bulk_update(reactive(input$air),       c("no2_avg", "pm25_avg", "pm10_avg", "co_avg", "so2_avg", "pb_avg"))
  bulk_update(reactive(input$green),     c("Recreation_Area_per_Capita", "Forest_Area", "Water_Area"))
  bulk_update(reactive(input$mob_trans), c("Charg_Points_per100EV", "Share_Car_Hybrid", "Share_Car_Electro"))
  bulk_update(reactive(input$areal),     c("Settlement_Area_in_Flood_Zone", "Sealed_Area_per_Capita"))
  bulk_update(reactive(input$pop),       c("Population", "Population_Density"))
  bulk_update(reactive(input$infra),     c("Highway_Access", "Airport_Access", "Highspeed_Rail_Access", "Public_Transport_Access"))
  bulk_update(reactive(input$digital),   c("Broadband_50Mbps", "Broadband_100Mbps", "Broadband_1000Mbps"))
  bulk_update(reactive(input$retail),    c("Supermarket_Access", "Doc_GP", "Pharmacy_Access"))
  bulk_update(reactive(input$edu),       c("School_Primary", "School_SpecialEdu", "Daycare", "Apprent_Positions"))
  bulk_update(reactive(input$social),    c("Share_Women_Council", "Migration_Balance", "Emp_Rate_Women", "Emp_Rate_Foreign"))
  bulk_update(reactive(input$sector),    c("Emp_Primary", "Emp_Secundary", "Emp_Tertiary"))
  bulk_update(reactive(input$economy),   c("GDP_perCapita", "Purchasing_Power", "Income_Median_Age25to54", "Income_Median_Age55to64", "Investment_Allocations"))
  
  observeEvent(input$all, {
    district_type("all")
    stage(2)
    selected_district(NULL)
  })
  
  observeEvent(input$sk, {
    district_type("sk")
    stage(2)
    selected_district(NULL)
  })
  
  observeEvent(input$lk, {
    district_type("lk")
    stage(2)
    selected_district(NULL)
  })
  
  district_index_reactive <- reactive({
    req(stage() == 2)
    req(district_type())
    
    normalized_data <- switch(
      district_type(),
      "all" = normalized_data_all,
      "sk"  = normalized_data_sk,
      "lk"  = normalized_data_lk
    )
    
    title_text <- switch(
      district_type(),
      "all" = "Top 20 Districts",
      "sk"  = "Top 20 Urban Districts",
      "lk"  = "Top 20 Rural Districts"
    )
    
    current_weights <- weights()
    order_vars <- names(current_weights)
    order_vars <- order_vars[order_vars %in% names(normalized_data)]
    current_weights <- current_weights[order_vars]
    
    names_all <- normalized_data %>%
      dplyr::select(Name, ID_K)
    
    score_denom <- sum(abs(unlist(current_weights)), na.rm = TRUE)
    
    if (length(order_vars) == 0 || is.na(score_denom) || score_denom == 0) {
      index <- normalized_data %>%
        dplyr::select(ID_K) %>%
        mutate(Index = 50) %>%
        left_join(names_all, by = "ID_K") %>%
        arrange(desc(Index), Name)
    } else {
      index_raw <- normalized_data %>%
        rowwise() %>%
        mutate(
          raw_score = sum(c_across(all_of(order_vars)) * unlist(current_weights), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        dplyr::select(ID_K, raw_score)
      
      min_score <- min(index_raw$raw_score, na.rm = TRUE)
      max_score <- max(index_raw$raw_score, na.rm = TRUE)
      
      if (isTRUE(all.equal(min_score, max_score))) {
        index <- index_raw %>%
          mutate(Index = 50)
      } else {
        index <- index_raw %>%
          mutate(Index = round(100 * (raw_score - min_score) / (max_score - min_score)))
      }
      
      index <- index %>%
        dplyr::select(ID_K, Index) %>%
        left_join(names_all, by = "ID_K") %>%
        arrange(desc(Index), Name)
    }
    
    list(
      data = index %>% slice(1:20),
      title = title_text
    )
  })
  
  county_data_reactive <- reactive({
    req(stage() == 2)
    req(selected_district())
    
    district_id <- selected_district()$ID_K
    
    county_data <- raw_county_data %>%
      dplyr::filter(ID_K == district_id)
    
    weights_county <- c(
      "Population"                    = input$Population / 10,
      "New_Housing_per_Capita"        = input$New_Housing_per_Capita / 10,
      "Permit_Housing_perCapita"      = input$Permit_Housing_perCapita / 10,
      "Age_below_6"                   = input$Age_below_6 / 10,
      "Age_6_18"                      = input$Age_6_18 / 10,
      "Age_65"                        = input$Age_65 / 10,
      "School_Primary"                = input$School_Primary / 10,
      "School_SpecialEdu"             = input$School_SpecialEdu / 10,
      "Migration_Balance"             = input$Migration_Balance / 10,
      "Purchasing_Power"              = input$Purchasing_Power / 10,
      "Recreation_Area_per_Capita"    = input$Recreation_Area_per_Capita / 10,
      "Forest_Area"                   = input$Forest_Area / 10,
      "Water_Area"                    = input$Water_Area / 10,
      "Population_Density"            = input$Population_Density / 10,
      "Highway_Access"                = input$Highway_Access / 10,
      "Airport_Access"                = input$Airport_Access / 10,
      "Highspeed_Rail_Access"         = input$Highspeed_Rail_Access / 10,
      "Supermarket_Access"            = input$Supermarket_Access / 10,
      "Doc_GP"                        = input$Doc_GP / 10,
      "Pharmacy_Access"               = input$Pharmacy_Access / 10,
      "Broadband_50Mbps"              = input$Broadband_50Mbps / 10,
      "Broadband_100Mbps"             = input$Broadband_100Mbps / 10,
      "Broadband_1000Mbps"            = input$Broadband_1000Mbps / 10,
      "Public_Transport_Access"       = input$Public_Transport_Access / 10,
      "Traffic_Accidents"             = input$Traffic_Accidents / 10,
      "Child_Poverty"                 = input$Child_Poverty / 10,
      "Daycare"                       = input$Daycare / 10,
      "Emp_Rate"                      = input$Emp_Rate / 10,
      "Emp_Rate_Women"                = input$Emp_Rate_Women / 10,
      "Rent_NetAvg"                   = input$Rent_NetAvg / 10,
      "Age_18_65"                     = input$Age_18_65 / 10,
      "Settlement_Area_in_Flood_Zone" = input$Settlement_Area_in_Flood_Zone / 10,
      "Sealed_Area_per_Capita"        = input$Sealed_Area_per_Capita / 10
    )
    
    order_vars <- names(weights_county)
    order_vars <- order_vars[order_vars %in% names(county_data)]
    weights_county <- weights_county[order_vars]
    
    county_data_norm <- county_data %>%
      mutate(across(
        all_of(order_vars),
        ~ {
          rng <- max(., na.rm = TRUE) - min(., na.rm = TRUE)
          if (is.na(rng) || rng == 0) 0 else (. - min(., na.rm = TRUE)) / rng
        }
      ))
    
    if (length(order_vars) == 0 || sum(abs(weights_county), na.rm = TRUE) == 0) {
      county_index <- county_data_norm %>%
        dplyr::select(Name, ID, ID_K) %>%
        mutate(Index = 50) %>%
        arrange(desc(Index), Name)
    } else {
      county_index <- county_data_norm %>%
        rowwise() %>%
        mutate(raw_score = sum(c_across(all_of(order_vars)) * weights_county, na.rm = TRUE)) %>%
        ungroup()
      
      min_score <- min(county_index$raw_score, na.rm = TRUE)
      max_score <- max(county_index$raw_score, na.rm = TRUE)
      
      if (isTRUE(all.equal(min_score, max_score))) {
        county_index <- county_index %>%
          mutate(Index = 50)
      } else {
        county_index <- county_index %>%
          mutate(Index = round(100 * (raw_score - min_score) / (max_score - min_score)))
      }
      
      county_index <- county_index %>%
        dplyr::select(Name, ID, ID_K, Index) %>%
        arrange(desc(Index), Name)
    }
    
    county_index
  })
  
  output$barPlot <- renderPlotly({
    req(stage() == 2)
    res <- district_index_reactive()
    req(res$data)
    
    p <- ggplot(res$data, aes(
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
        labels = c("0", "25", "50", "75", "100\nYour ideal district"),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        title = res$title,
        x = "",
        y = "Your Quality of Life Index"
      ) +
      theme_minimal(base_size = 16)
    
    
    ggplotly(p, tooltip = "colour", source = "district_click") %>%
      event_register("plotly_click") |>
      style(hoverinfo = "none")
  })
  
  observeEvent(
    event_data("plotly_click", source = "district_click", priority = "event"),
    {
      res <- district_index_reactive()
      click <- event_data("plotly_click", source = "district_click", priority = "event")
      
      req(click$key)
      
      selected_row <- res$data %>%
        dplyr::filter(ID_K == click$key) %>%
        slice(1)
      
      req(nrow(selected_row) == 1)
      selected_district(selected_row)
    },
    ignoreInit = TRUE
  )
  
  output$county_title <- renderText({
    req(stage() == 2)
    req(selected_district())
    paste0("Top municipalities in: ", selected_district()$Name)
  })
  
  output$countyPlot <- renderPlot({
    req(stage() == 2)
    req(selected_district())
    req(county_data_reactive())
    
    dat <- county_data_reactive()
    
    if (nrow(dat) == 0) {
      plot.new()
      text(
        0.5, 1,
        "No municipality data available for this district.",
        cex = 1.2
      )
      return(invisible(NULL))
    }
    
    if (nrow(dat) == 1) {
      plot.new()
      text(
        0.5, 1,
        "This district is not subdivided into multiple municipalities.",
        cex = 1.2
      )
      return(invisible(NULL))
    }
    
    ggplot(dat, aes(x = reorder(Name, Index), y = Index)) +
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
}

## -----------------------------------------------------------------------------
## Call to shinyApp
## -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)