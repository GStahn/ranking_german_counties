## ---------------------------
##
## Script name: app.R
##
## Purpose of the script: Creates a Shiny application for evaluating German districts
##                    based on the user's preferences. In a first step,
##                    the app ranks districts; in a second step, it displays the
##                    best municipalities within the top-ranked districts.
##
## Author: Gerrit Stahn
##
## Created on: 2026-05-19
## Last updated: 2026-05-19
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
library(bslib)
library(plotly)
library(later)

rm(list = ls())

# setwd("/Users/apxww/Desktop/GitHub/ranking_german_counties/Beta")
# path_data <- "/Users/apxww/Desktop/GitHub/ranking_german_counties/Data"

library(shinymanager)

# ------------------ PASSWORD PROTECTION ------------------

### Restrict inactivity ###
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# data.frame with credentials info
credentials <- data.frame(
  user = c("1", "icke"),
  password = c("1", "hack"),
  stringsAsFactors = FALSE
)

# Change language
set_labels(language = "en",
  "Please authenticate" = "Login",
  "Username:" = "Username:",
  "Password:" = "Password:",
  "Login" = "Login"
)

## -----------------------------------------------------------------------------
## Reusable UI components
## -----------------------------------------------------------------------------

theme_custom <- bs_theme(
  version = 3,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#205585",
  success = "#E1BD4E",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins"),
  code_font = font_google("Fira Code"),
  
  base_font_size = "14px",
  
  "border-radius-base" = "10px",
  "border-radius-large" = "12px",
  "border-radius-small" = "8px",
  
  "btn-font-weight" = "600",
  "link-color" = "#205585",
  "link-hover-color" = "#163B5C"
)

css <- "
html, body {
  width: 100%;
  overflow-x: hidden;
}

.app-shell {
  max-width: 1150px;
  margin: 0 auto;
}

.app-title {
  margin-bottom: 24px;
}

.app-title h1 {
  color: #205585;
  font-weight: 700;
  margin-bottom: 6px;
}

.app-subtitle {
  color: #5F6B7A;
  margin-bottom: 0;
}

.title-logo {
  max-height: 72px;
  max-width: 100%;
  object-fit: contain;
  margin-bottom: 12px;
}

.sidebar-section {
  margin-bottom: 20px;
  background: #FFFFFF;
  border: 1px solid #E3E7EB;
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 3px 10px rgba(0, 0, 0, 0.05);
}

.section-header {
  background: #F5F8FB;
  border-bottom: 1px solid #E3E7EB;
  padding: 12px 16px;
  font-weight: 700;
  color: #205585;
  font-size: 16px;
}

.section-body {
  padding: 16px;
}

.section-divider {
  height: 1px;
  background: #E9ECEF;
  margin: 16px 0;
  border: none;
}

.stage1-wrapper {
  max-width: 900px;
  margin: 0 auto 40px auto;
}

.district-buttons {
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
}

.district-buttons.center {
  justify-content: center;
  margin-top: 18px;
}

.main-card {
  background: #FFFFFF;
  border: 1px solid #E3E7EB;
  border-radius: 16px;
  padding: 18px;
  margin-bottom: 18px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.06);
}

.well {
  background: #FAFBFC !important;
  border: 1px solid #E3E7EB !important;
  border-radius: 12px !important;
  box-shadow: none !important;
}

.btn-primary {
  background-color: #205585 !important;
  border-color: #205585 !important;
  color: #FFFFFF !important;
}

.btn-primary:hover,
.btn-primary:focus {
  background-color: #163B5C !important;
  border-color: #163B5C !important;
  color: #FFFFFF !important;
}

.btn-success {
  background-color: #E1BD4E !important;
  border-color: #E1BD4E !important;
  color: #000000 !important;
}

.btn-success:hover,
.btn-success:focus {
  background-color: #C9A83F !important;
  border-color: #C9A83F !important;
  color: #000000 !important;
}

.tooltip-inner {
  max-width: 400px;
  width: 400px;
  text-align: left;
  white-space: normal;
}

@media (max-width: 768px) {
  .app-shell {
    padding-left: 10px;
    padding-right: 10px;
  }

  .section-body {
    padding: 14px;
  }
  
  .export-buttons {
    display: block;
  }

  .btn {
    width: 100%;
    margin-bottom: 8px;
  }

  .district-buttons {
    display: block;
  }
  @media (max-width: 768px) {
  .app-shell {
    padding-left: 10px;
    padding-right: 10px;
  }

  .sidebar-section,
  .main-card {
    margin-bottom: 14px;
    border-radius: 12px;
  }

  .section-body {
    padding: 12px;
  }

  .btn {
    width: 100%;
    margin-bottom: 8px;
  }

  .district-buttons {
    display: block;
  }

  .plotly {
    max-width: 100%;
    overflow-x: auto;
  }
}
}
.help-intro-box,
.help-section,
.help-tip-box {
  background: #FFFFFF;
  border: 1px solid #E3E7EB;
  border-radius: 12px;
  padding: 14px 16px;
  margin-bottom: 14px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.04);
}

.help-intro-box {
  background: #F5F8FB;
  padding: 16px;
  margin-bottom: 18px;
  padding-left: 20px;
}

.help-section-title {
  margin-top: 0;
  color: #205585;
  font-weight: 700;
  font-size: 18px;
  margin-bottom: 10px;
  padding-left: 20px;
}

.help-section-body ul,
.help-section-body ol {
  margin-bottom: 0;
  padding-left: 20px;
}

.help-tip-box {
  background: #FFF8E1;
  border-left: 4px solid #E1BD4E;
  border-radius: 10px;
  box-shadow: none;
  margin-top: 16px;
}
.login-help-overlay {
  position: fixed;
  inset: 0;
  z-index: 9999;
  background: rgba(0,0,0,0.35);

  display: none;
  align-items: center;
  justify-content: center;
}

.login-help-modal {
  width: min(760px, 92vw);
  max-height: 88vh;
  background: #FFFFFF;
  border-radius: 16px;
  box-shadow: 0 12px 32px rgba(0,0,0,0.18);

  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.login-help-header {
  padding: 18px 22px;
  border-bottom: 1px solid #E3E7EB;
  text-align: left;
}

.login-help-header > div {
  font-weight: 700;
  color: #205585;
  font-size: 1.2rem;
}

.login-help-body {
  padding: 20px 22px;
  overflow-y: auto;
  text-align: left;
}

.login-help-footer1,
.login-help-footer2 {
  padding: 14px 22px;
  border-top: 1px solid #E3E7EB;
}

.login-help-footer1 {
  text-align: left;
}

.login-help-footer2 {
  text-align: center;
}

/* Consistent layout inside the help window */

.login-help-body .help-intro-box,
.login-help-body .help-section,
.login-help-body .help-tip-box {
  background: #FFFFFF;
  border: 1px solid #E3E7EB;
  border-radius: 12px;
  padding: 14px 16px;
  margin-bottom: 14px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.04);
  text-align: left;
}

.login-help-body .help-intro-box {
  background: #F5F8FB;
  padding: 16px 20px;
  margin-bottom: 18px;
}

.login-help-body .help-section-title {
  margin-top: 0;
  color: #205585;
  font-weight: 700;
  font-size: 18px;
  margin-bottom: 10px;
  padding-left: 0;
  text-align: left;
}

.login-help-body .help-section-body {
  text-align: left;
}

.login-help-body .help-section-body ul,
.login-help-body .help-section-body ol {
  margin-bottom: 0;
  padding-left: 20px;
}

.login-help-body .help-tip-box {
  background: #FFF8E1;
  border-left: 4px solid #E1BD4E;
  border-radius: 10px;
  box-shadow: none;
  margin-top: 16px;
}
.responsive-img {
  max-width: 100%;
  height: auto;
  display: block;
}
"

help_section_ui <- function(title, icon, items, ordered = FALSE) {
  div(
    class = "help-section",
    div(class = "help-section-title", paste0(icon, " ", title)),
    div(
      class = "help-section-body",
      if (ordered) {
        tags$ol(lapply(items, tags$li))
      } else {
        tags$ul(lapply(items, tags$li))
      }
    )
  )
}

help_intro_ui <- function() {
  div(
    class = "help-intro-box",
    tags$h4(style = "margin-top:0; color:#205585;", "🎯 Purpose of the app"),
    tags$p(
      style = "margin-bottom:0;",
      tags$b("RegioIndex"),
      " helps you compare rural and urban districts in Germany based on ",
      tags$b("how well they match your personal preferences."),
      "You decide which factors matter most to you, and the app calculates a ranking tailored to your preferences."
    )
  )
}

help_content_ui <- function(include_feedback = FALSE) {
  tagList(
    help_intro_ui(),
    
    help_section_ui(
      "How to use the app",
      "⚙️",
      list(
        tagList("Use the ", tags$b("slider"), ", to set how important different topics are in finding your favorite place."),
        tagList("Scale: ", tags$b("-10 bis -1"), " = The fewer, the better, ", tags$b("0"), " = Not relevant, ", tags$b("+10"), " = The more, the better"),
        tagList(tags$i("Show details"), " lets you adjust the variables within a category separately, such as child poverty, access to public transport, or broadband."),
        tagList("Choose whether you want to compare ", tags$b("all districts"), tags$b("urban districts"), "only, or ", tags$b("rural districts"), "only.")
      )
    ),
    
    help_section_ui(
      "What you get as a result",
      "📊",
      list(
        tagList("A bar chart showing the ", tags$b("top 20 regions"), " that best match your preferences."),
        tagList("Each region receives a ", tags$b("RegioIndex score"), " between ", tags$b("0"), " and ", tags$b("100"), "."),
        tagList(tags$b("100"), " represents a theoretical ideal district that would perfectly match your desired profile."),
        tagList("You can ", tags$b("click selected districts in the chart"), "."),
        tagList("After clicking, a second chart appears for all districts that are subdivided into municipalities, showing a ", tags$b("ranking of these municipalities"), ".")
      )
    ),
    
    help_section_ui(
      "How the RegioIndex is calculated",
      "🧮",
      list(
        tagList("All indicators are first ", tags$b("normalized"), ", damit sie vergleichbar sind."),
        tagList("Your slider values are used as ", tags$b("weights"), " and rescaled internally."),
        tagList("For each region, a ", tags$b("weighted sum"), " is calculated and then scaled to ", tags$b("0–100"), " skaliert.")
      ),
      ordered = TRUE
    ),
    
    help_section_ui(
      "Export",
      "📤",
      list(
        tagList("You can export all ", tags$b("generated figures "), "as PNG files ", tags$b(" "), ".")
      )
    ),
    
    div(
      class = "help-tip-box",
      tags$b("Tip: "),
      "Try different weightings to see which regions fit different lifestyles."
    ),
    
    if (include_feedback) {
      help_section_ui(
        "Feedback",
        "⚠️",
        list(
          HTML(
            paste0(
              '<a href="mailto:info@sc261.de',
              '?subject=Feedback on RegioIndex',
              '&body=Hello,%0D%0A%0D%0A',
              'I have the following feedback on RegioIndex:%0D%0A%0D%0A">',
              'Send feedback',
              '</a>'
            )
          )
        )
      )
    }
  )
}

district_buttons_ui <- function(center = FALSE) {
  div(
    class = paste("district-buttons", if (center) "center" else ""),
    actionButton("all", "All districts", class = "btn btn-primary"),
    actionButton("sk", "Urban districts only", class = "btn btn-success"),
    actionButton("lk", "Rural districts only", class = "btn btn-outline-primary")
  )
}

controls_ui <- tagList(
  
  sliderInput("air", "Air pollution (category)", -10, 0, 0, width = "100%"),
  checkboxInput("show_air", "Show air pollution details", FALSE),
  conditionalPanel(
    condition = "input.show_air == true",
    wellPanel(
      sliderInput("no2_avg", "NO2", -10, 0, 0, width = "100%"),
      sliderInput("pm25_avg", "PM2.5", -10, 0, 0, width = "100%"),
      sliderInput("pm10_avg", "PM10", -10, 0, 0, width = "100%"),
      sliderInput("co_avg", "CO", -10, 0, 0, width = "100%"),
      sliderInput("so2_avg", "SO2", -10, 0, 0, width = "100%"),
      sliderInput("pb_avg", "Lead (Pb)", -10, 0, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("green", "Green spaces (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_green", "Show green space details", FALSE),
  conditionalPanel(
    condition = "input.show_green == true",
    wellPanel(
      sliderInput("Recreation_Area_per_Capita", "Recreational area per capita", -10, 10, 0, width = "100%"),
      sliderInput("Forest_Area", "Forest area", -10, 10, 0, width = "100%"),
      sliderInput("Water_Area", "Water area", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("mob_trans", "Mobility transition (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_mob_trans", "Show mobility details", FALSE),
  conditionalPanel(
    condition = "input.show_mob_trans == true",
    wellPanel(
      sliderInput("Charg_Points_per100EV", "Charging points per 100 electric vehicles", -10, 10, 0, width = "100%"),
      sliderInput("Share_Car_Hybrid", "Share of hybrid vehicles", -10, 10, 0, width = "100%"),
      sliderInput("Share_Car_Electro", "Share of electric vehicles", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("areal", "Land-use risk (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_areal", "Show land-use risk details", FALSE),
  conditionalPanel(
    condition = "input.show_areal == true",
    wellPanel(
      sliderInput("Settlement_Area_in_Flood_Zone", "Settlement area in flood zones", -10, 10, 0, width = "100%"),
      sliderInput("Sealed_Area_per_Capita", "Sealed area per capita", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("pop", "Population (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_pop", "Show population details", FALSE),
  conditionalPanel(
    condition = "input.show_pop == true",
    wellPanel(
      sliderInput("Population", "Total population", -10, 10, 0, width = "100%"),
      sliderInput("Population_Density", "Population density", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("Age_below_6", "Age < 6", -10, 10, 0, width = "100%"),
  sliderInput("Age_6_18", "Age 6–18", -10, 10, 0, width = "100%"),
  sliderInput("Age_18_65", "Age 18–65", -10, 10, 0, width = "100%"),
  sliderInput("Age_65", "Age > 65", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("New_Housing_per_Capita", "New housing units per capita", -10, 10, 0, width = "100%"),
  sliderInput("Permit_Housing_perCapita", "Building permits", -10, 10, 0, width = "100%"),
  sliderInput("Land_Price", "Land price", -10, 10, 0, width = "100%"),
  sliderInput("Rent_NetAvg", "Average net rent excluding utilities", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("infra", "Transport infrastructure (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_infra", "Show transport infrastructure details", FALSE),
  conditionalPanel(
    condition = "input.show_infra == true",
    wellPanel(
      sliderInput("Highway_Access", "Highway access", -10, 10, 0, width = "100%"),
      sliderInput("Airport_Access", "Airport access", -10, 10, 0, width = "100%"),
      sliderInput("Highspeed_Rail_Access", "Long-distance/high-speed rail access", -10, 10, 0, width = "100%"),
      sliderInput("Public_Transport_Access", "Public transport access", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("digital", "Digital infrastructure (category)", 0, 10, 0, width = "100%"),
  checkboxInput("show_digital", "Show digital infrastructure details", FALSE),
  conditionalPanel(
    condition = "input.show_digital == true",
    wellPanel(
      sliderInput("Broadband_50Mbps", "Broadband 50 Mbps", 0, 10, 0, width = "100%"),
      sliderInput("Broadband_100Mbps", "Broadband 100 Mbps", 0, 10, 0, width = "100%"),
      sliderInput("Broadband_1000Mbps", "Broadband 1000 Mbps", 0, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("retail", "Retail & services (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_retail", "Show service details", FALSE),
  conditionalPanel(
    condition = "input.show_retail == true",
    wellPanel(
      sliderInput("Supermarket_Access", "Supermarket access", -10, 10, 0, width = "100%"),
      sliderInput("Doc_GP", "General practitioners", -10, 10, 0, width = "100%"),
      sliderInput("Pharmacy_Access", "Pharmacy access", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("edu", "Education (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_edu", "Show education details", FALSE),
  conditionalPanel(
    condition = "input.show_edu == true",
    wellPanel(
      sliderInput("School_Primary", "Primary schools", -10, 10, 0, width = "100%"),
      sliderInput("School_SpecialEdu", "Special-needs schools", -10, 10, 0, width = "100%"),
      sliderInput("Daycare", "Kitaversorgung", -10, 10, 0, width = "100%"),
      sliderInput("Apprent_Positions", "Apprenticeship positions", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("social", "Social structure (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_social", "Show social structure details", FALSE),
  conditionalPanel(
    condition = "input.show_social == true",
    wellPanel(
      sliderInput("Share_Women_Council", "Women on municipal councils", -10, 10, 0, width = "100%"),
      sliderInput("Migration_Balance", "Migration balance", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Rate_Women", "Female employment rate", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Rate_Foreign", "Employment rate of foreign nationals", -10, 10, 0, width = "100%")
    )
  ),
  sliderInput("Pay_Gap_Gender", "Earnings gap between men and women", -10, 10, 0, width = "100%"),
  sliderInput("Child_Poverty", "Child poverty", -10, 0, 0, width = "100%"),
  hr(),
  
  sliderInput("Emp_Rate", "Overall employment rate", 0, 10, 0, width = "100%"),
  hr(),
  
  
  sliderInput("Emp_Primary", "Primary sector", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Secundary", "Secondary sector", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Tertiary", "Tertiary sector", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("Emp_AO_Academic", "Academic qualification", -10, 10, 0, width = "100%"),
  sliderInput("Emp_AO_Vocational", "Vocational training", -10, 10, 0, width = "100%"),
  sliderInput("Emp_AO_NoTrain", "No vocational training", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Expert", "Experts", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Specialist", "Specialists", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Professional", "Skilled workers", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Helper", "Helpers", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("economy", "Economic performance (category)", -10, 10, 0, width = "100%"),
  checkboxInput("show_economy", "Show economic performance details", FALSE),
  conditionalPanel(
    condition = "input.show_economy == true",
    wellPanel(
      sliderInput("GDP_perCapita", "GDP per capita", -10, 10, 0, width = "100%"),
      sliderInput("Purchasing_Power", "Purchasing power", -10, 10, 0, width = "100%"),
      sliderInput("Income_Median_Age25to54", "Income (age 25–54)", -10, 10, 0, width = "100%"),
      sliderInput("Income_Median_Age55to64", "Income (age 55–64)", -10, 10, 0, width = "100%"),
      sliderInput("Investment_Allocations", "Investment funding", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("Traffic_Accidents", "Traffic accidents", -10, 0, 0, width = "100%"),
  sliderInput("Emp_Creative", "Creative economy", -10, 10, 0, width = "100%"),
  hr()
)

tooltips_ui <- tagList(
  bsTooltip("Age_18_65",
            "Age 18–65 describes the share of the population of working age. A positive weight favors regions with a large working-age population; a negative weight favors regions with a smaller working-age share.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_6_18",
            "Age 6–18 describes the share of children and adolescents of school age. A positive weight favors regions with many young people; a negative weight favors regions with a smaller share of school-age children and adolescents.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_65",
            "Age > 65 describes the share of older people in a region. A positive weight favors regions with many seniors; a negative weight favors regions with a younger population structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_below_6",
            "Age < 6 describes the share of very young children in a region. A positive weight favors regions with many small children; a negative weight favors regions with a smaller share of very young children.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Airport_Access",
            "Average driving time to the nearest international airport in Germany, measured in minutes. Negative weights favor districts with shorter average driving times.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("all",
            "This button calculates the RegioIndex for all German rural and urban districts.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Apprent_Positions",
            "Total number of company-based apprenticeship positions per 100 apprenticeship seekers. A positive weight favors regions with many apprenticeship positions; a negative weight favors regions with a less developed apprenticeship market.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("areal",
            "The land-use risks category describes flood risks and soil sealing. A positive weight favors more exposed regions; a negative weight favors regions with lower land-use risks.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("air",
            "Evaluates air quality in a region. The more negative the weight for this category and its variables, the more regions with cleaner air and lower pollution are favored in the ranking.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_1000Mbps",
            "Share of households with internet access of at least 1000 Mbps. A higher weight favors regions with better broadband coverage.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_100Mbps",
            "Share of households with internet access of at least 100 Mbps. A higher weight favors regions with better broadband coverage.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_50Mbps",
            "Share of households with internet access of at least 50 Mbps. A higher weight favors regions with better broadband coverage.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Charg_Points_per100EV",
            "Charging points per 100 electric vehicles describes the expansion of charging infrastructure relative to electric-vehicle use. A positive weight favors regions with good charging provision; a negative weight favors regions where charging infrastructure plays a smaller role.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Child_Poverty",
            "Share of children living in households receiving citizen's benefit/basic income support. Since the slider only allows negative values, regions with higher child poverty can be deliberately weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("co_avg",
            "Average carbon monoxide concentration in the air. Since the slider only allows negative values, regions with higher CO pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Daycare",
            "Daycare provision: share of children with access to a childcare place. A positive weight favors regions with good early-childhood care; a negative weight favors regions with lower institutional childcare density.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("digital",
            "The digital infrastructure category describes a region's broadband coverage at 50, 100, and 1000 Mbps. A higher weight favors regions with better digital connectivity.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Doc_GP",
            "Number of general practitioners relative to the population. A positive weight favors regions with good primary-care provision; a negative weight favors regions with less dense medical infrastructure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("edu",
            "The education category describes a region's education and care infrastructure, such as primary and special-needs schools, daycare centers, and apprenticeship positions. A positive weight favors regions with well-developed education and care services; a negative weight favors quieter or less dense regions with fewer institutional services.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("economy",
            "The economic performance category describes a region's economic strength, for example through GDP per capita, purchasing power, income, and investment funding. A positive weight favors economically stronger regions with higher purchasing power and income; a negative weight favors regions with lower economic dynamism and potentially greater funding or development potential.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Academic",
            "Academic qualification describes the share of employees with academic qualifications. A positive weight favors regions with a higher share of academics; a negative weight favors regions with a more practice- or vocationally oriented employment structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_NoTrain",
            "No vocational training describes the share of employees without formal vocational training. A positive weight favors regions with a higher share of low-qualified employment; a negative weight favors regions with a more formally qualified employment structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Vocational",
            "Vocational training describes the share of vocationally qualified employees. A positive weight favors regions with a strong dual or vocational qualification structure; a negative weight favors regions less shaped by vocationally trained workers.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Creative",
            "Share of employees in creative industries among all employees subject to social insurance contributions. Serves as a proxy for a district's cultural offering. A positive weight favors regions with a stronger creative economy; a negative weight favors regions with a less pronounced creative or cultural economic profile.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Expert",
            "Share of employees in highly qualified occupations among all employees subject to social insurance contributions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Helper",
            "Helpers describes the share of simple jobs with lower skill requirements. A positive weight favors regions with a higher share of helper occupations; a negative weight favors regions with a more qualification-intensive employment structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Primary",
            "Share of employees in agriculture, forestry, and fishing among all employees subject to social insurance contributions. A positive weight favors more agricultural or nature-oriented regions; a negative weight favors regions less shaped by agriculture.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Professional",
            "Skilled workers describes the share of qualified skilled labor. A positive weight favors regions with a strong skilled-worker base; a negative weight favors regions with a less pronounced skilled-worker structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate",
            "Overall employment rate: share of employed people in the working-age population. A higher weight favors regions with a higher employment rate.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Foreign",
            "Employment rate of foreign nationals describes the labor-force participation of foreign nationals. A positive weight favors regions with strong labor-market integration of foreign nationals; a negative weight favors regions with less pronounced international labor-market integration.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Women",
            "Female employment rate describes women's labor-force participation. A positive weight favors regions with a high female employment rate; a negative weight favors regions with more traditional employment structures.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Secundary",
            "Share of employees in manufacturing and construction among all employees subject to social insurance contributions. A positive weight favors regions with a stronger industrial or commercial profile; a negative weight favors regions with less industrial character.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Specialist",
            "Specialists describes the share of higher-level specialist occupations. A positive weight favors regions with many specialized employees; a negative weight favors regions with lower specialization.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Tertiary",
            "Share of employees in the service sector among all employees subject to social insurance contributions. A positive weight favors more service-, knowledge-, and administration-oriented regions; a negative weight favors regions with a less pronounced service profile.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Forest_Area",
            "Share of forest area in the region's total area. A positive weight favors forest-rich regions; a negative weight favors more urban or open regions with less forest character.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("GDP_perCapita",
            "GDP per capita describes a region's economic capacity. A positive weight favors economically strong regions; a negative weight favors regions with lower performance levels and greater development potential.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("green",
            "The green spaces category summarizes how strongly a region is characterized by recreational areas, forests, and water bodies. A positive weight favors regions with many green and natural areas. A negative weight favors denser or more urban areas where green, forest, and water areas are less pronounced by comparison.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highspeed_Rail_Access",
            "Average driving time to the nearest long-distance or high-speed railway station, measured in minutes. A positive weight favors regions with good long-distance rail access; a negative weight favors less frequented regions in quieter locations.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highway_Access",
            "Average driving time to the nearest highway interchange, measured in minutes.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age25to54",
            "Income age 25–54 describes the median income of the population in the main working-age group. A positive weight favors high-income regions; a negative weight favors regions with more moderate income levels.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age55to64",
            "Income age 55–64 describes the median income of older employed people. A positive weight favors regions with high income strength in this age group; a negative weight favors regions with more moderate income levels.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("infra",
            "The transport infrastructure category describes a region's accessibility by highway, airport, long-distance rail, and public transport. A positive weight favors regions with good transport connections; a negative weight favors quieter, less developed regions with generally lower traffic and noise exposure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Investment_Allocations",
            "This variable measures how much public investment funding a region receives per resident. A positive weight favors regions with stronger investment funding; a negative weight favors regions with lower funding intensity.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Land_Price",
            "Land price describes the price level for building land. A positive weight favors regions with higher land prices and stronger demand pressure; a negative weight favors regions with cheaper land prices.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("lk",
            "This button calculates the RegioIndex only for rural districts.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Migration_Balance",
            "Migration balance describes whether a region tends to experience in- or out-migration. A positive weight favors growing regions with a positive migration balance; a negative weight favors regions with lower inflows or stronger out-migration.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("mob_trans",
            "The mobility transition category describes the expansion of electrified mobility, such as charging points and hybrid and electric vehicles. A positive weight favors regions with advanced electrified mobility. A negative weight favors regions where conventional mobility with combustion-engine vehicles is more widespread.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("New_Housing_per_Capita",
            "New housing units per capita describes new construction activity in a region. A positive weight favors regions with stronger housing construction; a negative weight favors regions with lower building dynamism.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("no2_avg",
            "Average nitrogen dioxide concentration in the air. Since the slider only allows negative values, regions with higher NO2 pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pb_avg",
            "Average lead concentration in the air. Since the slider only allows negative values, regions with higher lead pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pay_Gap_Gender",
            "Median income of full-time employed women relative to that of full-time employed men. A positive weight favors regions with a larger earnings gap; a negative weight favors regions with lower gender-specific income inequality.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Permit_Housing_perCapita",
            "Building permits describe planned or approved residential construction activity. A positive weight favors regions with high future construction activity; a negative weight favors regions with quieter or more stable settlement development.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pharmacy_Access",
            "Average driving time to the nearest pharmacy. A positive weight favors regions with good pharmacy access; a negative weight favors quieter or less centrally supplied regions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm10_avg",
            "Average particulate-matter concentration (PM10). Since the slider only allows negative values, regions with higher PM10 pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm25_avg",
            "Average particulate-matter concentration (PM2.5). Since the slider only allows negative values, regions with higher PM2.5 pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pop",
            "The population category describes the size and density of a region. A positive weight favors populous and densely populated regions; a negative weight favors smaller or more sparsely populated regions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population",
            "Total population describes the size of a region. A positive weight favors populous regions; a negative weight favors smaller, more manageable regions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population_Density",
            "Residents per square kilometer. A positive weight favors urban or densely populated regions; a negative weight favors quieter, less dense regions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Public_Transport_Access",
            "Average driving time to the nearest public transport stop. A positive weight favors regions with well-developed public transport; a negative weight favors regions with more individual or less centralized mobility structures.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Purchasing_Power",
            "Purchasing power describes the population's financial consumption and demand capacity. A positive weight favors regions with high purchasing power; a negative weight favors regions with lower price and consumption levels.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Recreation_Area_per_Capita",
            "Per-capita area for leisure and recreation. A positive weight favors regions with a lot of recreational area; a negative weight favors more urban or more densely used regions.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Rent_NetAvg",
            "Average net rent excluding utilities describes a region's rent level. A positive weight favors regions with higher rents and stronger housing demand; a negative weight favors regions with lower rent levels.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("retail",
            "The retail & services category describes local provision of supermarkets, general practitioners, and pharmacies. A positive weight favors regions with good local services; a negative weight favors quieter or less centrally supplied regions with lower service density.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_Primary",
            "Primary schools describes the provision of primary-school offerings in a region. A positive weight favors regions with good primary-school provision; a negative weight favors regions with lower school-service density.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_SpecialEdu",
            "Special-needs schools describes the provision of specialized educational offerings. A positive weight favors regions with a more developed special-education infrastructure; a negative weight favors regions with less specialized educational offerings.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Sealed_Area_per_Capita",
            "Sealed area per capita describes the extent of built-up or sealed land per resident. A positive weight favors more sealed or built-up regions; a negative weight favors regions with less soil sealing and more open space.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Settlement_Area_in_Flood_Zone",
            "Settlement area in flood zones describes the share of built-up areas exposed to flood risk. A positive weight favors regions with higher risk exposure; a negative weight favors regions with lower flood hazard.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Electro",
            "Share of registered electric passenger cars in the total vehicle fleet. A positive weight favors regions with a higher share of electric vehicles; a negative weight favors regions where conventional drive systems are more widespread.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Hybrid",
            "Share of registered hybrid passenger cars in the total vehicle fleet. This describes the spread of partly electrified mobility. A positive weight favors regions with a higher hybrid share; a negative weight favors regions with a more conventional vehicle structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Women_Council",
            "Women on municipal councils describes women's political representation in a region. A positive weight favors regions with a higher share of women on municipal councils; a negative weight favors regions with a more traditional political representation structure.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sk",
            "This button calculates the RegioIndex only for urban districts.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("social",
            "The social structure category summarizes how inclusive, balanced, and participation-oriented a region's social structure is. A positive weight favors regions with stronger inclusion and social participation. A negative weight places more emphasis on regions with lower inclusion or less balanced social structures.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("so2_avg",
            "Average sulfur dioxide concentration in the air. Since the slider only allows negative values, regions with higher SO2 pollution are weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Supermarket_Access",
            "Average driving time to the nearest supermarket. A positive weight favors regions with good local provision; a negative weight favors quieter regions with lower service density.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Traffic_Accidents",
            "Road casualties per 100,000 residents: this variable primarily reflects traffic safety in a region. Since the slider only allows negative values, regions with higher accident exposure can be deliberately weighted lower.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Water_Area",
            "Share of water areas in a region's total area. A positive weight favors water-rich regions; a negative weight favors regions less shaped by water areas.",
            placement = "top", trigger = "hover")
)

## -----------------------------------------------------------------------------
## UI
## -----------------------------------------------------------------------------

app_ui <- fluidPage(
  theme = theme_custom,
  
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML(inactivity)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setStage', function(value) {
        document.body.setAttribute('data-stage', value);
      });
    ")),
    tags$script(HTML("
  function setDeviceType() {
    Shiny.setInputValue('is_mobile', window.innerWidth <= 768, {priority: 'event'});
  }

  $(document).on('shiny:connected', function() {
    setDeviceType();
  });

  $(window).on('resize', function() {
    setDeviceType();
  });
")),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('scrollToId', function(id) {
    var el = document.getElementById(id);
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  });
"))
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "app-title",
      
      fluidRow(
        column(
          width = 8,
          
          # Optional: title image
          # img(src = "title.png", class = "title-logo"),
          
          h1("RegioIndex"),
          p(
            class = "app-subtitle",
            "The app that helps you find your favorite place in Germany."
          )
        ),
        
        column(
          width = 4,
          align = "right",
          div(
            style = "margin-top:10px;",
            actionButton(
              "show_help",
              label = "Explanation",
              icon = icon("circle-info"),
              class = "btn btn-primary btn-sm"
            )
          )
        )
      )
    ),
    
    column(
      width = 4,
      
      div(
        class = "sidebar-section",
        div(class = "section-header", "Weighting"),
        div(
          class = "section-body",
          controls_ui,
          tooltips_ui
        )
      ),
      
      div(
        class = "sidebar-section",
        div(class = "section-header", "District selection"),
        div(
          class = "section-body",
          district_buttons_ui()
        )
      )
    ),
    column(
      width = 8,
      uiOutput("results_ui")
    ),
    
    # Export municipalities
    div(
      class = "section-body",
      uiOutput("download_gem_ui") 
    )
  )
)

# ------------------ SECURE APP WRAPPER ------------------

ui <- secure_app(
  app_ui,
  theme = theme_custom,
  tags_top = tags$div(
    class = "login-top-content",
    
    tags$head(
      tags$style(css)
    ),
    
    tags$img(
      src = "modal_pic_small.png",
      class = "responsive-img",
      alt = "Logo not found"
    ),
    
    div(
      style = "margin-top:12px;",
      tags$button(
        type = "button",
        class = "btn btn-success",
        onclick = "document.getElementById('loginHelpOverlay').style.display='flex';",
        "Explanation"
      )
    ),
    
    div(
      id = "loginHelpOverlay",
      class = "login-help-overlay",
      style = "display:none;",
      
      div(
        class = "login-help-modal",
        
        div(
          class = "login-help-header",
          tags$div(
            style = "font-weight:700; color:#205585; font-size:1.2rem;"
          ),
        ),
        
        div(
          class = "login-help-body",
          help_content_ui(include_feedback = FALSE)
        ),
        
        div(
          class = "login-help-footer2",
          tags$button(
            type = "button",
            class = "btn btn-secondary btn-sm",
            onclick = "document.getElementById('loginHelpOverlay').style.display='none';",
            "Close"
          )
        )
      )
    )
  )
)

## -----------------------------------------------------------------------------
## Server
## -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  data <- reactive({
    reactiveValuesToList(result_auth)
  })

  observe_helpers()
  
  district_type <- reactiveVal(NULL)
  selected_district <- reactiveVal(NULL)
  
  normalized_data_all <- read_rds(file = "normalized_data_kre_2023.rds") %>%
    select(-Unemp_Men, -Apprent)
  
  normalized_data_sk <- read_rds(file = "normalized_data_SK_2023.rds") %>%
    select(-Unemp_Men, -Apprent)
  
  normalized_data_lk <- read_rds(file = "normalized_data_LK_2023.rds") %>%
    select(-Unemp_Men, -Apprent)
  
  raw_county_data <- read_rds(file = "normalized_data_gem_2023.rds") %>%
    select(-Unemp_Men)
  
  show_help_modal <- function() {
    modalDialog(
      title = div(style = "font-weight:700; color:#205585;", "Explanation"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      
      tags$div(
        style = "max-height:70vh; overflow-y:auto;",
        div(
          style = "padding: 4px 2px;",
          help_content_ui(include_feedback = TRUE)
        )
      )
    )
  }
  
  observeEvent(input$show_help, {
    showModal(show_help_modal())
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
    selected_district(NULL)
    
    later::later(function() {
      session$sendCustomMessage("scrollToId", "top20_anchor")
    }, 0.1)
  })
  
  observeEvent(input$sk, {
    district_type("sk")
    selected_district(NULL)
    
    later::later(function() {
      session$sendCustomMessage("scrollToId", "top20_anchor")
    }, 0.1)
  })
  
  observeEvent(input$lk, {
    district_type("lk")
    selected_district(NULL)
    
    later::later(function() {
      session$sendCustomMessage("scrollToId", "top20_anchor")
    }, 0.1)
  })
  
  district_index_reactive <- reactive({
    req(district_type())
    
    normalized_data <- switch(
      district_type(),
      "all" = normalized_data_all,
      "sk"  = normalized_data_sk,
      "lk"  = normalized_data_lk
    )
    
    title_text <- switch(
      district_type(),
      "all" = "Top 20 districts",
      "sk"  = "Top 20 urban districts",
      "lk"  = "Top 20 rural districts"
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
  
  output$results_ui <- renderUI({
    req(district_type())
    
    tagList(
      div(id = "top20_anchor"),
      div(
        class = "main-card",
        plotlyOutput("barPlot", height = "700px")
      ),
      
      conditionalPanel(
        condition = "input.all > 0 || input.sk > 0 || input.lk > 0",
        div(
          class = "section-body",
          uiOutput("download_kre_ui") 
        )
      ),
      
      conditionalPanel(
        condition = "output.has_county_plot",
        div(id = "county_anchor"),
        div(
          class = "main-card",
          h4(textOutput("county_title")),
          plotOutput("countyPlot", height = "3200px")
        )
      )
    )
  })
  
  output$has_selected_district <- reactive({
    !is.null(selected_district())
  })
  outputOptions(output, "has_selected_district", suspendWhenHidden = FALSE)
    
    p <- reactive({ 
    res <- district_index_reactive()
    req(res$data)

      ggplot(res$data, aes(
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
        labels = c("0", "25", "50", "75", "100\nIhr Idealkreis"),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        title = res$title,
        x = "",
        y = "Your quality-of-life index"
      ) +
      theme_minimal(base_size = 16)
    })
     
    output$barPlot <- renderPlotly({   
    ggplotly(p(), tooltip = "colour", source = "district_click") %>%
      style(hoverinfo = "none") %>% config(displayModeBar = FALSE) %>%  htmlwidgets::onRender(
        paste0(
          "function(el, x) {",
          "  function setCursor() {",
          "    var dragLayer = el.getElementsByClassName('nsewdrag')[0];",
          "    if (dragLayer) {",
          "      dragLayer.style.cursor = 'default';",
          "      el.on('plotly_hover', function(data) {",
          "        dragLayer.style.cursor = 'pointer';",
          "      });",
          "      el.on('plotly_unhover', function(data) {",
          "        dragLayer.style.cursor = 'default';",
          "      });",
          "      return true;",
          "    }",
          "    return false;",
          "  }",
          "  if (!setCursor()) {",
          "    var observer = new MutationObserver(function(mutations) {",
          "      if (setCursor()) { observer.disconnect(); }",
          "    });",
          "    observer.observe(el, { childList: true, subtree: true });",
          "  }",
          "}"
        )
      )
  })

    # ------------------ DOWNLOAD UI KRE ------------------
    
    output$download_kre_ui <- renderUI({
      req(p())
      
      div(
        class = "export-buttons",
        downloadButton("download_kre", "Download districts")
      )
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
      
      later::later(function() {
        session$sendCustomMessage("scrollToId", "county_anchor")
      }, 0.1)
    },
    ignoreInit = TRUE
  )

  
  output$county_title <- renderText({
    req(selected_district())
    paste0("Top municipalities in: ", selected_district()$Name)
  })
  
  p_gem <- reactive({
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
        y = "Quality-of-life index"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black")
      )
  })
  
  observeEvent(
    event_data("plotly_click", source = "district_click"),
    {
      req(county_data_reactive())
      
      if (nrow(county_data_reactive()) == 0) {
        showModal(modalDialog(
          title = "Hinweis",
          "No municipality data is available for this district.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      
      if (nrow(county_data_reactive()) == 1) {
        showModal(modalDialog(
          title = "Hinweis",
          "This district is not subdivided into multiple municipalities.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  )
  
  output$countyPlot <- renderPlot({
    req(county_data_reactive())
    req(nrow(county_data_reactive()) > 1)
    
    p_gem()
  })
  
  plot_gem_ready <- reactiveVal(FALSE)
  
  observeEvent(
    event_data("plotly_click", source = "district_click"),
    {
      plot_gem_ready(TRUE)
    }
  )
  
  output$has_county_plot <- reactive({
    dat <- county_data_reactive()
    !is.null(dat) && nrow(dat) > 1
  })
  
  outputOptions(output, "has_county_plot", suspendWhenHidden = FALSE)
  
  output$download_gem_ui <- renderUI({
    dat <- county_data_reactive()
    req(dat)
    req(nrow(dat) > 1)
    req(selected_district())
    
    div(
      class = "export-buttons",
      downloadButton("download_gem", paste0("Download municipalities in: ", selected_district()$Name))
    )
  })
  
  download_name <- reactiveVal("District selection")
  
  observeEvent(input$all, {
    download_name("all_districts_")
  })
  
  observeEvent(input$sk, {
    download_name("urban_districts_")
  })
  
  observeEvent(input$lk, {
    download_name("rural_districts_")
  })
  
  output$download_kre <- downloadHandler(
    filename = function() {
      paste0("Top20_", download_name(), Sys.Date(), ".png")
    },
    content = function(file) {
      
      res <- district_index_reactive()
      req(res$data)
      
      ggplot2::ggsave(file, plot = p(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_gem <- downloadHandler(
    filename = function() {
      paste0("municipality_ranking_", selected_district()$Name, Sys.Date(), ".png")
    },
    content = function(file) {
      req(county_data_reactive())
      
      ggplot2::ggsave(file, plot = p_gem(), width = 10, height = 6, dpi = 300)
    }
  )

}

## -----------------------------------------------------------------------------
## Launch the Shiny app
## -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)