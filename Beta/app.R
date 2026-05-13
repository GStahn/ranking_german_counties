## ---------------------------
##
## Skriptname: app.R
##
## Zweck des Skripts: Erstellt eine Shiny-Anwendung zur Bewertung deutscher Kreise
##                    basierend auf den Präferenzen des Nutzers. In einem ersten Schritt
##                    bewertet die App die Kreise, und in einem zweiten Schritt werden die
##                    besten Gemeinden innerhalb der bestbewerteten Kreise angezeigt.
##
## Autor: Gerrit Stahn
##
## Erstellt am: 2026-03-24
## Letzte Aktualisierung: 2026-05-13
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
  "Please authenticate" = "Anmeldung",
  "Username:" = "Benutzername:",
  "Password:" = "Passwort:",
  "Login" = "Login"
)

## -----------------------------------------------------------------------------
## Wiederverwendbare UI-Bausteine
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

/* Einheitliches Layout innerhalb des Hilfefensters */

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
    tags$h4(style = "margin-top:0; color:#205585;", "🎯 Zweck der App"),
    tags$p(
      style = "margin-bottom:0;",
      tags$b("RegioIndex"),
      " hilft Ihnen, Land- und Stadtkreise in Deutschland danach zu vergleichen, ",
      tags$b("wie gut sie Ihren persönlichen Präferenzen entsprechen."),
      "Sie entscheiden, welche Faktoren für Sie am wichtigsten sind, und die App berechnet ein individuell zugeschnittenes Ranking."
    )
  )
}

help_content_ui <- function(include_feedback = FALSE) {
  tagList(
    help_intro_ui(),
    
    help_section_ui(
      "So nutzen Sie die App",
      "⚙️",
      list(
        tagList("Verwenden Sie die ", tags$b("Schieberegler"), ", um einzustellen, wie wichtig Ihnen verschiedene Themen bei der Findung Ihres Lieblingsortes sind."),
        tagList("Skala: ", tags$b("-10 bis -1"), " = Je weniger davon umso besser, ", tags$b("0"), " = Nicht relevant, ", tags$b("+10"), " = Je mehr davon umso besser"),
        tagList("Mit ", tags$i("Details anzeigen"), " können Sie die Variablen einer Kategorie separat anpassen, z. B. Kinderarmut, Zugang zum ÖPNV oder Breitband."),
        tagList("Wählen Sie aus, ob Sie ", tags$b("alle Kreise"), ", nur ", tags$b("Stadtkreise"), " oder nur ", tags$b("Landkreise"), " vergleichen möchten.")
      )
    ),
    
    help_section_ui(
      "Was Sie als Ergebnis erhalten",
      "📊",
      list(
        tagList("Ein Balkendiagramm mit den ", tags$b("Top-20-Regionen"), ", die am besten zu Ihren Präferenzen passen."),
        tagList("Jede Region erhält einen ", tags$b("RegioIndex-Wert"), " zwischen ", tags$b("0"), " und ", tags$b("100"), "."),
        tagList(tags$b("100"), " steht für einen theoretischen idealen Kreis, der Ihrem Wunschprofil perfekt entsprechen würde."),
        tagList("Sie können ", tags$b("auf ausgewählte Kreise im Diagramm klicken"), "."),
        tagList("Nach dem Klick erscheint bei allen Kreisen, die in Gemeinden unterteilt sind, ein zweites Diagramm mit einem ", tags$b("Ranking dieser Gemeinden"), ".")
      )
    ),
    
    help_section_ui(
      "Wie der RegioIndex berechnet wird",
      "🧮",
      list(
        tagList("Alle Indikatoren werden zunächst ", tags$b("normiert"), ", damit sie vergleichbar sind."),
        tagList("Ihre Schieberegler-Werte werden als ", tags$b("Gewichte"), " verwendet und intern reskaliert."),
        tagList("Für jede Region wird eine ", tags$b("gewichtete Summe"), " berechnet und anschließend auf ", tags$b("0–100"), " skaliert.")
      ),
      ordered = TRUE
    ),
    
    help_section_ui(
      "Export",
      "📤",
      list(
        tagList("Sie haben die Möglichkeit, alle ", tags$b("erzeugten Abbildungen "), "als png-Datei ", tags$b("zu exportieren"), ".")
      )
    ),
    
    div(
      class = "help-tip-box",
      tags$b("Tipp: "),
      "Probieren Sie verschiedene Gewichtungen aus, um zu sehen, welche Regionen zu unterschiedlichen Lebensstilen passen."
    ),
    
    if (include_feedback) {
      help_section_ui(
        "Feedback",
        "⚠️",
        list(
          HTML(
            paste0(
              '<a href="mailto:info@sc261.de',
              '?subject=Feedback zu RegioIndex',
              '&body=Hallo,%0D%0A%0D%0A',
              'Ich habe folgendes Feedback zu RegioIndex:%0D%0A%0D%0A">',
              'Feedback senden',
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
    actionButton("all", "Alle Kreise", class = "btn btn-primary"),
    actionButton("sk", "Nur Stadtkreise", class = "btn btn-success"),
    actionButton("lk", "Nur Landkreise", class = "btn btn-outline-primary")
  )
}

controls_ui <- tagList(
  
  sliderInput("air", "Luftverschmutzung (Kategorie)", -10, 0, 0, width = "100%"),
  checkboxInput("show_air", "Details zur Luftverschmutzung anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_air == true",
    wellPanel(
      sliderInput("no2_avg", "NO2", -10, 0, 0, width = "100%"),
      sliderInput("pm25_avg", "PM2.5", -10, 0, 0, width = "100%"),
      sliderInput("pm10_avg", "PM10", -10, 0, 0, width = "100%"),
      sliderInput("co_avg", "CO", -10, 0, 0, width = "100%"),
      sliderInput("so2_avg", "SO2", -10, 0, 0, width = "100%"),
      sliderInput("pb_avg", "Blei (Pb)", -10, 0, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("green", "Grünflächen (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_green", "Details zu Grünflächen anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_green == true",
    wellPanel(
      sliderInput("Recreation_Area_per_Capita", "Erholungsfläche pro Kopf", -10, 10, 0, width = "100%"),
      sliderInput("Forest_Area", "Waldfläche", -10, 10, 0, width = "100%"),
      sliderInput("Water_Area", "Wasserfläche", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("mob_trans", "Mobilitätswende (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_mob_trans", "Details zur Mobilität anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_mob_trans == true",
    wellPanel(
      sliderInput("Charg_Points_per100EV", "Ladepunkte je 100 E-Fahrzeuge", -10, 10, 0, width = "100%"),
      sliderInput("Share_Car_Hybrid", "Anteil Hybridfahrzeuge", -10, 10, 0, width = "100%"),
      sliderInput("Share_Car_Electro", "Anteil Elektrofahrzeuge", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("areal", "Flächenrisiko (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_areal", "Details zum Flächenrisiko anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_areal == true",
    wellPanel(
      sliderInput("Settlement_Area_in_Flood_Zone", "Siedlungsfläche in Überschwemmungsgebiet", -10, 10, 0, width = "100%"),
      sliderInput("Sealed_Area_per_Capita", "Versiegelte Fläche pro Kopf", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("pop", "Bevölkerung (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_pop", "Details zur Bevölkerung anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_pop == true",
    wellPanel(
      sliderInput("Population", "Gesamtbevölkerung", -10, 10, 0, width = "100%"),
      sliderInput("Population_Density", "Bevölkerungsdichte", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("Age_below_6", "Alter < 6", -10, 10, 0, width = "100%"),
  sliderInput("Age_6_18", "Alter 6–18", -10, 10, 0, width = "100%"),
  sliderInput("Age_18_65", "Alter 18–65", -10, 10, 0, width = "100%"),
  sliderInput("Age_65", "Alter > 65", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("New_Housing_per_Capita", "Neue Wohnungen pro Kopf", -10, 10, 0, width = "100%"),
  sliderInput("Permit_Housing_perCapita", "Baugenehmigungen", -10, 10, 0, width = "100%"),
  sliderInput("Land_Price", "Bodenpreis", -10, 10, 0, width = "100%"),
  sliderInput("Rent_NetAvg", "Durchschnittliche Nettokaltmiete", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("infra", "Verkehrsinfrastruktur (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_infra", "Details zur Verkehrsinfrastruktur anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_infra == true",
    wellPanel(
      sliderInput("Highway_Access", "Autobahnanschluss", -10, 10, 0, width = "100%"),
      sliderInput("Airport_Access", "Flughafenanbindung", -10, 10, 0, width = "100%"),
      sliderInput("Highspeed_Rail_Access", "Fernbahn-/Hochgeschwindigkeitszuganbindung", -10, 10, 0, width = "100%"),
      sliderInput("Public_Transport_Access", "ÖPNV-Anbindung", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("digital", "Digitale Infrastruktur (Kategorie)", 0, 10, 0, width = "100%"),
  checkboxInput("show_digital", "Details zur digitalen Infrastruktur anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_digital == true",
    wellPanel(
      sliderInput("Broadband_50Mbps", "Breitband 50 Mbps", 0, 10, 0, width = "100%"),
      sliderInput("Broadband_100Mbps", "Breitband 100 Mbps", 0, 10, 0, width = "100%"),
      sliderInput("Broadband_1000Mbps", "Breitband 1000 Mbps", 0, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("retail", "Handel & Dienstleistungen (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_retail", "Details zu Dienstleistungen anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_retail == true",
    wellPanel(
      sliderInput("Supermarket_Access", "Supermarktanbindung", -10, 10, 0, width = "100%"),
      sliderInput("Doc_GP", "Hausärzte", -10, 10, 0, width = "100%"),
      sliderInput("Pharmacy_Access", "Apothekenanbindung", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("edu", "Bildung (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_edu", "Details zur Bildung anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_edu == true",
    wellPanel(
      sliderInput("School_Primary", "Grundschulen", -10, 10, 0, width = "100%"),
      sliderInput("School_SpecialEdu", "Förderschulen", -10, 10, 0, width = "100%"),
      sliderInput("Daycare", "Kitaversorgung", -10, 10, 0, width = "100%"),
      sliderInput("Apprent_Positions", "Ausbildungsplätze", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("social", "Sozialstruktur (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_social", "Details zur Sozialstruktur anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_social == true",
    wellPanel(
      sliderInput("Share_Women_Council", "Frauen in Gemeinderäten", -10, 10, 0, width = "100%"),
      sliderInput("Migration_Balance", "Wanderungssaldo", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Rate_Women", "Frauenerwerbsquote", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Rate_Foreign", "Erwerbsquote Ausländer", -10, 10, 0, width = "100%")
    )
  ),
  sliderInput("Pay_Gap_Gender", "Verdienst Unterschied Männer und Frauen", -10, 10, 0, width = "100%"),
  sliderInput("Child_Poverty", "Kinderarmut", -10, 0, 0, width = "100%"),
  hr(),
  
  sliderInput("Emp_Rate", "Gesamtbeschäftigungsquote", 0, 10, 0, width = "100%"),
  hr(),
  
  
  sliderInput("Emp_Primary", "Primärer Sektor", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Secundary", "Sekundärer Sektor", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Tertiary", "Tertiärer Sektor", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("Emp_AO_Academic", "Akademische Qualifikation", -10, 10, 0, width = "100%"),
  sliderInput("Emp_AO_Vocational", "Berufsausbildung", -10, 10, 0, width = "100%"),
  sliderInput("Emp_AO_NoTrain", "Ohne Berufsausbildung", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Expert", "Experten", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Specialist", "Spezialisten", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Professional", "Fachkräfte", -10, 10, 0, width = "100%"),
  sliderInput("Emp_Helper", "Helfer", -10, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("economy", "Wirtschaftsleistung (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_economy", "Details zur Wirtschaftsleistung anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_economy == true",
    wellPanel(
      sliderInput("GDP_perCapita", "BIP pro Kopf", -10, 10, 0, width = "100%"),
      sliderInput("Purchasing_Power", "Kaufkraft", -10, 10, 0, width = "100%"),
      sliderInput("Income_Median_Age25to54", "Einkommen (Alter 25–54)", -10, 10, 0, width = "100%"),
      sliderInput("Income_Median_Age55to64", "Einkommen (Alter 55–64)", -10, 10, 0, width = "100%"),
      sliderInput("Investment_Allocations", "Investitionsförderung", -10, 10, 0, width = "100%")
    )
  ),
  hr(),
  
  sliderInput("Traffic_Accidents", "Verkehrsunfälle", -10, 0, 0, width = "100%"),
  sliderInput("Emp_Creative", "Kreativwirtschaft", -10, 10, 0, width = "100%"),
  hr()
)

tooltips_ui <- tagList(
  bsTooltip("Age_18_65",
            "Alter 18–65 beschreibt den Anteil der Bevölkerung im erwerbsfähigen Alter. Ein positives Gewicht bevorzugt Regionen mit hoher Erwerbsbevölkerung, ein negatives Gewicht Regionen mit geringerem Anteil im erwerbsfähigen Alter.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_6_18",
            "Alter 6–18 beschreibt den Anteil von Kindern und Jugendlichen im Schulalter. Ein positives Gewicht bevorzugt Regionen mit vielen jungen Menschen, ein negatives Gewicht Regionen mit geringerem Anteil schulpflichtiger Kinder und Jugendlicher.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_65",
            "Alter > 65 beschreibt den Anteil älterer Menschen in einer Region. Ein positives Gewicht bevorzugt Regionen mit vielen Senior:innen, ein negatives Gewicht Regionen mit jüngerer Bevölkerungsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_below_6",
            "Alter < 6 beschreibt den Anteil sehr junger Kinder in einer Region. Ein positives Gewicht bevorzugt Regionen mit vielen Kleinkindern, ein negatives Gewicht Regionen mit geringerem Anteil sehr junger Kinder.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Airport_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten internationalen Flughafen in Deutschland, gemessen in Minuten. Negative Gewichte begünstigen Kreise mit kürzerer durchschnittlicher Fahrtzeit.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("all",
            "Diese Schaltfläche berechnet den RegioIndex für alle deutschen Land- und Stadtkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Apprent_Positions",
            "Gesamtzahl der betrieblichen Ausbildungsplätze je 100 Ausbildungsplatzsuchende. Ein positives Gewicht bevorzugt Regionen mit vielen Ausbildungsplätzen, ein negatives Gewicht Regionen mit weniger stark ausgeprägtem Ausbildungsmarkt.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("areal",
            "Die Kategorie Flächenrisiken beschreibt Hochwasserrisiken und Flächenversiegelung. Ein positives Gewicht bevorzugt stärker belastete Regionen, ein negatives Gewicht Regionen mit geringeren Flächenrisiken.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("air",
            "Bewertet die Luftqualität in einer Region. Je negativer das Gewicht für diese Kategorie und ihre Variablen gesetzt wird, umso mehr werden Regionen mit saubererer Luft und geringerer Belastung im Raking berücksichtigt.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_1000Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 1000 Mbps. Ein höheres Gewicht bevorzugt Regionen mit besserer Basis-Breitbandversorgung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_100Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 100 Mbps. Ein höheres Gewicht bevorzugt Regionen mit besserer Basis-Breitbandversorgung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_50Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 50 Mbps. Ein höheres Gewicht bevorzugt Regionen mit besserer Basis-Breitbandversorgung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Charg_Points_per100EV",
            "Ladepunkte je 100 E-Fahrzeuge beschreibt den Ausbau der Ladeinfrastruktur im Verhältnis zur E-Fahrzeugnutzung. Ein positives Gewicht bevorzugt Regionen mit guter Ladeversorgung, ein negatives Gewicht Regionen, in denen Ladeinfrastruktur eine geringere Rolle spielt.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Child_Poverty",
            "Anteil der Kinder, die in Haushalten leben, die Bürgergeld/Grundsicherung erhalten. Da der Regler nur negative Werte zulässt, können Regionen mit höherer Kinderarmut gezielt geringer gewichtet werden.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("co_avg",
            "Durchschnittliche Kohlenmonoxidkonzentration in der Luft. Da der Regler nur negative Werte zulässt, werden Regionen mit höherer CO-Belastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Daycare",
            "Kitaversorgung: Anteil der Kinder mit Zugang zu einem Betreuungsplatz. Ein positives Gewicht bevorzugt Regionen mit guter frühkindlicher Betreuung, ein negatives Gewicht Regionen mit geringerer institutioneller Betreuungsdichte.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("digital",
            "Die Kategorie Digitale Infrastruktur beschreibt die Breitbandversorgung einer Region mit 50, 100 und 1000 Mbps. Ein höheres Gewicht bevorzugt Regionen mit besser ausgebauter digitaler Anbindung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Doc_GP",
            "Anzahl der Allgemeinärzte im Verhältnis zur Bevölkerung. Ein positives Gewicht bevorzugt Regionen mit guter hausärztlicher Versorgung, ein negatives Gewicht Regionen mit weniger dichter medizinischer Infrastruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("edu",
            "Die Kategorie Bildung beschreibt die Bildungs- und Betreuungsinfrastruktur einer Region, etwa durch Grund- und Förderschulen, Kitas und Ausbildungsplätze. Ein positives Gewicht bevorzugt Regionen mit gut ausgebauten Bildungs- und Betreuungsangeboten, ein negatives Gewicht eher ruhigere bzw. weniger verdichtete Regionen mit geringerer institutioneller Angebotsdichte.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("economy",
            "Die Kategorie Wirtschaftsleistung beschreibt die wirtschaftliche Stärke einer Region, etwa durch BIP pro Kopf, Kaufkraft, Einkommen und Investitionsförderung. Ein positives Gewicht bevorzugt wirtschaftlich stärkere Regionen mit höherer Kauf- und Einkommenskraft, ein negatives Gewicht Regionen mit geringerer wirtschaftlicher Dynamik und potenziell höherem Förder- bzw. Entwicklungspotenzial.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Academic",
            "Akademische Qualifikation beschreibt den Anteil akademisch qualifizierter Beschäftigter. Ein positives Gewicht bevorzugt Regionen mit höherem Akademiker:innenanteil, ein negatives Gewicht Regionen mit stärker praxis- oder ausbildungsorientierter Beschäftigungsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_NoTrain",
            "Ohne Berufsausbildung beschreibt den Anteil Beschäftigter ohne formale Berufsausbildung. Ein positives Gewicht bevorzugt Regionen mit höherem Anteil niedrigqualifizierter Beschäftigung, ein negatives Gewicht Regionen mit stärker formal qualifizierter Beschäftigungsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Vocational",
            "Berufsausbildung beschreibt den Anteil beruflich qualifizierter Beschäftigter. Ein positives Gewicht bevorzugt Regionen mit stark ausgeprägter dualer bzw. beruflicher Qualifikationsstruktur, ein negatives Gewicht Regionen mit geringerer Prägung durch beruflich Ausgebildete.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Creative",
            "Anteil der Beschäftigten in Kreativbranchen an allen sozialversicherungspflichtig Beschäftigten. Dient als Proxy für das kulturelle Angebot eines Kreises. Ein positives Gewicht bevorzugt Regionen mit stärker ausgeprägter Kreativwirtschaft, ein negatives Gewicht Regionen mit geringerer kreativer bzw. kultureller Wirtschaftsprägung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Expert",
            "Anteil der Beschäftigten in hochqualifizierten Berufen an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Helper",
            "Helfer beschreibt den Anteil einfacher Tätigkeiten mit geringerem Anforderungsniveau. Ein positives Gewicht bevorzugt Regionen mit höherem Anteil an Helfertätigkeiten, ein negatives Gewicht Regionen mit stärker qualifikationsintensiver Beschäftigungsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Primary",
            "Anteil der Beschäftigten in Land-, Forstwirtschaft und Fischerei an allen sozialversicherungspflichtig Beschäftigten. Ein positives Gewicht bevorzugt stärker agrarisch bzw. naturnah geprägte Regionen, ein negatives Gewicht weniger stark landwirtschaftlich geprägte Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Professional",
            "Fachkräfte beschreibt den Anteil qualifizierter Facharbeit. Ein positives Gewicht bevorzugt Regionen mit starker Fachkräftebasis, ein negatives Gewicht Regionen mit weniger stark ausgeprägter Fachkräftestruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate",
            "Gesamtbeschäftigungsquote: Anteil der Erwerbstätigen an der erwerbsfähigen Bevölkerung. Ein höheres Gewicht bevorzugt Regionen mit höherer Beschäftigungsquote.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Foreign",
            "Erwerbsquote Ausländer beschreibt die Erwerbsbeteiligung ausländischer Personen. Ein positives Gewicht bevorzugt Regionen mit hoher Arbeitsmarktintegration ausländischer Personen, ein negatives Gewicht Regionen mit geringer ausgeprägter internationaler Erwerbsintegration.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Women",
            "Frauenerwerbsquote beschreibt die Erwerbsbeteiligung von Frauen. Ein positives Gewicht bevorzugt Regionen mit hoher Frauenerwerbsquote, ein negatives Gewicht Regionen mit traditionelleren Erwerbsstrukturen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Secundary",
            "Anteil der Beschäftigten im produzierenden Gewerbe und Baugewerbe an allen sozialversicherungspflichtig Beschäftigten. Ein positives Gewicht bevorzugt stärker industriell bzw. gewerblich geprägte Regionen, ein negatives Gewicht Regionen mit geringerer industrieller Prägung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Specialist",
            "Spezialisten beschreibt den Anteil gehobener fachlicher Tätigkeiten. Ein positives Gewicht bevorzugt Regionen mit vielen spezialisierten Beschäftigten, ein negatives Gewicht Regionen mit geringerer Spezialisierung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Tertiary",
            "Anteil der Beschäftigten im Dienstleistungssektor an allen sozialversicherungspflichtig Beschäftigten. Ein positives Gewicht bevorzugt stärker service-, wissens- und verwaltungsorientierte Regionen, ein negatives Gewicht Regionen mit geringerer Dienstleistungsprägung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Forest_Area",
            "Anteil der Waldfläche an der Gesamtfläche der Region. Ein positives Gewicht bevorzugt waldreiche Regionen, ein negatives Gewicht stärker urbane oder offenere Regionen mit geringerer Waldprägung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("GDP_perCapita",
            "BIP pro Kopf beschreibt die wirtschaftliche Leistungsfähigkeit einer Region. Ein positives Gewicht bevorzugt wirtschaftsstarke Regionen, ein negatives Gewicht Regionen mit geringerem Leistungsniveau und höherem Entwicklungspotenzial.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("green",
            "Die Kategorie Grünflächen bildet zusammenfassend ab, wie stark eine Region durch Erholungsflächen, Waldflächen und Wasserflächen geprägt ist. Ein positives Gewicht begünstigt Regionen mit einer hohen Ausstattung an Grün- und Naturflächen. Ein negatives Gewicht begünstigt dagegen stärker verdichtete bzw. urbane Räume, in denen Grün-, Wald- und Wasserflächen im Vergleich weniger stark ausgeprägt sind.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highspeed_Rail_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten Fernbahn- oder Hochgeschwindigkeitsbahnhof, gemessen in Minuten. Ein positives Gewicht bevorzugt Regionen mit guter Fernbahnanbindung, ein negatives Gewicht weniger stark frequentierte Regionen mit ruhigerer Lage.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highway_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten Autobahnanschlussstelle, gemessen in Minuten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age25to54",
            "Einkommen Alter 25–54 beschreibt das mittlere Einkommen der Bevölkerung im Haupterwerbsalter. Ein positives Gewicht bevorzugt einkommensstarke Regionen, ein negatives Gewicht Regionen mit moderaterem Einkommensniveau.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age55to64",
            "Einkommen Alter 55–64 beschreibt das mittlere Einkommen älterer Erwerbstätiger. Ein positives Gewicht bevorzugt Regionen mit hoher Einkommensstärke in dieser Altersgruppe, ein negatives Gewicht Regionen mit moderaterem Einkommensniveau.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("infra",
            "Die Kategorie Verkehrsinfrastruktur beschreibt die Erreichbarkeit einer Region über Autobahn, Flughafen, Fernbahn und ÖPNV. Ein positives Gewicht bevorzugt Regionen mit guter Verkehrsanbindung, ein negatives Gewicht eher ruhigere, weniger stark erschlossene Regionen mit tendenziell geringerer Verkehrs- und Lärmbelastung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Investment_Allocations",
            "Diese Variable misst, wie viel öffentliche Investitionsförderung eine Region pro Einwohner erhält. Ein positives Gewicht bevorzugt Regionen mit stärkerer Investitionsförderung, ein negatives Gewicht Regionen mit geringerem Fördermitteleinsatz.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Land_Price",
            "Bodenpreis beschreibt das Preisniveau für Bauland. Ein positives Gewicht bevorzugt Regionen mit höheren Bodenpreisen und stärkerem Nachfragedruck, ein negatives Gewicht Regionen mit günstigeren Grundstückspreisen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("lk",
            "Diese Schaltfläche berechnet den RegioIndex ausschließlich für Landkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Migration_Balance",
            "Wanderungssaldo beschreibt, ob eine Region eher Zu- oder Abwanderung verzeichnet. Ein positives Gewicht bevorzugt wachsende Regionen mit positivem Wanderungssaldo, ein negatives Gewicht Regionen mit geringerem Zuzug bzw. stärkerer Abwanderung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("mob_trans",
            "Die Kategorie Mobilitätswende beschreibt den Ausbau elektrifizierter Mobilität, etwa durch Ladepunkte sowie Hybrid- und Elektrofahrzeuge. Ein positives Gewicht bevorzugt Regionen mit fortgeschrittener elektrifizierter Mobilität. Ein negatives Gewicht begünstigt Regionen, in denen klassische Mobilitätsformen mit Verbrennerfahrzeugen stärker verbreitet sind.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("New_Housing_per_Capita",
            "Neue Wohnungen pro Kopf beschreibt die Neubautätigkeit in einer Region. Ein positives Gewicht bevorzugt Regionen mit stärkerem Wohnungsneubau, ein negatives Gewicht Regionen mit geringerer baulicher Dynamik.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("no2_avg",
            "Durchschnittliche Stickstoffdioxidkonzentration in der Luft. Da der Regler nur negative Werte zulässt, werden Regionen mit höherer NO2-Belastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pb_avg",
            "Durchschnittliche Bleikonzentration in der Luft. Da der Regler nur negative Werte zulässt, werden Regionen mit höherer Bleibelastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pay_Gap_Gender",
            "Medianeinkommen vollzeitbeschäftigter Frauen im Verhältnis zu dem vollzeitbeschäftigter Männer. Ein positives Gewicht bevorzugt Regionen mit größerem Verdienstunterschied, ein negatives Gewicht Regionen mit geringerer geschlechtsspezifischer Einkommensungleichheit.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Permit_Housing_perCapita",
            "Baugenehmigungen beschreiben geplante bzw. genehmigte Wohnbauaktivität. Ein positives Gewicht bevorzugt Regionen mit hoher zukünftiger Bautätigkeit, ein negatives Gewicht Regionen mit ruhigerer oder stabilerer Siedlungsentwicklung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pharmacy_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten Apotheke. in positives Gewicht bevorzugt Regionen mit guter Apothekenanbindung, ein negatives Gewicht ruhigere bzw. weniger zentral versorgte Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm10_avg",
            "Durchschnittliche Feinstaubkonzentration (PM10). Da der Regler nur negative Werte zulässt, werden Regionen mit höherer PM10-Belastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm25_avg",
            "Durchschnittliche Feinstaubkonzentration (PM2.5). Da der Regler nur negative Werte zulässt, werden Regionen mit höherer PM2.5-Belastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pop",
            "Die Kategorie Bevölkerung beschreibt Größe und Dichte einer Region. Ein positives Gewicht bevorzugt bevölkerungsreiche und dicht besiedelte Regionen, ein negatives Gewicht eher kleinere bzw. dünner besiedelte Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population",
            "Gesamtbevölkerung beschreibt die Größe einer Region. Ein positives Gewicht bevorzugt bevölkerungsreiche Regionen, ein negatives Gewicht kleinere, überschaubarere Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population_Density",
            "Einwohner pro Quadratkilometer. Ein positives Gewicht bevorzugt urbane bzw. dicht besiedelte Regionen, ein negatives Gewicht ruhigere, weniger verdichtete Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Public_Transport_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten ÖPNV-Haltestelle. Ein positives Gewicht bevorzugt Regionen mit gut ausgebautem ÖPNV, ein negatives Gewicht Regionen mit stärker individueller bzw. weniger zentralisierter Mobilitätsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Purchasing_Power",
            "Kaufkraft beschreibt die finanzielle Konsum- und Nachfragekraft der Bevölkerung. Ein positives Gewicht bevorzugt Regionen mit hoher Kaufkraft, ein negatives Gewicht Regionen mit günstigerem Preis- und Konsumniveau.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Recreation_Area_per_Capita",
            "Pro-Kopf-Fläche für Freizeit und Erholung. Ein positives Gewicht bevorzugt Regionen mit viel Erholungsfläche, ein negatives Gewicht stärker urbane bzw. dichter genutzte Regionen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Rent_NetAvg",
            "Durchschnittliche Nettokaltmiete beschreibt das Mietpreisniveau einer Region. Ein positives Gewicht bevorzugt Regionen mit höheren Mieten und stärkerer Wohnraumnachfrage, ein negatives Gewicht Regionen mit günstigerem Mietniveau.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("retail",
            "Die Kategorie Handel & Dienstleistungen beschreibt die wohnortnahe Versorgung einer Region mit Supermärkten, Hausärzten und Apotheken. Ein positives Gewicht bevorzugt Regionen mit guter Nahversorgung, ein negatives Gewicht eher ruhigere bzw. weniger zentral versorgte Regionen mit geringerer Angebotsdichte.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_Primary",
            "Grundschulen beschreibt die Versorgung einer Region mit Grundschulangeboten. Ein positives Gewicht bevorzugt Regionen mit guter Grundschulversorgung, ein negatives Gewicht Regionen mit geringerer schulischer Angebotsdichte.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_SpecialEdu",
            "Förderschulen beschreibt die Versorgung mit spezialisierten Bildungsangeboten. Ein positives Gewicht bevorzugt Regionen mit stärker ausgebauter sonderpädagogischer Infrastruktur, ein negatives Gewicht Regionen mit geringerer Spezialisierung im Bildungsangebot.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Sealed_Area_per_Capita",
            "Versiegelte Fläche pro Kopf beschreibt den Umfang bebauter bzw. versiegelter Flächen je Einwohner. Ein positives Gewicht bevorzugt stärker versiegelte bzw. baulich geprägte Regionen, ein negatives Gewicht Regionen mit geringerer Flächenversiegelung und mehr Offenflächen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Settlement_Area_in_Flood_Zone",
            "Siedlungsfläche in Überschwemmungsgebiet beschreibt den Anteil bebauter Flächen mit Hochwasserrisiko. Ein positives Gewicht bevorzugt Regionen mit höherer Risikoexposition, ein negatives Gewicht Regionen mit geringerer Hochwassergefährdung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Electro",
            "Anteil der gemeldeten Elektro-Pkw am gesamten Fahrzeugbestand. Ein positives Gewicht bevorzugt Regionen mit höherem Elektrofahrzeuganteil, ein negatives Gewicht Regionen, in denen klassische Antriebsformen stärker verbreitet sind.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Hybrid",
            "Anteil der gemeldeten Hybrid-Pkw am gesamten Fahrzeugbestand. Anteil Hybridfahrzeuge beschreibt die Verbreitung teil-elektrifizierter Mobilität. Ein positives Gewicht bevorzugt Regionen mit höherem Hybridanteil, ein negatives Gewicht Regionen mit stärker klassisch geprägter Fahrzeugstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Women_Council",
            "Frauen in Gemeinderäten beschreibt die politische Repräsentation von Frauen in einer Region. Ein positives Gewicht bevorzugt Regionen mit höherem Frauenanteil in Gemeinderäten, ein negatives Gewicht Regionen mit traditionellerer politischer Repräsentationsstruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sk",
            "Diese Schaltfläche berechnet den RegioIndex ausschließlich für Stadtkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("social",
            "Die Kategorie „Sozialstruktur“ fasst zusammen, wie inklusiv, ausgewogen und teilhabeorientiert die gesellschaftliche Struktur einer Region ist. Ein positives Gewicht begünstigt Regionen mit stärker ausgeprägter Inklusion und sozialer Teilhabe. Ein negatives Gewicht legt dagegen mehr Gewicht auf Regionen mit geringerer Inklusion bzw. weniger ausgewogenen sozialen Strukturen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("so2_avg",
            "Durchschnittliche Schwefeldioxidkonzentration in der Luft. Da der Regler nur negative Werte zulässt, werden Regionen mit höherer SO2-Belastung geringer gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Supermarket_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten Supermarkt. Ein positives Gewicht bevorzugt Regionen mit guter Nahversorgung, ein negatives Gewicht ruhigere Regionen mit geringerer Angebotsdichte.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Traffic_Accidents",
            "Verkehrsverunglückte je 100.000 Einwohner: Diese Variable spiegelt vor allem die Verkehrssicherheit in einer Region wider. Da der Regler nur negative Werte zulässt, können Regionen mit höherer Unfallbelastung gezielt geringer gewichtet werden.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Water_Area",
            "Anteil der Wasserflächen an der Gesamtfläche einer Region. Ein positives Gewicht bevorzugt wasserreiche Regionen, ein negatives Gewicht Regionen mit geringerer Prägung durch Wasserflächen.",
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
          
          # Optional: Titelbild
          # img(src = "title.png", class = "title-logo"),
          
          h1("RegioIndex"),
          p(
            class = "app-subtitle",
            "Die App, mit der du deinen Lieblingsort in Deutschland findest."
          )
        ),
        
        column(
          width = 4,
          align = "right",
          div(
            style = "margin-top:10px;",
            actionButton(
              "show_help",
              label = "Erklärung",
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
        div(class = "section-header", "Gewichtung"),
        div(
          class = "section-body",
          controls_ui,
          tooltips_ui
        )
      ),
      
      div(
        class = "sidebar-section",
        div(class = "section-header", "Kreisauswahl"),
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
    
    # Export Gem
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
        "Erklärung"
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
            "Schließen"
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
      title = div(style = "font-weight:700; color:#205585;", "Erklärung"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Schließen"),
      
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
      "all" = "Top-20-Kreise",
      "sk"  = "Top-20-Stadtkreise",
      "lk"  = "Top-20-Landkreise"
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
        "<b>Kreis:</b> ", Name,
        "<br><b>Index:</b> ", Index,
        "<br><b>Kreis-ID:</b> ", ID_K
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
        y = "Ihr Lebensqualitätsindex"
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
        downloadButton("download_kre", "Download Kreise")
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
    paste0("Top-Gemeinden in: ", selected_district()$Name)
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
        y = "Lebensqualitätsindex"
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
          "Für diesen Kreis sind keine Gemeindedaten verfügbar.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      
      if (nrow(county_data_reactive()) == 1) {
        showModal(modalDialog(
          title = "Hinweis",
          "Dieser Kreis ist nicht in mehrere Gemeinden unterteilt.",
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
      downloadButton("download_gem", paste0("Download Gemeinden in: ", selected_district()$Name))
    )
  })
  
  download_name <- reactiveVal("Kreisauswahl")
  
  observeEvent(input$all, {
    download_name("alle_Kreise_")
  })
  
  observeEvent(input$sk, {
    download_name("Stadtkreise_")
  })
  
  observeEvent(input$lk, {
    download_name("Landkreise_")
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
      paste0("Ranking_Gemeinden_", selected_district()$Name, Sys.Date(), ".png")
    },
    content = function(file) {
      req(county_data_reactive())
      
      ggplot2::ggsave(file, plot = p_gem(), width = 10, height = 6, dpi = 300)
    }
  )

}

## -----------------------------------------------------------------------------
## Aufruf der Shiny-App
## -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
