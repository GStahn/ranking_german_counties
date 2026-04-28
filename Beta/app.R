## ---------------------------
##
## Skriptname: app.R (Origin: WO_index_shiny_expost_countyranking_twostages_de)
##
## Zweck des Skripts: Erstellt eine Shiny-Anwendung zur Bewertung deutscher Kreise
##                    basierend auf den Präferenzen des Nutzers. In einem ersten Schritt
##                    bewertet die App die Kreise, und in einem zweiten Schritt werden die
##                    besten Gemeinden innerhalb der bestbewerteten Kreise angezeigt.
##
## Autor: Gerrit Stahn
##
## Erstellt am: 2026-03-24
## Letzte Aktualisierung: 2026-04-28
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
}

.help-section-title {
  margin-top: 0;
  color: #205585;
  font-weight: 700;
  font-size: 18px;
  margin-bottom: 10px;
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
      tags$b("wie gut sie Ihren persönlichen Präferenzen entsprechen"),
      ". Sie entscheiden, welche Faktoren für Sie am wichtigsten sind, und die App berechnet ein individuell zugeschnittenes Ranking."
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
        tagList("Verwenden Sie die ", tags$b("Schieberegler"), ", um einzustellen, wie wichtig Ihnen verschiedene Themen sind, z. B. Umwelt, Infrastruktur oder Wirtschaft."),
        tagList("Skala: ", tags$b("-10"), " = weniger ist besser, ", tags$b("0"), " = nicht relevant, ", tags$b("+10"), " = sehr wichtig."),
        tagList("Mit ", tags$b("Details anzeigen"), " können Sie die Variablen einer Kategorie separat anpassen, z. B. NO", tags$sub("2"), ", PM2.5, ÖPNV oder Breitband."),
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
        tagList("Sie können ", tags$b("auf jeden Kreis im Diagramm klicken"), ", um ihn genauer zu erkunden."),
        tagList("Nach dem Klick erscheint ein zweites Diagramm mit einem ", tags$b("Ranking der Gemeinden innerhalb dieses Kreises"), ".")
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
  
  sliderInput("areal", "Räumliche Aspekte (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_areal", "Details zu räumlichen Aspekten anzeigen", FALSE),
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
  sliderInput("Pay_Gap_Gender", "Gender Pay Gap", -10, 10, 0, width = "100%"),
  sliderInput("Child_Poverty", "Kinderarmut", -10, 0, 0, width = "100%"),
  hr(),
  
  sliderInput("Emp_Rate", "Gesamtbeschäftigungsquote", 0, 10, 0, width = "100%"),
  hr(),
  
  sliderInput("sector", "Wirtschaftssektoren (Kategorie)", -10, 10, 0, width = "100%"),
  checkboxInput("show_sector", "Details zu Wirtschaftssektoren anzeigen", FALSE),
  conditionalPanel(
    condition = "input.show_sector == true",
    wellPanel(
      sliderInput("Emp_Primary", "Primärer Sektor", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Secundary", "Sekundärer Sektor", -10, 10, 0, width = "100%"),
      sliderInput("Emp_Tertiary", "Tertiärer Sektor", -10, 10, 0, width = "100%")
    )
  ),
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
            "Anteil der Bevölkerung im Alter von 18 bis 65 Jahren.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_6_18",
            "Kinder und Jugendliche: Anteil der Bevölkerung im Alter von 6 bis 18 Jahren.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_65",
            "Ältere Bevölkerung: Anteil der Einwohner ab 65 Jahren.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Age_below_6",
            "Kleinkinder: Anteil der Bevölkerung unter 6 Jahren.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Airport_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten internationalen Flughafen in Deutschland, gemessen in Minuten. Negative Gewichte begünstigen Kreise mit kürzerer durchschnittlicher Fahrtzeit.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("all",
            "Diese Schaltfläche berechnet den RegioIndex für alle deutschen Land- und Stadtkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Apprent_Positions",
            "Gesamtzahl der betrieblichen Ausbildungsplätze je 100 Ausbildungsplatzsuchende. Diese Variable zeigt, wie leicht es für Jugendliche ist, einen Ausbildungsplatz zu finden.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("areal",
            "Erfasst in der Regel weniger günstige räumliche Gegebenheiten, wie Flächenversiegelung oder Siedlungsflächen in hochwassergefährdeten Gebieten. Negative Gewichte für diese Kategorie begünstigen zunehmend Kreise mit vergleichsweise geringeren solchen Flächenanteilen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("air",
            "Bewertet die Luftqualität in einer Region. Ein negatives Gewicht für diese Kategorie und ihre Variablen begünstigt Kreise mit saubererer Luft und geringerer Belastung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_1000Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 1000 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_100Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 100 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Broadband_50Mbps",
            "Anteil der Haushalte mit einem Internetzugang von mindestens 50 Mbps.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Charg_Points_per100EV",
            "Ladepunkte je 100 Elektrofahrzeuge: Zeigt, wie gut die Ladeinfrastruktur im Verhältnis zum lokalen Elektrofahrzeugbestand ausgebaut ist.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Child_Poverty",
            "Anteil der Kinder, die in Haushalten leben, die Bürgergeld/Grundsicherung erhalten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("co_avg",
            "Durchschnittliche Kohlenmonoxidkonzentration in der Luft. Negative Gewichte begünstigen Kreise mit geringerer Umweltbelastung durch CO.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Daycare",
            "Kitaversorgung: Anteil der Kinder mit Zugang zu einem Betreuungsplatz.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("digital",
            "Bewertet die digitale Infrastruktur einer Region, insbesondere die Breitbandversorgung. Ein höheres Gewicht für diese Kategorie begünstigt eine bessere digitale Anbindung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Doc_GP",
            "Anzahl der Allgemeinärzte im Verhältnis zur Bevölkerung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("edu",
            "Beschreibt die Verfügbarkeit von Bildungsinfrastruktur wie Schulen, Kindertageseinrichtungen und Ausbildungsmöglichkeiten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("economy",
            "Misst die Wirtschaftsleistung einer Region, einschließlich Kaufkraft, Einkommen, BIP und Investitionen. Höhere Werte deuten auf eine stärkere Wirtschaftsleistung hin.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Academic",
            "Anteil der Beschäftigten mit Hochschulabschluss an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_NoTrain",
            "Anteil der Beschäftigten ohne formale Qualifikation an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_AO_Vocational",
            "Anteil der Beschäftigten mit abgeschlossener Berufsausbildung an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Creative",
            "Anteil der Beschäftigten in Kreativbranchen an allen sozialversicherungspflichtig Beschäftigten. Dient als Proxy für das kulturelle Angebot eines Kreises.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Expert",
            "Anteil der Beschäftigten in hochqualifizierten Berufen an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Helper",
            "Anteil der Beschäftigten in Helferberufen an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Primary",
            "Anteil der Beschäftigten in Land-, Forstwirtschaft und Fischerei an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Professional",
            "Anteil der Beschäftigten in Berufen mit mittlerem Anforderungsniveau an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate",
            "Gesamtbeschäftigungsquote: Anteil der Erwerbstätigen an der erwerbsfähigen Bevölkerung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Foreign",
            "Anteil der Beschäftigten mit ausländischer Staatsangehörigkeit.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Rate_Women",
            "Anteil der erwerbstätigen Frauen.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Secundary",
            "Anteil der Beschäftigten im produzierenden Gewerbe und Baugewerbe an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Specialist",
            "Anteil der Beschäftigten in Spezialistenberufen an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Emp_Tertiary",
            "Anteil der Beschäftigten im Dienstleistungssektor an allen sozialversicherungspflichtig Beschäftigten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Forest_Area",
            "Anteil der Waldfläche an der Gesamtfläche der Region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("GDP_perCapita",
            "Wirtschaftsleistung pro Einwohner.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("green",
            "Beschreibt den Zugang zu Grün-, Wald- und Wasserflächen. Ein höheres Gewicht zeigt eine stärkere Präferenz für Erholungspotenzial und naturnahe Lebensqualität.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highspeed_Rail_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten Fernbahn- oder Hochgeschwindigkeitsbahnhof, gemessen in Minuten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Highway_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten Autobahnanschlussstelle, gemessen in Minuten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age25to54",
            "Medianeinkommen, Alter 25–54: Typisches Einkommensniveau der Kernerwerbsbevölkerung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Income_Median_Age55to64",
            "Medianeinkommen, Alter 55–64: Typisches Einkommensniveau kurz vor dem Rentenalter.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("infra",
            "Misst den Zugang zur Verkehrsinfrastruktur wie Autobahnen, Flughäfen, Bahn und ÖPNV. Wenn die Gewichte positive gesetzt werden, werden Regionen mit einer besseren infrastrukturellen Anbindung höher gewichtet.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Investment_Allocations",
            "Diese Variable misst, wie viel öffentliche Investitionsförderung ein Kreis pro Einwohner erhält.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Land_Price",
            "Durchschnittlicher Preis pro Quadratmeter Bauland.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("lk",
            "Diese Schaltfläche berechnet den RegioIndex ausschließlich für Landkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("Migration_Balance",
            "Wanderungssaldo: Differenz zwischen Zu- und Abwanderung in einer Region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("mob_trans",
            "Erfasst Aspekte der Mobilitätswende, wie Elektromobilität und Ladeinfrastruktur.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("New_Housing_per_Capita",
            "Diese Variable zeigt, wie viele neue Wohnungen im Verhältnis zur Bevölkerung gebaut werden, und dient als Proxy für das Wohnungsangebot in einer Region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("no2_avg",
            "Durchschnittliche Stickstoffdioxidkonzentration in der Luft.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pb_avg",
            "Durchschnittliche Bleikonzentration in der Luft.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pay_Gap_Gender",
            "Medianeinkommen vollzeitbeschäftigter Frauen im Verhältnis zu dem vollzeitbeschäftigter Männer. Diese Variable misst den geschlechtsspezifischen Lohnunterschied.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Permit_Housing_perCapita",
            "Baugenehmigungen pro Kopf: Genehmigte Wohnungen im Verhältnis zur Bevölkerung, als Indikator für künftige Bautätigkeit und Wohnungsangebot.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Pharmacy_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten Apotheke.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm10_avg",
            "Durchschnittliche Feinstaubkonzentration (PM10).",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pm25_avg",
            "Durchschnittliche Feinstaubkonzentration (PM2.5).",
            placement = "top", trigger = "hover"),
  
  bsTooltip("pop",
            "Berücksichtigt die Größe und Dichte der Bevölkerung. Höhere Gewichte begünstigen tendenziell stärker urbanisierte und dichter besiedelte Gebiete.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population",
            "Gesamtanzahl der Einwohner in einer Region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Population_Density",
            "Einwohner pro Quadratkilometer. Gibt an, wie dicht besiedelt eine Region ist.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Public_Transport_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zur nächsten ÖPNV-Haltestelle.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Purchasing_Power",
            "Verfügbares Einkommen pro Einwohner, das nach Steuern und Sozialabgaben für Konsum und Sparen zur Verfügung steht.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Recreation_Area_per_Capita",
            "Pro-Kopf-Fläche für Freizeit und Erholung.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Rent_NetAvg",
            "Durchschnittliche Nettokaltmiete für Wohnraum.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("retail",
            "Erfasst lokale Grundversorgungsangebote, wie den Zugang zu Supermärkten, Ärzten und Apotheken.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_Primary",
            "Anzahl der Grundschulen in einer Region.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("School_SpecialEdu",
            "Anzahl der Schulen mit sonderpädagogischem Schwerpunkt.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sector",
            "Zeigt die Wirtschaftsstruktur einer Region anhand der Beschäftigung in verschiedenen Sektoren.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Sealed_Area_per_Capita",
            "Beschreibt die Menge versiegelter Fläche pro Einwohner.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Settlement_Area_in_Flood_Zone",
            "Anteil der bebauten Fläche in potenziell hochwassergefährdeten Gebieten.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Electro",
            "Anteil der reinen Elektro-Pkw am gesamten Fahrzeugbestand.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Car_Hybrid",
            "Anteil der Hybrid-Pkw am gesamten Pkw-Bestand.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Share_Women_Council",
            "Anteil der Frauen in Gemeinderäten: Diese Variable spiegelt die politische Repräsentation und Geschlechtergleichstellung in einer Region wider.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("sk",
            "Diese Schaltfläche berechnet den RegioIndex ausschließlich für Stadtkreise.",
            placement = "bottom", trigger = "hover"),
  
  bsTooltip("social",
            "Erfasst soziale Strukturen wie Chancengleichheit, Integration und gesellschaftliche Teilhabe.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("so2_avg",
            "Durchschnittliche Schwefeldioxidkonzentration in der Luft.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Supermarket_Access",
            "Durchschnittliche Fahrtzeit mit dem Auto zum nächsten Supermarkt.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Traffic_Accidents",
            "Verkehrsverunglückte je 100.000 Einwohner: Diese Variable spiegelt vor allem die Verkehrssicherheit in einer Region wider.",
            placement = "top", trigger = "hover"),
  
  bsTooltip("Water_Area",
            "Anteil der Wasserflächen an der Gesamtfläche einer Region.",
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
      width = 500,
      height = 333,
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
          class = "login-help-footer",
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
        condition = "output.has_selected_district",
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
  
  output$barPlot <- renderPlotly({
    res <- district_index_reactive()
    req(res$data)
    
    p <- ggplot(res$data, aes(
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
    
    
    ggplotly(p, tooltip = "colour", source = "district_click") %>%
      event_register("plotly_click") |>
      style(hoverinfo = "none") %>% config(displayModeBar = FALSE) %>%
      htmlwidgets::onRender(
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
  
  output$countyPlot <- renderPlot({
    req(selected_district())
    req(county_data_reactive())
    
    dat <- county_data_reactive()
    
    if (nrow(dat) == 0) {
      plot.new()
      text(
        0.5, 1,
        "Für diesen Kreis sind keine Gemeindedaten verfügbar.",
        cex = 1.2
      )
      return(invisible(NULL))
    }
    
    if (nrow(dat) == 1) {
      plot.new()
      text(
        0.5, 1,
        "Dieser Kreis ist nicht in mehrere Gemeinden unterteilt.",
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
        y = "Lebensqualitätsindex"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black")
      )
  })
}

## -----------------------------------------------------------------------------
## Aufruf der Shiny-App
## -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
