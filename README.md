# ranking_german_counties

This repository contains the code and data pipeline for **RegioIndex**, a Shiny application that allows users to rank German counties (`Landkreise` and `Stadtkreise`) according to their own preferences.

Users can assign positive, neutral, or negative weights to a broad set of regional indicators. Based on these preferences, the app calculates a customized, preference-weighted index and returns a ranking of German counties.

## Live App

A beta version of the application is available here:

👉 [https://gstahn.shinyapps.io/Beta_counties/](https://gstahn.shinyapps.io/Beta_counties/)

## Beta Access

The beta version of the application is password-protected. Demo access is available with the following credentials:

```text
Username: 1
Password: 1
```

Alternatively:

```text
Username: icke
Password: hack
```

## Project Goal

The aim of this project is to make regional comparison in Germany more interactive and user-specific. Instead of relying on a fixed ranking, users can define what makes a region attractive to them.

The application can be used to explore questions such as:

- Which German counties best match a specific lifestyle or location preference?
- How does the ranking change when users prioritize infrastructure, housing, air quality, employment, education, or economic indicators?
- Which municipalities are located within the highest-ranked counties?

## Main Features

- Interactive Shiny application for ranking German counties
- Preference-based weighting of regional indicators
- Separate comparison of:
  - all counties
  - only `Stadtkreise`
  - only `Landkreise`
- User-adjustable sliders for categories and individual indicators
- Weighted index calculation scaled from 0 to 100
- Visualization of the top-ranked regions
- Optional second-stage ranking of municipalities within selected high-ranking counties
- Export of generated plots as image files

## Index Logic

The app calculates a customized regional index based on user-defined weights.

In simplified form, the workflow is:

1. Regional indicators are normalized to make them comparable.
2. User-selected slider values are interpreted as weights.
3. A weighted score is calculated for each region.
4. The resulting score is rescaled to an index between 0 and 100.

A value of **100** represents a theoretical ideal region that perfectly matches the selected preference profile.

## Repository Structure

```text
ranking_german_counties/
├── Beta/                         # Shiny app and deployment-related files
│   ├── app.R                     # Main Shiny application
│   ├── www/                      # Static app assets
│   └── normalized_data_*.rds     # Prepared data used by the app
│
├── Data/                         # Raw and processed data sources
│   ├── Destatis/
│   ├── INKAR/
│   ├── IOER_Monitor/
│   ├── Kreis_data/
│   ├── Manipulated/
│   └── Umweltbundesamt/
│
├── Scripts/                      # Scripts for data preparation and index construction
│   ├── Data_inkar.R
│   ├── Data_ioer.R
│   ├── Data_uba.R
│   ├── Merge_final_prep.R
│   └── WO_Index.R
│
├── Graphs/                       # Output folder for generated figures
├── Work/                         # Working files
├── County_Variable_Categories_v2.txt
├── modal_pic.png
├── modal_pic_small.png
└── README.md
```

## Data Sources

The project combines and prepares regional indicators from multiple German data sources, including folders for:

- Destatis
- INKAR
- IOER Monitor
- Umweltbundesamt

The processed data are used by the Shiny app to calculate preference-weighted rankings for German counties and municipalities.

Please note that third-party data sources may be subject to their own terms of use, licenses, or attribution requirements. Users of this repository are responsible for complying with the applicable terms of the original data providers.

## Requirements

The app is written in R and uses Shiny. The main packages used by the app include:

```r
shiny
dplyr
readr
ggplot2
plotly
bslib
shinyalert
shinyhelper
shinyBS
shinymanager
later
```

## Running the App Locally

To run the application locally, clone this repository and open the Shiny app file:

```bash
git clone https://github.com/GStahn/ranking_german_counties.git
cd ranking_german_counties/Beta
```

Then start the app from R or RStudio:

```r
shiny::runApp("app.R")
```

Alternatively, open `Beta/app.R` in RStudio and click **Run App**.

## Usage

1. Open the app locally or via the hosted beta version.
2. Log in using one of the demo credential pairs listed above.
3. Adjust the sliders according to your preferences.
4. Select whether to compare all counties, only `Stadtkreise`, or only `Landkreise`.
5. Review the resulting top-ranked regions.
6. Click on a county to inspect municipality-level rankings where available.
7. Export plots if needed.

## Status

This repository currently contains a beta version of the application. The app and data preparation scripts may continue to evolve as additional indicators, refinements, or interface improvements are added.

## Author

Gerrit Stahn

## License

This project is licensed under the **PolyForm Noncommercial License 1.0.0**.

You may use, copy, modify, and share this software for **noncommercial purposes only**. Commercial use is not permitted without prior written permission from the author.

For commercial licensing inquiries, please contact the author.

See the full license text here:

[https://polyformproject.org/licenses/noncommercial/1.0.0/](https://polyformproject.org/licenses/noncommercial/1.0.0/)

### Important Note

Because this license restricts commercial use, this project is not licensed under a conventional open-source license as defined by the Open Source Initiative. It is made available for noncommercial use, research, learning, experimentation, and personal projects.
