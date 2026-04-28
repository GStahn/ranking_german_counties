## ---------------------------
##
## Script name: shinyapp_set_deploy
##
## Purpose of script: Set Account and deploy app "Beta_counties" (German version)
##
## Author: Gerrit Stahn
##
## Date Created: 2026-03-24
## Last Update: 2026-03-24
##
## Copyright (c) Gerrit Stahn, 2026
## Email: gerrit.stahn@wiwi.uni-halle.de
##

## -----------------------------------------------------------------------------
## Start
## -----------------------------------------------------------------------------

### Install packages (uncomment as required) ###
# install.packages('rsconnect')

### Load add-on packages ### 
library(rsconnect)    # For hosting via shinyapps.io
library(shiny)

# ------------------ Set Account Info  And Deploy ---------------------
setAccountInfo(name='gstahn',
                          token='DB1DFDB75D35637DD11DEB8AB2EB7DC6',
                          secret='P5qKk2Hb5ns1ESCAurQaHPTbeCLPK8Zm9LiOah/A')

# Test app #
# runApp("app.R")

deployApp(appDir= "/Users/apxww/Desktop/GitHub/ranking_german_counties/Beta", appName = "Beta_counties", appTitle = "Counties (Beta)")

# terminateApp("Beta_counties")
## -----------------------------------------------------------------------------