## ---------------------------
##
## Script name: shinyapp_set_deploy_example
##
## Purpose of script: Set Account and deploy app "Beta_counties" (German version)
##
## Author: Gerrit Stahn
##
## Date Created: 2026-03-24
## Last Update: 2026-05-13
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
                          token='YOUR_TOKEN',
                          secret='YOUR_SECRET')

# Test app #
# runApp("app.R")

deployApp(appDir= "PATH/Beta", appName = "Beta_NAME", appTitle = "NAME (Beta)")

# terminateApp("Beta_NAME")
## -----------------------------------------------------------------------------