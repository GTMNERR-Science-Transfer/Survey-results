### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 12 June 2023
# gklarenberg@ufl.edu

# Set key

# This script includes my personal API key: hence confidential

#### NOT TO BE SHARED WITH ANYONE ####
#### NOT TO BE SAVED TO SHARED DRIVES, CLOUD DRIVES ETC ####

# This script / project is set up with renv, meaning it restores the versions of
# the packages that were last used (when everything worked, we assume)
# See https://rstudio.github.io/renv/articles/renv.html

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

# Load packages
library(qualtRics)

# Documentation for qualtRics: https://docs.ropensci.org/qualtRics/

#### Set up API credentials ####
# To get this information, log into Qualtrics, in the top-right, click
# on the "My Account" bubble and pick "Account settings". Go to the tab
# "Qualtrics IDs".
# In the box "API" click on "Generate token". Copy-paste the code that appears
# in the function below for api_key =

# To send a request to a specific endpoint, you need to know your datacenter. 
# The format here is: {datacenterid}.qualtrics.com
# Where {datacenterid} is a specific datacenter assigned to you based on 
# your location.
# Your datacenterid is listed in the box "User".

# You only have to run this function once, as install = TRUE saves it to your 
# environment
qualtrics_api_credentials(api_key = "YOUR KEY GOES HERE",
                          base_url = "ca1.qualtrics.com",
                          install = TRUE)

# You can restart R to activate this, OR
# reload your environment so you can use the credentials without restarting R:
readRenviron("~/.Renviron")

# You can check that the key is properly stored with:
Sys.getenv("QUALTRICS_API_KEY")

# If you need to overwrite existing credentials, make sure to add the argument
# overwrite = TRUE to the function:
# qualtrics_api_credentials(api_key = "",
#                           base_url = "",
#                           overwrite = TRUE,
#                           install = TRUE)
