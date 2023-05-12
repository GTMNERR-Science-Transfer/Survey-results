# Survey data processing 
# THIS IS THE RAW **NON DE-IDENTIFIED DATA**: hence confidential

#### NOT TO BE SHARED WITH ANYONE ####
#### NOT TO BE SAVED TO SHARED DRIVES, CLOUD DRIVES ETC ####

# This script / project is set up with renv, meaning it 
# See https://rstudio.github.io/renv/articles/renv.html

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay.

install.packages("pacman") # This package is used in the next line of code to
# check for installed packages and load them

# The following line checks if the required packages are installed, and if not,
# install them and then loads them. If yes, it loads them
pacman::p_load("qualtRics","questionr", "tidyverse")

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
qualtrics_api_credentials(api_key = "mPL8Fr97QRvWHw7TFXCmBDDwVvsS6quKdnMwQcT2",
                          base_url = "ca1.qualtrics.com",
                          install = TRUE)

#API/V3/UR_0MJe8Ic1zAztWER

# Get questionnaire metadata

# Get questions

# Get questionnaires

# Create a metadata file from the first two lines of the results file
metadata <- read_csv("Using collaborative open science tools to improve engagement with the ecology of the Guana River Est_May 2, 2023_19.26.csv",
                     n_max = 2, col_names = FALSE)
metadata <- as.data.frame(t(metadata))
colnames(metadata) <- c("code", "question")
# This dataframe now has a column with the Qualtrics codes and a column with the actual questions
write_csv(metadata, "metadata.csv")
##### Update this manually with all the choices

# Read in all files
# Skip first line wit the questions to ensure R picks the correct data types; 
# the column names now are the Qualtrics codes, which don't really make sense,
# they're not well-ordered. But that's fine, we'll change that in the next script.
# Add a column that indicates source
results_email <- read_csv("Using collaborative open science tools to improve engagement with the ecology of the Guana River Est_May 2, 2023_19.26.csv",
                          skip = 2)
results_email$source <- "email"
# results_socialmedia
# results_socialmedia$source <- "social"
# results_qr
# results_qr$source <- "qr"


# Combine all files/responses - add a column that indicates source
results_raw <- results_email

# Filter for those who agreed and for those who finalized the survey
results_raw <- results_raw %>% 
  filter()

# Save email addresses of people that want to stay involved in a separate file

# De-identify data

