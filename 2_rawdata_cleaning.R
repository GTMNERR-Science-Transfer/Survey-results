### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 12 May 2023
# gklarenberg@ufl.edu

# Survey data cleaning and organizing (working with deidentified data)

# This script / project is set up with renv, meaning it restores the versions of
# the packages that were last used (when everything worked, we assume)
# See https://rstudio.github.io/renv/articles/renv.html

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

# Important notes on how the data is organized:
# 1. If there is only one choice for people, the results show numbers in one
# column
# 2. If people can choose several options, there will be a column for each option
# 3. Question IDs (QID...) are not in order and neither are the multiple choice
# options from point 1. See the file questions_detail.csv for details.

# Read in de-identified dataset
all_surveys <- read_csv("data_deidentified/survey_data_safe_numeric_raw.csv")

##### Create sub datasets #####
# Make sure that each also has the ID, so we can match thins up if necessary
# All datasets are transformed to long version so plotting and analysis is
# easier (hopefully).

names(all_surveys)

# Make dataset with "intro" questions
intro <- all_surveys %>% 
  select("ID", "source", starts_with("D-"))

# Make "has accessed data" dataset
yes_data <- all_surveys %>% 
  select("ID", "source", contains("YD-"))

# Make "has NOT accessed data" dataset
no_data <- all_surveys %>% 
  select("ID", "source", starts_with("ND-"))

# Make "dashboard" dataset
dashboard <- all_surveys %>% 
  select("ID", "source", starts_with("T-"))

str(dashboard)

#%>% 
  #pivot_longer(cols = 3:ncol(.))

# Make "trust" dataset
trust <- all_surveys %>% 
  select("ID", "source", starts_with("TR-"))

# Make "demographics" dataset
demographics <- all_surveys %>% 
  select("ID", "source", starts_with("DE-"))
