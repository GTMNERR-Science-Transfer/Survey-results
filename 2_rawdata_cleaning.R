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
# 1. If there is only one choice for people to pick, the results show  as different
# numbers (1, 2, 3 etc). NOTE TO SELF: FIND WHERE THE OPTIONS ARE LISTED!
# 2. If people can choose several options, there will be a column for each option,
# and there will be a 1 in the column if they picked it. The document 
# "questions_details.csv" in the metadata folder shows which column is which
# question and which option.
# 3. Question IDs (QID...) in "question_details.csv" are not in necessarily order 
# and neither are the multiple choice options from point 1. 

# Read in de-identified dataset
all_surveys <- read_csv("data_deidentified/survey_data_safe_numeric_raw.csv")

##### Create sub datasets #####
# Make sure that each also has the ID, so we can match thins up if necessary
# All datasets are transformed to long version so plotting and analysis is
# easier (hopefully). In order to make them long, the numeric results need to
# be used - so text answers are saved separately.
# These datasets are all saved with the word "basic" in the filename, meaning that
# the headers of the files are codes, not the actual questions. In order to get
# the actual questions, use the document "questions_details.csv" in the
# metadata folder.

names(all_surveys)

#### Save all text answers separately ####
# (as they prevent pivoting the data to a long format)
text_only <- all_surveys %>% 
  select("ID", "source", ends_with("_TEXT"))
write_csv(text_only, "data_deidentified/subsets/text_results_basic.csv")

#### Make dataset with "intro" questions ####
intro <- all_surveys %>% 
  select("ID", "source", starts_with("D-")) %>% # Get section D questions
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/intro_results_basic.csv")

#### Make "has accessed data" dataset ####
yes_data <- all_surveys %>% 
  select("ID", "source", contains("YD-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/data_yes_results_basic.csv")

#### Make "has NOT accessed data" dataset ####
no_data <- all_surveys %>% 
  select("ID", "source", starts_with("ND-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/data_no_results_basic.csv")

#### Make "dashboard" dataset ####
dashboard <- all_surveys %>% 
  select("ID", "source", starts_with("T-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/dashboard_results_basic.csv")

#### Make "trust" dataset ####
trust <- all_surveys %>% 
  select("ID", "source", starts_with("TR-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/trust_results_basic.csv")


#### Make "demographics" dataset ####
demographics <- all_surveys %>% 
  select("ID", "source", starts_with("DE-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 4:ncol(.),
               names_to = "question",
               values_to = "answer")
write_csv(text_only, "data_deidentified/subsets/demographics_results_basic.csv")

