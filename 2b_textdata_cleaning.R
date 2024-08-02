### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 05 September 2023
# gklarenberg@ufl.edu

# Extracting text answers and organize with each question (working with deidentified data)

# This script / project is set up with renv, meaning it restores the versions of
# the packages that were last used (when everything worked, we assume)
# See https://rstudio.github.io/renv/articles/renv.html

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

#### 1. Load packages and read in data ------------------------------------
library(tidyverse)

# Read in de-identified dataset.
txt_data <- read_csv("2_data_deidentified/subsets/text_results_basic.csv")

# Load question metadata
questions_detail <- read_csv("1_metadata/mc_questions_options.csv")
# To add on the actual questions and options picked (in text)

#### 2. Text answers for "has accessed data" data -----------------------------
# Get all yes_data questions
txt_data_yes <- txt_data %>% 
  filter(grepl(pattern = "YD*", x = qname_main))

# Add on the metadata details for the questions
txt_data_yes <- txt_data_yes %>% 
  left_join(select(questions_detail, qname_main, main, q_code, field_no, field_name, q_text), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  arrange(qname_main, field_no, q_code)
write_csv(txt_data_yes, "2_data_deidentified/subsets/data_yes_results_text.csv")

# Separate data for the different questions (need to be put in a table in the
# report)
# Website used
txt_data_yes_YD2_2 <- txt_data_yes %>% 
  filter(qname_main == "YD-2", q_code == 2)
# Other way of accessing data
txt_data_yes_YD2_4 <- txt_data_yes %>% 
  filter(qname_main == "YD-2", q_code == 4)
# Other advantages
txt_data_yes_YD3 <- txt_data_yes %>% 
  filter(qname_main == "YD-3")
# Other disadvantages
txt_data_yes_YD4 <- txt_data_yes %>% 
  filter(qname_main == "YD-4")
# Other data usage
txt_data_yes_YD6 <- txt_data_yes %>% 
  filter(qname_main == "YD-6")

