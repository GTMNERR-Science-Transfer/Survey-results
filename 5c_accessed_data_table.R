### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 15 August 2023
# gklarenberg@ufl.edu

# Summarize "have accessed" data in table

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

#### Load data --------------------------------------------------
yes_data <- read_csv("data_deidentified/subsets/data_yes_results_basic.csv")

#### Visualize dashboard preferences ------------------------------------------
# YD-1 - What Guana Estuary data do you currently use, or have you used in the past?
# For each dataset (coded 1-8?):
# YD-2 - How do you most frequently obtain or access Guana Estuary data on [Field-1]?
# YD-3 - What are the advantages of this primary method of accessing or obtaining Guana Estuary data on [Field-1]?
# YD-4 - What are the disadvantages of the primary method of accessing or obtaining Guana Estuary data on [Field-1]?
# YD-5 - How often do/did you access or obtain Guana Estuary data on [Field-1]?
# YD-6 - What do you typically use Guana Estuary data on [Field-1] for?
# YD-7 - How well do Guana Estuary data on [Field-1] generally satisfy your need(s)?

yes_data_table <- yes_data %>% 
  group_by(field_name, main, q_text) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = round(cnt/sum(cnt)*100)) %>% 
  select(-cnt) %>% 
  ungroup() %>% 
  group_by(field_name, main) %>% 
  pivot_wider(names_from = field_name, values_from = percentage)

write_csv(yes_data_table, "results/yes_access_summary_table.csv")
