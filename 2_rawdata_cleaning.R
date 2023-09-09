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
# numbers (1, 2, 3 etc). See file "mc_questions_options.csv" in the metadata folder.
# 2. If people can choose several options, there will be a column for each option,
# with _1, _2, etc added on, and there will be a 1 in the column if they picked it. 
# The document "mc_questions_options.csv" in the metadata folder shows which column 
# is which question and which option.
# 3. Question IDs (QID...) in "mc_questions_options.csv" are not in necessarily order 
# and neither are the multiple choice options from point 1. 
# 4. For the data questions where we ask several questions about different data
# types, the pre-fix 1_, 2_, 3_, etc refers directly to the choices made in the
# question before.

# Read in de-identified dataset. I prefer to use the numeric results. The "text" results
# also have numeric results for some questions, and when making long datasets, mixing text
# and numeric data complicates things.
all_surveys <- read_csv("data_deidentified/survey_data_safe_numeric_raw.csv")

##### Create sub datasets ------------------------------------
# Make sure that each also has the ID, so we can match thins up if necessary
# All datasets are transformed to long version so plotting and analysis is
# easier (hopefully). In order to make them long, the numeric results need to
# be used - so text answers are saved separately.
# These datasets are all saved with the word "basic" in the filename, meaning that
# the headers of the files are codes, not the actual questions. In order to get
# the actual questions, use the document "questions_details.csv" in the
# metadata folder.

names(all_surveys)

# Load question metadata
questions_detail <- read_csv("metadata/mc_questions_options.csv")
# To add on the actual questions and options picked (in text)

#### Save all text answers separately ------------------------------------
# (as they prevent pivoting the data to a long format)
text_only <- all_surveys %>% 
  select("ID", "source", ends_with("_TEXT")) %>% # select text questions
  pivot_longer(cols  = c(-1, -2), names_to = "qname", values_to = "text_answer") %>% # make long
  filter(!is.na(text_answer)) %>% # remove NAs
  separate(qname, c("field_no", "qname_main", "q_code", "type"), 
           sep = "_", remove = FALSE, convert = TRUE, fill = "left") %>% 
  select(-type)

write_csv(text_only, "data_deidentified/subsets/text_results_basic.csv")

#### Make dataset with "intro" questions ------------------------------------

intro <- all_surveys %>% 
  select("ID", "source", starts_with("D-")) %>% # Get section D questions
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, main, q_code, q_text, order)

write_csv(intro, "data_deidentified/subsets/intro_results_basic.csv")

#### Make "has accessed data" dataset ------------------------------------
# Do in two parts, because the way the questions codes are created is different 
# First only the question about which data
# Then the part that refers to each data type that people picked

yes_data1 <- all_surveys %>% 
  select("ID", "source", contains("YD-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  select("ID", "source", starts_with("YD")) %>% # Filter for questions that do NOT start with a number
  select(!contains("DO")) %>% # Take out the results that indicate in which order the choices were displayed
  # to the survey takers (not relevant for us - question was set up to randomize the order)
  pivot_longer(cols = 3:ncol(.), 
               names_to = "qname",
               values_to = "choice") %>% 
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, main, q_code, q_text, order)

yes_data2 <- all_surveys %>% 
  select("ID", "source", contains("YD-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  select("ID", "source", !starts_with("YD")) %>% # Filter for questions that start with a number
  pivot_longer(cols = 3:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("field_no", "qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, field_no, field_name, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, field_no, field_name, main, q_code, q_text, order)

# Put together
yes_data <- full_join(yes_data1, yes_data2)

write_csv(yes_data, "data_deidentified/subsets/data_yes_results_basic.csv")

#### Make "has NOT accessed data" dataset ------------------------------------
no_data <- all_surveys %>% 
  select("ID", "source", starts_with("ND-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, main, q_code, q_text, order)

write_csv(no_data, "data_deidentified/subsets/data_no_results_basic.csv")

#### Make "dashboard" dataset ------------------------------------
dashboard <- all_surveys %>% 
  select("ID", "source", starts_with("T-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, main, q_code, q_text, order)

write_csv(dashboard, "data_deidentified/subsets/dashboard_results_basic.csv")

#### Make "trust" dataset ------------------------------------
trust <- all_surveys %>% 
  select("ID", "source", starts_with("TR-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 3:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, qname, qname_main, main, q_code, q_text, order)

write_csv(trust, "data_deidentified/subsets/trust_results_basic.csv")

#### Make "demographics" dataset ------------------------------------
demographics <- all_surveys %>% 
  select("ID", "source", starts_with("DE-")) %>% 
  select(!ends_with("_TEXT")) %>% # Take out the text answers
  pivot_longer(cols = 4:ncol(.),
               names_to = "qname",
               values_to = "choice") %>% 
  # separate main questions code and numeric options
  separate(qname, c("qname_main", "q_code"), sep = "_", remove = FALSE, convert = TRUE) %>% 
  left_join(select(questions_detail, qname_main, q_type), relationship = "many-to-many") %>%  # add on question type so we can use it to add correct text
  filter(!duplicated(.)) %>% 
  mutate(order = if_else(q_type == "RO", # For the ordering questions, save the order as a separate column
                         true = choice, 
                         false = NA)) %>% 
  mutate(q_code = if_else(is.na(q_code), # If the response is NA in column q_code, it means it's a single answer MC question
                          true = choice, # Move the answer in choice to q_code
                          false = q_code)) %>%  # Otherwise just keep q_code
  filter(!is.na(choice)) %>% # Remove NA (not picked) questions/answers
  select(!choice) %>% # no need for this column anymore
  left_join(select(questions_detail, qname, qname_main, main, q_code, q_text)) %>% 
  # reorder columns...
  select(ID, source, `DE-1`, qname, qname_main, main, q_code, q_text, order)

write_csv(demographics, "data_deidentified/subsets/demographics_results_basic.csv")

