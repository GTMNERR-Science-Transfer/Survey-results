### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 13 June 2023
# gklarenberg@ufl.edu

# Survey metadata extraction

# This script / project is set up with renv, meaning it restores the versions of
# the packages that were last used (when everything worked, we assume)
# See https://rstudio.github.io/renv/articles/renv.html

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

# Load packages
library("qualtRics")
library("tidyverse")

# Documentation for qualtRics: https://docs.ropensci.org/qualtRics/

#### API credentials were set up in file 00 ####

# You can only use the API to download Qualtrics results for surveys that you are
# admin on. To check which surveys you can access, you can uncomment and run 
# the next line:
# all_surveys()

# Our survey IDs are
all_survey_ids <- c("SV_agTpds5m6MDrqAe", # visitor's center
                    "SV_bqMgSIcEsmEy91Q", # kiosk
                    "SV_ezASGpdQPyOr8Xk", # social media
                    "SV_9RjNbEveqMo0MLk") # email
names(all_survey_ids) <- c("visitor", "kiosk", "socialmedia", "email")

#### Extract metadata ####
#### 1. Questions ####
# Get questions - I am getting these from the emailed survey, as this one also has
# the trust questions. These are purely just the questions, not the answer options.
questions <- survey_questions("SV_9RjNbEveqMo0MLk")
write_csv(questions, "metadata/survey_questions_all.csv")

# More detailed dataframe of questions and answer options (since our survey is
# complicated...)
questions_detail <- extract_colmap(fetch_survey(surveyID = "SV_9RjNbEveqMo0MLk"))
#write_csv(questions_detail, "metadata/questions_detail.csv")

# Clean this up so we can easily match codes and text later. Do this separate for 
# "regular" questions, and the questions on data that people accessed (as this 
# has more complicated codes), and then join later.

# "Regular" questions (ignore the warning message)
questions_detail2 <- questions_detail %>%
  filter(!str_detect(qname, "^[0-9]")) %>% # Filter for questions that do NOT start with a number
  separate(qname, c("qname_main", "option", "option1"), sep = "_", remove = FALSE) %>% 
  separate(ImportId, c("ImportId", "no1", "no2"), sep = "_", remove = FALSE)

# Sub-questions on data accessed (ignore the warning message)
questions_detail3 <- questions_detail %>%
  filter(str_detect(qname, "^[0-9]")) %>% # Filter for questions that DO start with a number
  separate(qname, c("field_no", "qname_main", "option", "option1"), sep = "_", remove = FALSE) %>% 
  separate(ImportId, c("field_no2", "ImportId", "no1", "no2"), sep = "_", remove = FALSE)
# For questions YD-5 and YD-7, the actual question is in the column "sub". Column
# "main" has the dataset type that the question is about. This causes problems
# later, so move this text from sub to main
questions_detail3 <- questions_detail3 %>%  
  mutate(main = if_else(qname_main == "YD-5" | qname_main == "YD-7", 
                         true = sub, 
                         false = main))

# Put back together
questions_detail_all <- full_join(questions_detail2, questions_detail3)
# Remove description as it is main and sub combined, and timeZone.
# Column choiceId is less comprehensive than option (and not the same), so also remove
# In the column ImportID, separate QID number from the extra numbers and only save QID##
questions_detail_all <- questions_detail_all %>% 
  select(-c("description","timeZone", "choiceId", "no1", "no2", "field_no2"))

# Accessing the questions metadata to get the multiple choice codes and text
# These are the questions where respondents have to pick ONE option, or where they
# can pick a few and then reorder them
mc_questions <- metadata("SV_9RjNbEveqMo0MLk")
mc_questions <- mc_questions$questions

# Loop over each QID, get choices, and then recode and description
mc_info_all <- data.frame()
for(i in 1:length(mc_questions)){
  if(!is.null(mc_questions[[i]]$choices)){ # check if choices exists, if not, skip
    mc_info_q <- data.frame()
    ch <- mc_questions[[i]]$choices # get only the choices info
    for(j in 1:length(ch)){ # Now loop over every choice
      q_code <- names(ch[j]) # the "names" are the codes
      option <- ch[[j]]$recode # to be on the safe side also get the recode
      q_text <- ch[[j]]$description # get question text
      mc_info <- data.frame(q_code, option, q_text)
      mc_info_q <- rbind(mc_info_q, mc_info)
    }
    mc_info_q$ImportId <- names(mc_questions)[i] 
    mc_info_q$q_type <- mc_questions[[i]]$questionType$type
    mc_info_q$q_selector <- mc_questions[[i]]$questionType$selector
    mc_info_all <- rbind(mc_info_all, mc_info_q)  
  }
}

# Combine questions_detail and mc_info_all 
# The problem is that questions_detail only lists the options that have to be ranked -
# it does not list options for the multiple choice questions...
# So, first add the questions to mc_info_all
mc_info_all <- left_join(mc_info_all, select(questions_detail_all, c("ImportId", "main")), by  = "ImportId", relationship = "many-to-many")
# Take out duplicate rows
mc_info_all <- mc_info_all %>%
  filter(!duplicated(.))
# Join again 
questions_detail_all2 <- full_join(questions_detail_all, mc_info_all, 
                                   by = c("main", "ImportId"),
                                   relationship = "many-to-many")

# REMOVE: 
# if !is.na(option.x) & !is.na(option1) & q_selector != "Likert" & option.x != option.y 
questions_detail_all2 <- questions_detail_all2[!((!is.na(questions_detail_all2$option.x) | !is.na(questions_detail_all2$option1)) &
                                  questions_detail_all2$q_selector != "Likert" &
                                  questions_detail_all2$option.x != questions_detail_all2$option.y), ]

# reorder columns
questions_detail_all2 <- questions_detail_all2 %>% 
  select(ImportId, qname, qname_main, main, field_no, q_text, q_code, option1, q_type, q_selector)
# Save
write_csv(questions_detail_all2, "metadata/mc_questions_options.csv")

#### 2. Questionaire metadata ####
# Get questionnaire metadata: this is a list with 2 dataframes with information and
# a list of lists of the questions. The latter is saved separately later.
metadata_responses <- data.frame()
for (survey in all_survey_ids){
  metadata_surveys <- metadata(survey)
  # Get general survey and response info
  combined <- cbind(metadata_surveys$metadata, 
                    metadata_surveys$responsecounts)
  metadata_responses <- rbind(metadata_responses, 
                              combined)
}
# Add date that this info was gathered
metadata_responses$date_checked <- paste(Sys.Date(), Sys.time())
write_csv(metadata_responses, "metadata/response_info.csv")

#### 3. Questionnaire metadata ####
# Get questionnaire metadata: this is a list with 2 dataframes with information and
# a list of lists of the questions. The latter is not really necessary to save
metadata_responses <- data.frame()
for (survey in all_survey_ids){
  metadata_surveys <- metadata(survey)
  combined <- cbind(metadata_surveys$metadata, 
                    metadata_surveys$responsecounts)
  metadata_responses <- rbind(metadata_responses, 
                              combined)
}
# Add date that this info was gathered
metadata_responses$date_checked <- paste(Sys.Date(), Sys.time())
write_csv(metadata_responses, "metadata/response_info.csv")
