### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 12 May 2023
# gklarenberg@ufl.edu

# Survey data extraction

# THIS IS THE RAW **NON DE-IDENTIFIED DATA** and it includes my personal
# API key: hence confidential

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
library("qualtRics")
library("tidyverse")

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
qualtrics_api_credentials(api_key = "mPL8Fr97QRvWHw7TFXCmBDDwVvsS6quKdnMwQcT2",
                          base_url = "ca1.qualtrics.com",
                          install = TRUE)

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
# the trust questions
questions <- survey_questions("SV_9RjNbEveqMo0MLk")
write_csv(questions, "metadata/survey_questions_all.csv")

# More detailed dataframe of questions and answer options (since our survey is
# complicated...)
questions_detail <- extract_colmap(fetch_survey(surveyID = "SV_9RjNbEveqMo0MLk"))
# Clean this up so we can easily match codes and text later. Do this separate for 
# "regular" questions, and the questions on data that people accessed (as this 
# has more complicated codes), and then joi later.

# "regular" questions
questions_detail2 <- questions_detail %>%
  filter(!str_detect(qname, "^[0-9]")) %>% 
  separate(qname, c("qname_main", "option", "option1"), sep = "_", remove = FALSE)

questions_detail3 <- questions_detail %>%
  filter(str_detect(qname, "^[0-9]")) %>% 
  separate(qname, c("field_no", "qname_main", "option", "option1"), sep = "_", remove = FALSE)

questions_detail <- full_join(questions_detail2, questions_detail3)
# Remove description as it is main and sub combined, and timeZone.
# Column choiceId is less comprehensive than option, so also remove
questions_detail <- questions_detail %>% 
  select(-c("description","timeZone", "choiceId"))
#write_csv(questions_detail, "metadata/questions_detail.csv")

# Accessing the questions metadata to get the multiple choice codes and text
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
    mc_info_all <- rbind(mc_info_all, mc_info_q)  
  }
}

# Combine questions_detail and mc_info_all
questions_detail_2 <- full_join(questions_detail, mc_info_all)

##### still working on this
test = left_join(mc_info_all, questions_detail, by = "ImportId")

write_csv(mc_info_all, "metadata/mc_questions_options.csv")

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


#### Get surveys ####

# Collect all our surveys (this can take a while to run)
rm(survey_data)
for (survey in all_survey_ids){
  survey_ind <- fetch_survey(surveyID = survey, # read survey data
                             # FYI without changing convert I could not get the 
                             # full_join to work properly. Some problem with the
                             # factor levels
                             label = FALSE, # recoded values instead of text
                             convert = FALSE, # no conversion to proper data type
                             force_request = TRUE, # this forces a download (instead 
                             # of loading existing temporary data downloaded earlier)
                             save_dir = "survey_downloads_confidential") # surveys will be saved 
  # as RDS files in this directory
  if (nrow(survey_ind != 0)){ # only if the survey has results, attach them
    if (!exists("survey_data")){ # if survey_data does not yet exist, create it
      survey_ind$source <- names(all_survey_ids[which(all_survey_ids == survey)])
      survey_data <- survey_ind
    } else { # if it does exist, add on
      survey_ind$source <- names(all_survey_ids[which(all_survey_ids == survey)])
      survey_data <- full_join(survey_data, survey_ind)
    } 
  }
}

# Filter for those who agreed and for those who finalized the survey
survey_data <- survey_data %>% 
  filter(`Informed Consent` == 1,
         Finished == 1,
         `F-4` != "Sbaker25@ufl.edu") # Remove test survey Shirley did

# Save email addresses of people that want to stay involved in a separate file
# These questions are F-1 through F-4
survey_data_contacts <- survey_data %>% 
  select(starts_with("F-"))
# Add proper questions as headers instead of codes
names(survey_data_contacts) <- questions_detail %>% 
  filter(str_detect(qname, "F-")) %>% 
  pull(description) # pull instead of select gives a vector, which what we need
# for renaming the headers

# Make long version
survey_data_contacts <- survey_data_contacts %>%
  pivot_longer(cols = 2:4,
               names_to = "input_type",
               #names_prefix = "`What would be your preferred method of giving us feedback and input? (check all that apply) - `",
               names_prefix = "^[^_]*- ",
               values_to = "selected") %>%
  distinct() # Remove duplicates
# Am not removing the NAs in type_input, this way we are also keeping people that
# said no to wanting to give further input and also no to receiving email updates.
# Maybe/probably still good info to have?

write_csv(survey_data_contacts, "data_deidentified/survey_data_contacts_num.csv")

# De-identify data
# Remove email addresses, IPAddress, latitude and longitude 
# Assign random unique IDs
set.seed(7) # Set a seed so we get the same randomized numbers every time
random_IDs <- sample(x = 1:nrow(survey_data), 
                     size = nrow(survey_data), 
                     replace = FALSE)
survey_data_safe <- survey_data %>% 
  select(!c(starts_with("F-"), "IPAddress", "LocationLatitude", "LocationLongitude")) %>% 
  mutate(ID = random_IDs)

# Tidy up: remove unnecessary columns
survey_data_safe <- survey_data_safe %>% 
  select(!c("Status", "Progress", "Finished", "ResponseId", "RecipientLastName",
            "RecipientFirstName", "RecipientEmail", "ExternalReference", 
            "DistributionChannel", "UserLanguage", "Informed Consent"))

write_csv(survey_data_safe, "data_deidentified/survey_data_safe_numeric_raw.csv")

