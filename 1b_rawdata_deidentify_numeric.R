### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 19 June 2023
# gklarenberg@ufl.edu

#### Survey data extraction ####

# THIS SCRIPT ONLY WORKS ON THE RAW **NON DE-IDENTIFIED DATA** 

# You will need to run the script that sets your Qualtrics key FIRST, before running
# this script. This script will only work if you have admin access to the project's
# suvreys (see below).
# If you have questions about this script or need to access the raw data, please
# contact the author of this script (see above).

#### NOTE for project collaborators: when running this script, make sure the raw 
# data is saved to your hard disk. De-identified data is confidential and is not
# allowed to be stored in the cloud or shared drives etc. As per IRB regulations.

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

#### API credentials were set up in file 0a ####

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

#### Get surveys ####

# Collect all our surveys (this can take a while to run)
if(exists("survey_data")){
  rm(survey_data)
}
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

# Filter for those who agreed and for those who did not finalize the survey
survey_data_unfinished <- survey_data %>% 
  filter(`Informed Consent` == 1,
         Finished == 0)
# Print to screen how many unfinished surveys...
nrow(survey_data_unfinished)
# None of these probably have email addresses in them, but just in case, saving
# these to the confidential folder (not tracked by git)
write_csv(survey_data_unfinished, "survey_downloads_confidential/survey_data_unfinished_numeric_raw.csv")

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
questions_detail <- read_csv("metadata/mc_questions_options.csv")
names(survey_data_contacts) <- questions_detail %>% 
  filter(str_detect(qname, "F-")) %>% 
  filter(!duplicated(qname)) %>% 
  pull(main) # pull instead of select gives a vector, which what we need
# for renaming the headers
names(survey_data_contacts)[2] <- paste0(names(survey_data_contacts)[2], " - meetings") 
names(survey_data_contacts)[3] <- paste0(names(survey_data_contacts)[3], " - surveys") 
names(survey_data_contacts)[4] <- paste0(names(survey_data_contacts)[4], " - testing") 

# Make long version
survey_data_contacts <- survey_data_contacts %>%
  pivot_longer(cols = 2:4,
               names_to = "input_type",
               #names_prefix = "`What would be your preferred method of giving us feedback and input? (check all that apply) - `",
               names_prefix = "^[^_]*- ",
               values_to = "selected") %>%
  mutate(selected = replace_na(selected, 0)) %>% 
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

