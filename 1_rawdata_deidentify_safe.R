### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 12 May 2023
# gklarenberg@ufl.edu

# Survey data extraction

# This script is a COPY of the original script that does the downloading and 
# deidentifying of data because the original includes my personal API key,
# and of course the de-identified data: hence confidential.
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
qualtrics_api_credentials(api_key = "YOUR-KEY",
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
# Get questions - I am getting these from the emailed survey, as this one also has
# the trust questions
questions <- survey_questions("SV_9RjNbEveqMo0MLk")
write_csv(questions, "metadata/survey_questions_all.csv")

# More detailed dataframe of questions and answer options (since our survey is
# complicated...)
questions_detail <- extract_colmap(fetch_survey(surveyID = "SV_9RjNbEveqMo0MLk"))
write_csv(questions_detail, "metadata/questions_detail.csv")

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

#### Get surveys ####

# Collect all our surveys (this can take a while to run)
rm(survey_data)
for (survey in all_survey_ids){
  survey_ind <- fetch_survey(surveyID = survey, # read survey data
                             # FYI without changing convert I could not get the 
                             # full_join to work properly. Some problem with the
                             # factor levels
                             #label = FALSE, # recoded values instead of text
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
  filter(`Informed Consent` == "I agree",
         Finished == TRUE,
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
               names_to = "question",
               values_to = "type_input") %>% 
  select(!question) %>%  # superfluous column now
  mutate(type_input = if_else(condition = (`Although you do not want to give further input on the project, would you like to receive email updates?` == "Yes"),
                              true = "Email updates only",
                              false = type_input,
                              missing = type_input)) %>% 
  distinct() # Remove duplicates
# Am not removing the NAs in type_input, this way we are also keeping people that
# said no to wanting to give further input and also no to receiving email updates.
# Maybe/probably still good info to have?

write_csv(survey_data_contacts, "data_deidentified/survey_data_contacts.csv")

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

write_csv(survey_data_safe, "data_deidentified/survey_data_safe_raw.csv")
