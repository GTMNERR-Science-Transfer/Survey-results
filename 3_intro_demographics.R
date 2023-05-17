### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 16 May 2023
# gklarenberg@ufl.edu

# Visualizing demographics and "intro" data

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

#### Load data ####
intro <- read_csv("data_deidentified/subsets/intro_results_basic.csv")
demogr <- read_csv("data_deidentified/subsets/demographics_results_basic.csv")
# Load the file with text details so these can be added to plots
q_text_lookup <- read_csv("metadata/questions_detail.csv")
# And load the file with multiple choice text and codes
mc_text_lookup <- read_csv("metadata/mc_questions_options.csv")

###### STILL FIX THIS (make nicer/more useful) AND ADD IT TO SCRIPT 1
# Join the latter two together so it is easier to get text for visualizations
names(mc_text_lookup)[4] <- "ImportId"
names(mc_text_lookup)[2] <- "choiceId"
text_lookup <- full_join(q_text_lookup, mc_text_lookup, by = c("ImportId", "choiceId"))


# Save for future ref
# aes(y = fct_rev(fct_infreq(factor(answer))))) + # Turn answer into factor
#   # so we can order according to frequency of occurence, and reverse to have highest
#   # on top

#### Visualize demographics ####
# DE-2 is age
# DE-3 is gender
# DE-4 is distance from GTM

ggplot(demogr %>% 
         filter(question == "DE-2") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(text_lookup %>% select(qname, choiceId, q_text)),
       aes(y = choiceId))+
  geom_bar(fill = "seagreen3") +
  labs(x = "Age group", y = "") +
  theme_bw()

ggplot(demogr %>% 
         filter(question == "DE-3") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(text_lookup %>% select(qname, choiceId, q_text)),
       aes(x = factor(choiceId), fill = factor(choiceId)))+
  geom_bar() +
  labs(x = "Gender", y = "") +
  theme_bw()

ggplot(demogr %>% 
         filter(question == "DE-4") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(text_lookup %>% select(qname, choiceId, q_text)),
       aes(y = choiceId))+
  geom_bar(fill = "royalblue") +
  labs(x = "Distance from GTM", y = "") +
  theme_bw()

#### Visualize introductory questions ####
# D-1 How are you connected?
# D-2 How often do you engage?
# D-3 What data generally interested in?
# D-4 Ever accessed data?

ggplot(intro %>% 
         filter(question == "D-2") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(text_lookup %>% select(qname, choiceId, q_text)),
       aes(y = factor(choiceId), fill = factor(choiceId)))+
  geom_bar() +
  labs(x = "How often do you engage with GTM?", y = "") +
  theme_bw()


ggplot(intro %>% 
         filter(question == "D-4") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(text_lookup %>% select(qname, choiceId, q_text)),
       aes(x = factor(choiceId), fill = factor(choiceId)))+
  geom_bar() +
  labs(x = "Accessed data?", y = "") +
  theme_bw()
