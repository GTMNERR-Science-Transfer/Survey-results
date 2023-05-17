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
text_lookup <- right_join(q_text_lookup, mc_text_lookup, by = c("ImportId", "choiceId"))


# Save for future ref
# aes(y = fct_rev(fct_infreq(factor(answer))))) + # Turn answer into factor
#   # so we can order according to frequency of occurence, and reverse to have highest
#   # on top

#### Visualize demographics ####
# DE-2 is age (QID13)
# DE-3 is gender (QID14)
# DE-4 is distance from GTM (QID15)

# This label solution is TEMPORARY!!
age_classes <- mc_text_lookup %>% filter(ImportId == "QID13") %>% pull(q_text)
ages <- data.frame(choiceId=c(1,2,3,4,5,6,7,8), age_classes)

ggplot(demogr %>% 
         filter(question == "DE-2") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(ages),
       aes(y = fct_rev(factor(age_classes))))+
  geom_bar(fill = "seagreen3") +
  labs(x = "", y = "", title = "What is your age group?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()
ggsave("results/demographics_age.jpg")

gender_classes <- mc_text_lookup %>% filter(ImportId == "QID14") %>% pull(q_text)
gender <- data.frame(choiceId=c(1,2,3,4), gender_classes)
ggplot(demogr %>% 
         filter(question == "DE-3") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(gender),
       aes(x = factor(gender_classes), fill = factor(gender_classes)))+
  geom_bar() +
  labs(x = "", y = "", title = "Gender") +
  scale_fill_manual(values = c("purple", "darkgreen"))+
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/demographics_gender.jpg")

distance_classes <- mc_text_lookup %>% filter(ImportId == "QID15") %>% pull(q_text)
distance <- data.frame(choiceId=c(1,2,3,4), distance_classes)
ggplot(demogr %>% 
         filter(question == "DE-4") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(distance),
       aes(y = distance_classes))+
  geom_bar(fill = "royalblue") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  labs(x = "", y = "", title = "How far are you from the GTM?") +
  theme_bw()
ggsave("results/demographics_distance.jpg")

#### Visualize introductory questions ####
# D-1 How are you connected? (QID6)
# D-2 How often do you engage? (QID7)
# D-3 What data generally interested in? (QID8)
# D-4 Ever accessed data? (QID9)

connect_classes <- mc_text_lookup %>% filter(ImportId == "QID6") %>% pull(q_text)
connect <- data.frame(choiceId=c(1,2,3,4,5,6,7), connect_classes)
ggplot(intro %>% 
         filter(str_detect(question, "D-1")) %>% 
         rename(choiceId = answer, qname = question) %>% 
         filter(!is.na(choiceId)) %>% 
         separate(col = qname, into = c("q", "choiceId"), sep = "_",
                  remove = FALSE) %>% 
         select(!q) %>% 
         mutate(choiceId = as.numeric(choiceId)) %>% 
         left_join(connect),
       aes(y = fct_rev(fct_infreq(connect_classes)), fill = connect_classes))+
  geom_bar() +
  labs(y = "", x = "", title = "How are you connected to the GTM?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "mako")+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))

ggsave("results/intro_connected.jpg", width = 12, height = 7)

engage_classes <- mc_text_lookup %>% filter(ImportId == "QID7") %>% pull(q_text)
engage <- data.frame(choiceId=c(1,2,3,4,5,6,7), engage_classes)
engage$engage_classes <- factor(engage$engage_classes, 
                                levels = c("Daily", "At least once a week",
                                           "2-3 times a month", "Once a month",
                                           "Once every 6 months", "Once every year",
                                           "Less than once a year"))
ggplot(intro %>% 
         filter(question == "D-2") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(engage),
       aes(y = fct_rev(engage_classes), fill = engage_classes))+
  geom_bar() +
  labs(x = "", y = "", title = "How often do you engage with the GTM?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/intro_GTM_engage.jpg")

# For this question, people could order their preferences
# This plot does not show the preferences
interest_classes <- mc_text_lookup %>% filter(ImportId == "QID8") %>% pull(q_text)
interest <- data.frame(choiceId=c(1,2,3,4,5,6,7,8,9), interest_classes)
ggplot(intro %>% 
         filter(str_detect(question, "D-3")) %>% 
         rename(choiceId = answer, qname = question) %>% 
         filter(!is.na(choiceId)) %>% 
         separate(col = qname, into = c("q", "choiceId2"), sep = "_",
                  remove = FALSE) %>% 
         select(!q) %>% 
         mutate(choiceId2 = as.numeric(choiceId2)) %>% 
         left_join(interest),
       aes(y = fct_rev(fct_infreq(interest_classes)), fill = interest_classes))+
  geom_bar() +
  labs(y = "", x = "", title = "What datasets are you interested in?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "plasma")+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=15))
ggsave("results/intro_interests.jpg", width = 12, height = 7)

access_classes <- mc_text_lookup %>% filter(ImportId == "QID9") %>% pull(q_text)
access <- data.frame(choiceId=c(1,2), access_classes)
ggplot(intro %>% 
         filter(question == "D-4") %>% 
         rename(choiceId = answer, qname = question) %>% 
         left_join(access),
       aes(x = access_classes, fill = access_classes))+
  geom_bar() +
  labs(x = "", y = "", title = "Have you accessed data before?") +
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/intro_data_access.jpg")