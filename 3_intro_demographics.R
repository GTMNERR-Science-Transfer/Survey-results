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

# Save for future ref
# aes(y = fct_rev(fct_infreq(factor(answer))))) + # Turn answer into factor
#   # so we can order according to frequency of occurrence, and reverse to have highest
#   # on top

#### Visualize demographics ####
# DE-2 is age 
# DE-3 is gender 
# DE-4 is distance from GTM 

ggplot(demogr %>% 
         filter(qname == "DE-2"),
       aes(y = fct_rev(factor(q_text))))+
  geom_bar(fill = "seagreen3") +
  labs(x = "", y = "", title = "What is your age group?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()
ggsave("results/demographics_age.jpg")

ggplot(demogr %>% 
         filter(qname == "DE-3"),
       aes(x = factor(q_text), fill = factor(q_text)))+
  geom_bar() +
  labs(x = "", y = "", title = "Gender") +
  scale_fill_manual(values = c("purple", "darkgreen"))+
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/demographics_gender.jpg")

ggplot(demogr %>% 
         filter(qname == "DE-4"),
       aes(y = q_text))+
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

ggplot(intro %>% 
         filter(str_detect(qname, "D-1")), 
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
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
# ORDER the 
ggplot(intro %>% 
         filter(qname == "D-2"), # %>% 
         #rename(choiceId = answer, qname = question) %>% 
         #left_join(engage),
       aes(y = fct_rev(q_text), fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "How often do you engage with the GTM?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/intro_GTM_engage.jpg")

# For this question, people could order their preferences
ggplot(intro %>% 
         filter(str_detect(qname, "D-3")),
       aes(y = fct_rev(fct_infreq(q_text)), fill = factor(order)))+
  geom_bar() +
  labs(y = "", x = "", title = "What datasets are you interested in?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "plasma", name = "Rank")+
  # add annotation with average score
  geom_text(data = intro %>% 
             filter(str_detect(qname, "D-3")) %>% 
             group_by(q_text) %>% summarize(ave_rank = mean(order, na.rm = TRUE)),
           aes(x = 5, y = q_text, label = ave_rank))+
  theme_bw()+
  theme(axis.text.y = element_text(size=15))

ggsave("results/intro_interests.jpg", width = 12, height = 7)

ggplot(intro %>% 
         filter(qname == "D-4"),# %>% 
         #rename(choiceId = answer, qname = question) %>% 
         #left_join(access),
       aes(x = q_text, fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "Have you accessed data before?") +
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/intro_data_access.jpg")