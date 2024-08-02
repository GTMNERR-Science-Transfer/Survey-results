### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 17 May 2023
# gklarenberg@ufl.edu

# Visualizing "have not accessed" data

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

#### Load data -----------------------------------
no_access <- read_csv("2_data_deidentified/subsets/data_no_results_basic.csv")

# Questions
# ND-1 If you had opportunity to access data in future, how often?
# ND-2 If you had opportunity to access data in future, what type of data?

#### If opportunity, how often? -----------------------------------

ggplot(no_access %>% 
         filter(str_detect(qname, "ND-1")) %>% 
         mutate(q_text = factor(q_text, levels = c("Daily", "At least once a week",
                                                   "2-3 times a month", "Once a month",
                                                   "Once every 6 months", "Once every year",
                                                   "Less than once a year",
                                                   "Never (not interested in accessing data)"))),
       aes(y = fct_rev(q_text)))+
  geom_bar(fill = "cyan4") +
  labs(y = "", x = "", title = "If you had the opportunity to access data in the future, how often would you access or obtain these data?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()+
  theme(axis.text.y = element_text(size=15))
ggsave("8_results/no_access_how_often.jpg", width = 12, height = 7)

#### If opportunity, what type of data? -----------------------------------
ggplot(no_access %>% 
         filter(str_detect(qname, "ND-2")),
       aes(y = fct_rev(fct_infreq(q_text))))+
  geom_bar(fill = "coral2") +
  labs(y = "", x = "", title = "If you had the opportunity to access data in the future,\nwhat would you use these data for?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  theme_bw()+
  theme(axis.text.y = element_text(size=15))
ggsave("8_results/no_access_interests.jpg", width = 12, height = 7)
