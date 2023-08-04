### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 17 May 2023
# gklarenberg@ufl.edu

# Visualizing "have accessed" data

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

#### YD-1 Which datasets accessed? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-1"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  labs(x = "", y = "", title = "What Guana Estuary data do you currently use,\nor have you used in the past?") +
  theme_bw()+
  theme(legend.position = "none")
ggsave("results/yes_access_which_data.jpg")

#### YD-2 How is data obtained? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-2"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "How do you most frequently obtain or access Guana Estuary data?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
ggsave("results/yes_access_how.jpg", width = 12, height = 7)

#### Add something here on "other" options, and website addresses

#### YD-3 Advantages? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-3"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "What are the advantages of this primary method of\naccessing or obtaining Guana Estuary data?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
#### Change this to percentages?
ggsave("results/yes_access_advantages.jpg", width = 12, height = 7)

#### YD-4 Disadvantages? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-4"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "What are the disadvantages of this primary method of\naccessing or obtaining Guana Estuary data?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
#### Change this to percentages?
ggsave("results/yes_access_disadvantages.jpg", width = 12, height = 7)

#### YD-5 How often? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-5"),
       aes(y = factor(q_text, levels = c("Daily", "At least once a week",
                                         "2-3 times a month", "Once a month",
                                         "Once every 6 months", "Once every year",
                                         "Less than once a year")), # Create levels so they stay in the correct order
           fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "How often do/did you access or obtain Guana Estuary data?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
ggsave("results/yes_access_how_often.jpg", width = 12, height = 7)

# Other option:
ggplot(yes_data %>% 
         filter(qname_main == "YD-5") %>% 
         group_by(field_name, q_text) %>% 
         summarize(tot_count = n()),
       aes(y = field_name,
           x = tot_count, color = factor(q_text, levels = c("Daily", "At least once a week",
                                                            "2-3 times a month", "Once a month",
                                                            "Once every 6 months", "Once every year",
                                                            "Less than once a year"))))+
  geom_point() +
  labs(x = "", y = "", title = "How often do/did you access or obtain Guana Estuary data?") +
  scale_color_brewer(palette = "RdYlBu", name = "")+
  theme_bw()

#### YD-6 Use for? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-6"),
       aes(y = q_text,
           fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "What do you typically use Guana Estuary data for?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
ggsave("results/yes_access_uses.jpg", width = 12, height = 7)

#### YD-7 Satisfaction? -----------------------------------------------
ggplot(yes_data %>% 
         filter(qname_main == "YD-7"),
       aes(y = factor(q_text,levels = c("Extremely well", "Very well",
                                        "Moderately well", "Slightly well",
                                        "Not well at all")),
           fill = q_text))+
  geom_bar() +
  labs(x = "", y = "", title = "How well do Guana Estuary data generally satisfy your need(s)?") +
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(.~ field_name)
ggsave("results/yes_access_satisfaction.jpg", width = 12, height = 7)

###### Other approach: make all 6 plots per field and put together - probably
# better in terms of figuring out for each dataset what the issues are?