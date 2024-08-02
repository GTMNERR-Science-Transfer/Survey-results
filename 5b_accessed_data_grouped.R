### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 17 May 2023
# gklarenberg@ufl.edu

# Visualizing "have accessed" data - GROUPED PER DATASET

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)
library(cowplot)

#### Load data --------------------------------------------------
yes_data <- read_csv("2_data_deidentified/subsets/data_yes_results_basic.csv")

# This script groups the questions and answers together by dataset
# YD-2 - How do you most frequently obtain or access Guana Estuary data on [Field-1]?
# YD-3 - What are the advantages of this primary method of accessing or obtaining Guana Estuary data on [Field-1]?
# YD-4 - What are the disadvantages of the primary method of accessing or obtaining Guana Estuary data on [Field-1]?
# YD-5 - How often do/did you access or obtain Guana Estuary data on [Field-1]?
# YD-6 - What do you typically use Guana Estuary data on [Field-1] for?
# YD-7 - How well do Guana Estuary data on [Field-1] generally satisfy your need(s)?

# Create color schemes associated with factors (answers)

# There are 8 dataset options to choose, for data accessed. Loop over all of them.

for (i in 1:8){
  
  data_name <- yes_data %>% 
    filter(field_no == i) %>% 
    pull(field_name) 
  data_name <- data_name[1]
  
  obtain <- ggplot(yes_data %>% 
                     filter(field_no == i, qname_main == "YD-2") %>%
                     group_by(q_text) %>% 
                     summarize(cnt = n()) %>%
                     mutate(percentage = round(cnt/sum(cnt)*100, 1)),
         aes(y = reorder(q_text, percentage), x = percentage, fill = q_text))+
    geom_col() +
    labs(x = "", y = "", 
         title = data_name,
         subtitle = "How do you most frequently obtain or access these data?") +
    scale_fill_viridis_d(option = "viridis")+
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  adv <- ggplot(yes_data %>% 
                  filter(field_no == i, qname_main == "YD-3") %>% 
                  group_by(q_text) %>% 
                  summarize(cnt = n()) %>%
                  mutate(percentage = round(cnt/sum(cnt)*100, 1)),
                aes(y = reorder(q_text, percentage), x = percentage, fill = q_text))+
    geom_col() +
    labs(x = "", y = "", subtitle = "What are the advantages of this primary method of accessing or obtaining these data?") +
    scale_fill_viridis_d(option = "mako")+
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  disadv <- ggplot(yes_data %>% 
                     filter(field_no == i, qname_main == "YD-4") %>% 
                     group_by(q_text) %>% 
                     summarize(cnt = n()) %>%
                     mutate(percentage = round(cnt/sum(cnt)*100, 1)),
                   aes(y = reorder(q_text, percentage), x = percentage, fill = q_text))+
    geom_col() +
    labs(x = "", y = "", subtitle = "What are the disadvantages of this primary method of accessing or obtaining these data?") +
    scale_fill_viridis_d(option = "rocket")+
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  howoften <- ggplot(yes_data %>% 
                       filter(field_no == i, qname_main == "YD-5") %>% 
                       group_by(q_text) %>% 
                       summarize(cnt = n()) %>%
                       mutate(percentage = round(cnt/sum(cnt)*100, 1)),
                     aes(y = factor(q_text, levels = c("Daily", "At least once a week",
                                                       "2-3 times a month", "Once a month",
                                                       "Once every 6 months", "Once every year",
                                                       "Less than once a year")), # Create levels so they stay in the correct order
                         x = percentage))+
    geom_col(fill = "green3") +
    labs(x = "", y = "", subtitle = "How often do/did you access or obtain these data?") +
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  use <- ggplot(yes_data %>% 
                  filter(field_no == i, qname_main == "YD-6") %>% 
                  group_by(q_text) %>% 
                  summarize(cnt = n()) %>%
                  mutate(percentage = round(cnt/sum(cnt)*100, 1)),
                aes(y = reorder(q_text, percentage),
                x = percentage,
                fill = q_text))+
    geom_col() +
    labs(x = "", y = "", subtitle = "What do you typically use these data for?") +
    scale_fill_viridis_d(option = "plasma")+
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  satisf <- ggplot(yes_data %>% 
                     filter(field_no == i, qname_main == "YD-7") %>% 
                     group_by(q_text) %>% 
                     summarize(cnt = n()) %>%
                     mutate(percentage = round(cnt/sum(cnt)*100, 1)),
                   aes(y = factor(q_text,levels = c("Extremely well", "Very well",
                                                    "Moderately well", "Slightly well",
                                                    "Not well at all")),
                       x = percentage))+
    geom_col(fill = "skyblue2") +
    labs(x = "", y = "", subtitle = "How well do these data generally satisfy your need(s)?") +
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  all <- plot_grid(obtain, adv, disadv, howoften, use, satisf, ncol = 1, align = "v")
  ggsave(filename = paste0("8_results/yes_access_plots_", i, ".jpg"), plot = all,
         width = 7, height = 15)
}


