### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 17 May 2023
# gklarenberg@ufl.edu

# Visualizing dashboard data

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

#### Load data --------------------------------------------------
dashboard <- read_csv("data_deidentified/subsets/dashboard_results_basic.csv")

#### Visualize dashboard preferences ------------------------------------------
# T-1 How to access? (QID23)
# T-2 Type of information? (QID25)
# T-3 Form of information? (QID49)
# T-4 Data delivery? (QID24)
# All these questions are questions where people could pick several items

##### T-1 How to access? (QID23) ---------------------------------------------

ggplot(dashboard %>% 
         filter(qname_main == "T-1"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(y = "", x = "", title = "How would you prefer to access data?") +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))

ggsave("results/dashboard_access.jpg")

##### T-2 Type of information? (QID25) ---------------------------------------
ggplot(dashboard %>% 
         filter(qname_main == "T-2"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(y = "", x = "", title = "What type of information do you prefer?") +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "plasma")+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))

ggsave("results/dashboard_type.jpg")

##### T-3 Form of information? (QID49) ----------------------------------------
ggplot(dashboard %>% 
         filter(qname_main == "T-3"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = factor(order)))+
  geom_bar() +
  labs(y = "", x = "", title = "What form of information do you prefer?") +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "rocket", name = "Rank")+
  theme_bw()+
  theme(axis.text.y = element_text(size=12))

ggsave("results/dashboard_form1.jpg", width = 12, height = 7)

# Similar plot, but with the average ranks (1 = most interested)
ggplot(dashboard %>% 
         filter(qname_main == "T-3") %>% 
         group_by(q_text) %>% summarize(ave_rank = mean(order, na.rm = TRUE)) %>% 
         mutate(q_text = fct_reorder(q_text, ave_rank)),
       aes(x = ave_rank, y = fct_rev(q_text))) +
  geom_col(fill = "royalblue")+
  labs(y = "", x = "", title = "What format of data delivery would suit your needs best?\nAverage ranking (1 = most interested in)")+
  theme_bw()+
  theme(axis.text.y = element_text(size=15))
ggsave("results/dashboard_form2.jpg", width = 12, height = 7)

##### T-4 Data delivery? (QID24) ----------------------------------------------

ggplot(dashboard %>% 
         filter(qname_main == "T-4"),
       aes(y = fct_rev(fct_infreq(q_text)), fill = factor(order)))+
  geom_bar() +
  labs(y = "", x = "", title = "What format of data delivery would suit your needs best?") +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "rocket", name = "Rank")+
  theme_bw()+
  theme(axis.text.y = element_text(size=12))

ggsave("results/dashboard_delivery1.jpg", width = 12, height = 7)

# Similar plot, but with the average ranks (1 = most interested)
ggplot(dashboard %>% 
         filter(qname_main == "T-4") %>% 
         group_by(q_text) %>% summarize(ave_rank = mean(order, na.rm = TRUE)) %>% 
         mutate(q_text = fct_reorder(q_text, ave_rank)),
       aes(x = ave_rank, y = fct_rev(q_text))) +
  geom_col(fill = "royalblue")+
  labs(y = "", x = "", title = "What format of data delivery would suit your needs best?\nAverage ranking (1 = most interested in)")+
  theme_bw()+
  theme(axis.text.y = element_text(size=15))
ggsave("results/dashboard_delivery2.jpg", width = 12, height = 7)