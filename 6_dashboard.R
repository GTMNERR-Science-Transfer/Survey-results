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
         filter(qname_main == "T-1"), #%>% 
         # rename(choiceId = answer, qname = question) %>% 
         # filter(!is.na(choiceId)) %>% 
         # separate(col = qname, into = c("q", "choiceId"), sep = "_",
         #          remove = FALSE) %>% 
         # select(!q) %>% 
         # mutate(choiceId = as.numeric(choiceId)) %>% 
         # left_join(how),
       aes(y = fct_rev(fct_infreq(q_text)), fill = q_text))+
  geom_bar() +
  labs(y = "", x = "", title = "How would you prefer to access data?") +
  #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))

ggsave("results/dashboard_access.jpg")

####################### 08/02 DONE UP TO HERE
##### T-2 Type of information? (QID25) ---------------------------------------
type_classes <- mc_text_lookup %>% filter(ImportId == "QID25") %>% pull(q_text)
type_info <- data.frame(choiceId=c(1,2,3,4,5,6), type_classes)
ggplot(dashboard %>% 
         filter(str_detect(question, "T-2")) %>% 
         rename(choiceId = answer, qname = question) %>% 
         filter(!is.na(choiceId)) %>% 
         separate(col = qname, into = c("q", "choiceId"), sep = "_",
                  remove = FALSE) %>% 
         select(!q) %>% 
         mutate(choiceId = as.numeric(choiceId)) %>% 
         left_join(type_info),
       aes(y = fct_rev(fct_infreq(type_classes)), fill = type_classes))+
  geom_bar() +
  labs(y = "", x = "", title = "What type of information do you prefer?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "plasma")+
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))

ggsave("results/dashboard_type.jpg")

##### T-3 Form of information? (QID49) ----------------------------------------
form_classes <- mc_text_lookup %>% filter(ImportId == "QID49") %>% pull(q_text)
form_info <- data.frame(choiceId=c(5,6,9,7,8), form_classes)
ggplot(dashboard %>% # can add fill for preferences??
         filter(str_detect(question, "T-3")) %>% 
         rename(choiceId2 = answer, qname = question) %>% 
         filter(!is.na(choiceId2)) %>% 
         separate(col = qname, into = c("q", "choiceId"), sep = "_",
                  remove = FALSE) %>% 
         select(!q) %>% 
         mutate(choiceId = as.numeric(choiceId)) %>% 
         left_join(form_info),
       aes(y = fct_rev(fct_infreq(form_classes)), fill = factor(choiceId2)))+
  geom_bar() +
  labs(y = "", x = "", title = "What form of information do you prefer?") +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))+
  scale_fill_viridis_d(option = "rocket", name = "Rank")+
  theme_bw()+
  theme(axis.text.y = element_text(size=12))

ggsave("results/dashboard_form.jpg", width = 12, height = 7)

##### T-4 Data delivery? (QID24) ----------------------------------------------
