### Using collaborative open science tools to improve engagement with the 
# ecology of the Guana River Estuary
# Geraldine Klarenberg, PhD
# 17 May 2023
# gklarenberg@ufl.edu

# Visualizing trust data

# Start all runs of this script with:
renv::restore()
# This ensures it uses the packages last used when everything worked okay. This 
# also ensures these packages are installed if you don't have them

library(tidyverse)

#### Load data --------------------------------------------------
trust <- read_csv("data_deidentified/subsets/trust_results_basic.csv")

#### Make tabular outputs
# table() and prop.table()
TR0 <- trust %>% 
  filter(qname == "TR-0") %>% 
  group_by(q_text) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = cnt/sum(cnt)*100) # in RMarkdown/ Quarto, add %>% kable() for nice table

TR1 <- trust %>% 
  filter(qname == "TR-1") %>% 
  mutate(q_text = factor(q_text,levels = c("Not at all", "Not very much",
                                           "Somewhat", "Very much"))) %>% 
  group_by(q_text) %>% 
  count(q_text, name = "cnt", .drop = FALSE) %>%
  ungroup %>% 
  mutate(percentage = cnt/sum(cnt)*100)

TR2 <- trust %>% 
  filter(qname == "TR-2") %>% 
  mutate(q_text = factor(q_text,levels = c("Not at all capable", "Not very capable",
                                           "Somewhat capable", "Very capable"))) %>% 
  group_by(q_text) %>% 
  count(q_text, name = "cnt", .drop = FALSE) %>%
  ungroup %>% 
  mutate(percentage = cnt/sum(cnt)*100)


##### Fix TR-3 and 4 - NAs

TR5 <- trust %>% 
  filter(qname == "TR-5") %>% 
  mutate(q_text = factor(q_text,levels = c("Terribly", "Poorly",
                                           "Average", "Well", "Very well"))) %>% 
  group_by(q_text) %>% 
  count(q_text, name = "cnt", .drop = FALSE) %>%
  ungroup %>% 
  mutate(percentage = cnt/sum(cnt)*100)


TR6 <- trust %>% 
  filter(qname == "TR-6") %>% 
  mutate(q_text = factor(q_text,levels = c("Terribly", "Poorly",
                                           "Average", "Well", "Very well"))) %>% 
  group_by(q_text) %>% 
  count(q_text, name = "cnt", .drop = FALSE) %>%
  ungroup %>% 
  mutate(percentage = cnt/sum(cnt)*100)

#### Visualize dashboard preferences ------------------------------------------
# TR-0 Part of initial (kick-off) stakeholder group?
# TR-1 Trust in UF team
# TR-2 Trust in abilities and tech expertise
# TR-3 Satisfaction with ...
# TR-4 Agree/disagree statements
# TR-5 Scientific community engaging with local communities, in general
# TR-6 This project engaging with local community

##### TR-0 Part of initial (kick-off) stakeholder group? -----------------------

ggplot(TR0,
       aes(x = q_text, y = percentage, fill = q_text))+
  geom_col() +
  labs(x = "", y = "Percentage of respondents", title = "Is respondent part of kick-off stakeholder group?") +
  scale_fill_manual(values = c("turquoise3", "violet"))+
  theme_bw()+
  theme(legend.position = "none")+
  annotate(geom = "text", label = paste0("N = ", sum(TR0$cnt)), x = 2.4, y = 60)
ggsave("results/trust_coregroup.jpg")

##### TR-1 Trust in UF team ---------------------------------------------
ggplot(TR1,
       aes(x = percentage, y = fct_rev(q_text)))+
  geom_col(fill = "darkseagreen") +
  labs(y = "", x = "Percentage of respondents", title = "How much do you trust the UF project team to build a dashboard\nthat meets your needs?") +
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12)) +
  annotate(geom = "text", label = paste0("N = ", sum(TR1$cnt)), x = 57, y = 4.3)

ggsave("results/trust_UFteam.jpg")

##### TR-2 Trust in abilities and tech expertise -----------------------------

ggplot(TR2,
       aes(x = percentage, y = fct_rev(q_text)))+
  geom_col(fill = "mediumorchid2") +
  labs(y = "", x = "Percentage of respondents", title = "To what extent do you believe that the abilities and technical\nexpertise of the UF project team make them capable of\nbuilding a dashboard that meets your needs?") +
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12)) +
  annotate(geom = "text", label = paste0("N = ", sum(TR2$cnt)), x = 57, y = 4.3)

ggsave("results/trust_UFexpertise.jpg")

##### TR-3 Satisfaction with ... ---------------------------------------------

##### NEED TO GO BACK TO DATA EXTRACTION FILE. DATA ALL NAs

##### TR-4 Agree/disagree statements -------------------------------------------

##### NEED TO GO BACK TO DATA EXTRACTION FILE. DATA ALL NAs

##### TR-5 Scientific community engaging with local communities, in general ----

ggplot(TR5,
       aes(x = percentage, y = fct_rev(q_text)))+
  geom_col(fill = "steelblue2") +
  labs(y = "", x = "Percentage of respondents", 
       title = "In general, how well do you think the scientific community in general\ndoes in engaging with local communities?") +
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))+
  annotate(geom = "text", label = paste0("N = ", sum(TR5$cnt)), x = 45, y = 5.3)

ggsave("results/trust_generalengagement.jpg")


##### TR-6 This project engaging with local community -------------------------

ggplot(TR6,
       aes(x = percentage, y = fct_rev(q_text)))+
  geom_col(fill = "dodgerblue3") +
  labs(y = "", x = "Percentage of respondents", 
       title = "How well do you think this project has done in engaging with\nthe local community?") +
  theme_bw()+
  theme(legend.position = "none", axis.text.y = element_text(size=12))+
  annotate(geom = "text", label = paste0("N = ", sum(TR5$cnt)), x = 38, y = 5.3)

ggsave("results/trust_projectengagement.jpg")
