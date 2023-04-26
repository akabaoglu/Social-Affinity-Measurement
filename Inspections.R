library(tidyverse)
library(haven)
library(ggrepel)
library(RColorBrewer)

load("Data/rel_final2.RData")
load("Data/eth_final2.RData")
load("Data/demog_final.RData")
load("Data/AgreementScores.RData")
dispute <- read_dta("Data/jpr-tprd-replicationdata.dta")
religion <- read_csv("Data/CREG.Rel.1.2.csv")
ethnicity <- read_csv("Data/CREG.Eth.1.2.csv")

dispute_trimmed <- dispute %>% 
  select(dyad, year) %>% 
  mutate(dyad = str_pad(as.character(dyad), width = 6, side = 'left', pad = "0"),
         c1 = as.double(substr(dyad, 1, 3)),
         c2 = as.double(substr(dyad, 4, 6)))

US_2013 <- tribble(~state_name, ~ccode, ~us_view, ~year,
                   "South Korea", 732, .78, 2013,
                   "Italy", 325, .76, 2013,
                   "Japan", 740, .69, 2013,
                   "Philippines", 840, .85, 2013,
#                   "Ghana",  452, .83, 2013,
                   "Israel", 666, .83, 2013,
#                   "Kenya", 501, .81, 2013,
#                   "Senegal", 433, .81, 2013,
                   "El Salvador", 92, .70, 2013,
                   "Brazil", 140, .73, 2013,
                   "Uganda", 500, .73, 2013,
                   "South Africa", 560, .72, 2013,
#                   "Nigeria", 475, .69, 2013,
                   "Chile", 155, .68, 2013, 
                   "Poland", 290, .67, 2013,
                   "Australia", 900, .66, 2013,
                   "Mexico", 70, .66, 2013,
                   "Canada", 20, .64, 2013,
                   "France", 220, .64, 2013,
                   "Spain", 230, .62, 2013,
                   "Indonesia", 850, .61, 2013,
                   "Czech Republic", 316, .58, 2013,
                   "United Kingdom", 200, .58, 2013,
                   "India", 750, .56, 2013,
                   "Bolivia", 145, .55, 2013,
                   "Malaysia", 820, .55, 2013,
                   "Germany", 255, .53, 2013,
                   "Venezuela", 101, .53, 2013,
                   "Russia", 365, .51, 2013,
                   "Lebanon", 660, .47, 2013,
                   "Tunisia", 616, .42, 2013,
#                   "Argentina", 160, .41, 2013,
                   "China", 710, .40, 2013,
                   "Greece", 350, .39, 2013,
                   "Turkey", 640, .21, 2013,
                   "Egypt", 651, .16, 2013,
                   "Jordan", 663, .14, 2013,
                   "Pakistan", 770, .11, 2013)

US_2013_comp <- US_2013 %>% left_join(demog_final %>% 
                                        select(c(1:3, 6)) %>% 
                                        filter(year == 1994, c1 == 2) %>% 
                                        select(-year), by = c("ccode" = "c2")) %>% 
  select(c(1:3, 6)) %>% 
  left_join(rel_final %>% 
              filter(c1 == 2, year == 2013) %>% 
              select(c(2,4)), by = c("ccode" = "c2")) %>% 
  left_join(dfAgree %>% 
              filter(ccode1 == 2, year == 2013) %>% 
              ungroup() %>% 
              select(c(3,4)), by = c("ccode" = "ccode2")) %>% 
  left_join(eth_final %>% 
              filter(c1 == 2, year == 2013) %>% 
              select(c(2,4)), by = c("ccode" = "c2")) %>% 
  left_join(dispute_trimmed %>% 
              mutate(dispute = 1) %>% 
              group_by(c1, c2) %>% 
              count() %>% 
              mutate(n = 1) %>% 
              rename("dispute" = n) %>% 
              ungroup() %>% 
              filter(c1 == 2), by = c("ccode" = "c2")) %>% 
  mutate(dispute = if_else(is.na(dispute), 0, 1)) %>% 
  select(-c1)


dat_TR_2015 <- tribble(~state_name, ~ccode, ~view,
                       "Azerbaijan", 373, .63,
                       "Germany", 260, .38,
                       "Saudi Arabia", 670, .38,
                       "Sweden", 380, .28,
                       "Russia", 365, .26,
                       "Iran", 630, .24,
                       "USA", 20, .23, 
                       "France", 220, .21,
                       "Greece", 350, .15,
                       "PRC", 710, .14,
                       "Syria", 652, .14,
                       "United Kingdom", 200, .13,
                       "Armenia", 371, .1,
                       "Israel", 666, .08) %>% 
  mutate(year = 2013)

TR2015_comp <- dat_TR_2015 %>% 
  left_join(demog_final %>% 
              select(c(1:3, 6)) %>% 
              filter(year == 1994, c1 == 640), by = c("ccode" = "c2")) %>% 
  select(c(1:3, 7)) %>% 
  left_join(rel_final %>% 
              filter(c1 == 640, year == 2013) %>% 
              select(c(2,4)), by = c("ccode" = "c2")) %>% 
  left_join(dfAgree %>% 
              filter(ccode1 == 640, year == 2013) %>% 
              ungroup() %>% 
              select(c(3,4)), by = c("ccode" = "ccode2")) %>% 
  left_join(eth_final %>% 
              filter(c1 == 640, year == 2013) %>% 
              select(c(2,4)), by = c("ccode" = "c2")) %>% 
  left_join(dispute_trimmed %>% 
              mutate(dispute = 1) %>% 
              group_by(c1, c2) %>% 
              count() %>% 
              mutate(n = 1) %>% 
              rename("dispute" = n) %>% 
              ungroup() %>% 
              filter(c2 == 640), by = c("ccode" = "c1")) %>% 
  mutate(dispute = if_else(is.na(dispute), 0, 1)) %>% 
  select(-c2)

rel_new <- religion %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(independent_country == 1, 
         !(group_name %in% c("other", "nonreligious", "traditionalbelief"))) %>% 
  group_by(cowcode, year, group_name) %>% 
  slice(1) %>% 
  ungroup()

eth_new <- ethnicity %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(group_name != "other",
         independent_country == 1) 


### Dataset Diagnostics

## Religion
religion %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(independent_country == 1, 
         group_name %in% c("other", "nonreligious", "traditionalbelief")) %>% 
  mutate(region = floor(cowcode/100),
         group_name = as_factor(group_name)) %>% 
  group_by(cowcode, group_name) %>% 
  summarize(across(c(group_proportion, region), ~ mean(.x, na.rm = T))) %>% 
  mutate(group_name = if_else(group_name == "other", "Other", 
                              if_else(group_name == "nonreligious", "Non-Religious", "Traditional Belief")),
         region = as_factor(region),
         group_proportion = group_proportion/100) %>% 
  ggplot(aes(x = region)) +
  geom_boxplot(aes(y = group_proportion)) +
  facet_wrap(~group_name) +
  scale_x_discrete(labels = c("N.America", "S.America", "W.Europe", 
                              "E.Europe", "C.Africa", "S.Africa", 
                              "MENA", "C.Asia", "SE.Asia", "Oceanica")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .375),
        axis.title = element_blank()) +
  theme(text = element_text(family = "Times New Roman"))


rel_new %>% 
  group_by(cowcode, year) %>% 
  summarize(total_cov = sum(group_proportion), .groups = 'drop') %>% 
  mutate(total_cov = if_else(total_cov > 100, 100, total_cov)) %>% 
  group_by(cowcode) %>% 
  summarize(means = mean(total_cov)) %>% 
  mutate(region = as_factor(floor(cowcode/100))) %>% 
  ggplot(aes(x = region)) +
  geom_boxplot(aes(y = means/100)) +
  scale_x_discrete(labels = c("N.America", "S.America", "W.Europe", 
                              "E.Europe", "C.Africa", "S.Africa", 
                              "MENA", "C.Asia", "SE.Asia", "Oceanica")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .375),
        axis.title.x = element_blank()) +
  labs(y = "Total Coverage") +
  theme(text = element_text(family = "Times New Roman"))

## Ethnicity

ethnicity %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(group_name %in% c("other"),
         independent_country == 1) %>% 
  group_by(cowcode, group_name) %>% 
  summarize(means = mean(group_proportion)) %>% 
  mutate(region = floor(cowcode/100)) %>% 
  ggplot(aes(x = as_factor(region))) +
  geom_boxplot(aes(y = means/100)) +
  scale_x_discrete(labels = c("N.America", "S.America", "W.Europe", 
                              "E.Europe", "C.Africa", "S.Africa", 
                              "MENA", "C.Asia", "SE.Asia", "Oceanica")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .375),
        axis.title.x = element_blank()) +
  labs(y = "Other") +
  theme(text = element_text(family = "Times New Roman"))

eth_new %>% 
  group_by(cowcode, year) %>% 
  summarize(total_cov = sum(group_proportion), .groups = 'drop') %>% 
  mutate(total_cov = if_else(total_cov > 100, 100, total_cov)) %>% 
  group_by(cowcode) %>% 
  summarize(means = mean(total_cov)) %>% 
  mutate(region = as_factor(floor(cowcode/100))) %>% 
  ggplot(aes(x = region)) +
  geom_boxplot(aes(y = means/100)) +
  scale_x_discrete(labels = c("N.America", "S.America", "W.Europe", 
                              "E.Europe", "C.Africa", "S.Africa", 
                              "MENA", "C.Asia", "SE.Asia", "Oceanica")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .375),
        axis.title.x = element_blank()) +
  labs(y = "Total Coverage") +
  theme(text = element_text(family = "Times New Roman"))


### Measurement Diagnostics

## Religious Affinity
rel_final %>% 
  ggplot() +
  geom_density(aes(x = affinity)) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Religious Affinity") +
  theme(text = element_text(family = "Times New Roman"))


quantile(rel_final$affinity, seq(0, 1, .1))

rel_final %>% 
  filter(c1 == 200, c2 %in% c(20, 70, 260, 437, 640, 666, 732, 750)) %>% 
  left_join(tribble(~c2, ~state_name,
                     20, "Canada",
                     70, "Mexico",
                    260, "Germany",
                    437, "Ghana",
                    640, "Turkey",
                    666, "Israel",
                    732, "South Korea",
                    750, "India"), by = "c2") %>% 
  mutate(state_name = as_factor(state_name)) %>% 
  ggplot(aes(x = year, y = affinity, color = state_name)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral")) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Religious Affinity with the U.K.") +
  theme(text = element_text(family = "Times New Roman"))
  
TR2015_comp %>% 
  filter(affinity.x != 0) %>% 
  ggplot(aes(x = affinity.x, view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick2') +
  geom_label_repel(aes(label = state_name), alpha = .8, family = "Times New Roman") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Religious Affinity with Turkey",
       y = "Positive Turkish Public Opinion") +
  theme(text = element_text(family = "Times New Roman"))

US_2013_comp %>% 
  filter(affinity.x != 0) %>% 
  ggplot(aes(x = affinity.x, y = us_view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick2') +
  geom_label_repel(aes(label = state_name), alpha = .8, family = "Times New Roman") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Religious Affinity with the U.S.",
       y = "Positive Public Opinion towards the U.S.") +
  theme(text = element_text(family = "Times New Roman"))
  

# Ethnic Affinity
eth_final %>% 
  ggplot() +
  geom_density(aes(x = affinity)) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Ethnic Affinity") +
  theme(text = element_text(family = "Times New Roman"))
  

quantile(eth_final$affinity, seq(.8, 1, .025))


eth_final %>% 
  filter(c1 == 200, c2 %in% c(205, 660, 101, 95, 590, 694, 41, 950)) %>% 
  left_join(tribble(~c2, ~state_name,
                    205, "Ireland",
                    660, "Lebanon",
                    101, "Venezuela",
                     95, "Panama",
                    590, "Mauritus",
                    694, "Qatar",
                     41, "Haiti",
                    950, "Fiji"), by = "c2") %>% 
  mutate(state_name = as_factor(state_name)) %>% 
  ggplot(aes(x = year, y = affinity, color = state_name)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral")) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Ethnic Affinity with the U.K.") +
  theme(text = element_text(family = "Times New Roman"))


TR2015_comp %>% 
  filter(affinity.y != 0) %>% 
  ggplot(aes(x = affinity.y, view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick2') +
  geom_label_repel(aes(label = state_name), alpha = .8, family = "Times New Roman") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Ethnic Affinity with Turkey",
       y = "Positive Turkish Public Opinion") +
  theme(text = element_text(family = "Times New Roman"))

US_2013_comp %>% 
  filter(affinity.y != 0) %>% 
  ggplot(aes(x = affinity.y, y = us_view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick2') +
  geom_label_repel(aes(label = state_name), alpha = .8, family ="Times New Roman")  +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Ethnic Affinity with the U.S.",
       y = "Positive Public Opinion towards the U.S.") +
  theme(text = element_text(family = "Times New Roman"))
