library(tidyverse)
library(haven)
library(ggrepel)
library(RColorBrewer)

load("Data/rel_final.RData")
load("Data/eth_final.RData")
load("Data/demog_final.RData")
load("Data/AgreementScores.RData")
dispute <- read_dta("Data/jpr-tprd-replicationdata.dta")

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







### Diagnostics

## Religious Affinity
rel_final %>% 
  ggplot() +
  geom_density(aes(x = rel_aff)) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Religious Affinity")


quantile(rel_final$rel_aff, seq(0, 1, .1))

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
  ggplot(aes(x = year, y = rel_aff, color = state_name)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral")) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Religious Affinity with the UK")
  
TR2015_comp %>% 
  filter(rel_aff != 0) %>% 
  ggplot(aes(x = rel_aff, view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick3') +
  geom_label_repel(aes(label = state_name), alpha = .8) +
  theme_bw() +
  labs(x = "Religious Affinity with Turkey",
       y = "Positive Turkish Public Opinion (%)")

US_2013_comp %>% 
  filter(rel_aff != 0) %>% 
  ggplot(aes(x = rel_aff, y = us_view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick3') +
  geom_label_repel(aes(label = state_name), alpha = .8) +
  theme_bw() +
  labs(x = "Religious Affinity with the U.S.",
       y = "Positive Public Opinion towards the U.S. (%)")
  

# Ethnic Affinity

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
  ggplot(aes(x = year, y = eth_aff, color = state_name)) +
  geom_point(size = .5) +
  geom_line() +
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral")) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Ethnic Affinity with the UK")


eth_final %>% 
  ggplot() +
  geom_density(aes(x = eth_aff)) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Ethnic Affinity")

quantile(eth_final$eth_aff, seq(.8, 1, .025))


TR2015_comp %>% 
  filter(eth_aff != 0) %>% 
  ggplot(aes(x = eth_aff, view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick3') +
  geom_label_repel(aes(label = state_name), alpha = .8) +
  theme_bw() +
  labs(x = "Ethnic Affinity with Turkey",
       y = "Positive Turkish Public Opinion (%)")

US_2013_comp %>% 
  filter(eth_aff != 0) %>% 
  ggplot(aes(x = eth_aff, y = us_view)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'firebrick3') +
  geom_label_repel(aes(label = state_name), alpha = .8) +
  theme_bw() +
  labs(x = "Ethnic Affinity with the U.S.",
       y = "Positive Public Opinion towards the U.S. (%)")
