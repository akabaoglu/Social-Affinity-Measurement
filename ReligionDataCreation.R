library(tidyverse)
library(readxl)
library(haven)
library(sna)
library(intergraph)
library(igraph)
library(btergm)
library(network)
library(varhandle)
library(janitor)
library(foreach)

rm(list = ls())

### DATA LOADING
religion <- read_csv("Data/CREG.Rel.1.2.csv")

### 
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
        axis.title = element_blank())

rel_new <- religion %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(independent_country == 1, 
         !(group_name %in% c("other", "nonreligious", "traditionalbelief"))) %>% 
  group_by(cowcode, year, group_name) %>% 
  slice(1) %>% 
  ungroup()


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
  labs(y = "Total Coverage")
  







### DO NOT RUN

rel_aff <- c()

for (i in min(rel_new$year):max(rel_new$year)){
  countries <- rel_new %>% 
    filter(year == i) %>% 
    select(cowcode) %>% 
    pull() %>% 
    unique()
  
  rel_aff <- rbind(rel_aff, expand_grid(c1 = countries, c2 = countries, year = i))
}

rel_final <- rel_aff %>% 
  filter(c1 != c2)






fx <- function(x1, x2, y){
  
  c1 <- rel_new %>% 
    filter(cowcode == x1 & year == y) %>% 
    select(cowcode) %>%
    unique() %>% 
    pull()
  
  c2 <- rel_new %>% 
    filter(cowcode == x2 & year == y) %>% 
    select(cowcode) %>% 
    unique() %>% 
    pull()
  
  if(length(c1) != 0 & length(c2) != 0){
    c1_rel <- rel_new %>% 
      filter(cowcode == x1 & year == y) %>% 
      select(group_name) %>% 
      pull()
    
    c2_rel <- rel_new %>% 
      filter(cowcode == x2 & year == y) %>% 
      select(group_name) %>% 
      pull()
    
    c_int <- intersect(c1_rel, c2_rel)
    
    
    if (length(c_int > 0)){
      dat <- rep(0, length(c_int))
      
      for (i in 1:length(c_int)){
        p1 <- rel_new %>% filter(cowcode == x1 & year == y & group_name == c_int[i]) %>% select(group_proportion) %>% pull()
        p2 <- rel_new %>% filter(cowcode == x2 & year == y & group_name == c_int[i]) %>% select(group_proportion) %>% pull()
        
        dat[i] <- sqrt((p1/100)*(p2/100))
      }
      
      return(sum(dat))
    }
    
    else return(0)
  }
  
  else return(NA)
}

# rel_trial <- expand_grid(c1 = unique(rel_new$cowcode), c2 = unique(rel_new$cowcode), year = 1945:2000) %>% 
#   filter(c1 != c2) %>% 
#   sample_n(1, replace = F) %>% 
#   mutate(rel_aff = pmap_dbl(list(c1, c2, year), fx))
# 
# rel_trial
# 
#   
# 
# rel_new %>% 
#   filter(cowcode == 2 & year == 1945)
# 
# rel_new %>% 
#   filter(cowcode == 20 & year == 1945)


### Anomalies:

# 780, 530, 1951: christian and protestant/roman catholic were coded as different categories 
# 560, 483, 1999: ,,
# 698, 517, 1972: ,,
# 696, 484, 1991: muslim and shia/sunni were coded as different categories

###



n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

rel_affinity <- foreach(i = 1:nrow(rel_final),
                             .combine = "c",
                             .packages = c("tidyverse")) %dopar% 
  {fx(x1 = rel_final$c1[i], x2 = rel_final$c2[i], y = rel_final$year[i])}


parallel::stopCluster(cl = my.cluster)

rel_final$affinity <- rel_affinity

#save(rel_final, file = "Data/rel_final2.RData")





