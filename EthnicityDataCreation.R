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

ethnicity <- read_csv("Data/CREG.Eth.1.2.csv")


### 

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
  labs(y = "Other")

eth_new <- ethnicity %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(group_name != "other",
         independent_country == 1) 

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
  labs(y = "Total Coverage")







### DO NOT RUN

eth_aff <- c()

for (i in min(eth_new$year):max(eth_new$year)){
  countries <- eth_new %>% 
    filter(year == i) %>% 
    select(cowcode) %>% 
    pull() %>% 
    unique()
  
  eth_aff <- rbind(eth_aff, expand_grid(c1 = countries, c2 = countries, year = i))
}


eth_final <- eth_aff %>% 
  filter(c1 != c2)








fx <- function(x1, x2, y){
  
  c1 <- eth_new %>% 
    filter(cowcode == x1 & year == y) %>% 
    select(cowcode) %>%
    unique() %>% 
    pull()
  
  c2 <- eth_new %>% 
    filter(cowcode == x2 & year == y) %>% 
    select(cowcode) %>% 
    unique() %>% 
    pull()
  
  if(length(c1) != 0 & length(c2) != 0){
    c1_ids <- eth_new %>% 
      filter(cowcode == x1 & year == y) %>% 
      select(group_name) %>% 
      pull()
    
    c2_ids <- eth_new %>% 
      filter(cowcode == x2 & year == y) %>% 
      select(group_name) %>% 
      pull()
    
    c_int <- intersect(c1_ids, c2_ids)
    
    
    if (length(c_int > 0)){
      dat <- rep(0, length(c_int))
      
      for (i in 1:length(c_int)){
        p1 <- eth_new %>% filter(cowcode == x1 & year == y & group_name == c_int[i]) %>% select(group_proportion) %>% pull()
        p2 <- eth_new %>% filter(cowcode == x2 & year == y & group_name == c_int[i]) %>% select(group_proportion) %>% pull()
        
        dat[i] <- sqrt((p1/100)*(p2/100))
      }
      
      return(sum(dat))
    }
    
    else return(0)
  }
  
  else return(NA)
}




n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

eth_affinity <- foreach(i = 1:nrow(eth_final),
                        .combine = "c",
                        .packages = c("tidyverse")) %dopar% 
  {fx(x1 = eth_final$c1[i], x2 = eth_final$c2[i], y = eth_final$year[i])}

parallel::stopCluster(cl = my.cluster)

eth_final$affinity <- eth_affinity

#save(eth_final, file = "Data/eth_final2.RData")



