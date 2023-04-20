library(tidyverse)
library(readxl)
library(haven)
library(janitor)
library(foreach)

rm(list = ls())

### DATA LOADING

ethnicity <- read_csv("Data/CREG.Eth.1.2.csv")


### 
eth_new <- ethnicity %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(group_name != "other",
         independent_country == 1) 

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



