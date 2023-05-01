library(tidyverse)
library(readxl)
library(haven)
library(janitor)
library(foreach)

rm(list = ls())

### DATA LOADING
religion <- read_csv("Data/CREG.Rel.1.2.csv")

### 
rel_new <- religion %>% 
  janitor::clean_names() %>% 
  mutate(estimate = if_else(is.na(group_proportion), 1, 0),
         group_proportion = if_else(estimate == 1, group_estimate, group_proportion)) %>% 
  filter(independent_country == 1, 
         !(group_name %in% c("other", "nonreligious", "traditionalbelief"))) %>% 
  group_by(cowcode, year, group_name) %>% 
  slice(1) %>% 
  ungroup()

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

#save(rel_final, file = "Prod/rel_final.RData")





