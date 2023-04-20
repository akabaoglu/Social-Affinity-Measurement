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
polity_v <- read_xls("Data/p5v2018.xls")
religion <- read_csv("Data/CREG.Rel.1.2.csv")
ethnicity <- read_csv("Data/CREG.Eth.1.2.csv")
mid_targets <- read.csv("Data/dyadic_mid_4.02.csv")
cow_trade.dt <- read_csv("Data/cow_trade_dyad.csv") %>% 
  select(1:7) %>%
  filter(year >= 1945 & year <= 2012)
mid_targets <- mid_targets %>% 
  select(statea, namea, stateb, nameb, strtyr, orignatb, hihostb) %>% 
  filter(orignatb == 1 & hihostb > 3 ) %>% # statea: target , stateb: initiator; highest level of hostility should at least be the use of force(>=4)
  select(c(1,2,3,4,5,6)) %>% 
  unique() %>% 
  mutate(mid_target = 1)

alliance.dt <- read_csv("Data/alliance_v4.1_by_directed_yearly.csv")

cap_dist.dt <- read.csv("Data/capdist.csv")

nmc <- read_csv("Data/NMC-60-abridged.csv") %>% select(c(2,3,10))

### 
eth_new <- ethnicity %>% 
  janitor::clean_names() %>% 
  filter(!(group_name %in% c("other", "mixed"))) %>% 
  mutate(group_proportion = if_else(is.na(group_proportion), group_estimate, group_proportion)) %>% 
  drop_na()

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

eth_trial <- expand_grid(c1 = unique(eth_new$cowcode), c2 = unique(eth_new$cowcode), year = 1945:2013) %>% 
  filter(c1 != c2) %>% 
  sample_n(1, replace = F) %>% 
  mutate(eth_aff = pmap_dbl(list(c1, c2, year), fx))

eth_trial

eth_new %>% 
  filter(cowcode == eth_trial$c1 & year == eth_trial$year)

eth_new %>% 
  filter(cowcode == eth_trial$c2 & year == eth_trial$year)

eth_available <- eth_new %>% 
  select(cowcode, year) %>% 
  group_by(cowcode) %>% 
  summarize(min = min(year), max = max(year))


eth_final <- expand_grid(c1 = unique(eth_new$cowcode), c2 = unique(eth_new$cowcode), year = 1945:2013) %>% 
  filter(c1 != c2) %>% 
  mutate(eth_aff = 0,
         avail = 0)

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

t1 <- Sys.time()
availability <- foreach(i = 1:nrow(eth_final),
                        .combine = "c",
                        .packages = c("tidyverse")) %dopar%{
                          if (eth_final$year[i] %in% c(eth_available$min[eth_available$cowcode == eth_final$c1[i]]:eth_available$max[eth_available$cowcode == eth_final$c1[i]]) & 
                              eth_final$year[i] %in% c(eth_available$min[eth_available$cowcode == eth_final$c2[i]]:eth_available$max[eth_available$cowcode == eth_final$c2[i]])) {
                            return(1)
                          } else return(0)}
t2 <- Sys.time()

parallel::stopCluster(cl = my.cluster)

eth_final$avail <- availability

eth_final <- eth_final %>% filter(avail == 1)

mean(availability)

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

eth_final$eth_aff <- eth_affinity

save(eth_final, file = "eth_final.RData")


