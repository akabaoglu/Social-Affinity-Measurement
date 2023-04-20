library(tidyverse)
library(haven)
library(foreach)

demog_cor <- read_csv("Data/Etnisk.Cor.csv")

demog <- read_sav("Data/etnisk.sav") %>% 
  select(1:3, ends_with("B")) %>% # Britannica Book of the Year
  left_join(demog_cor %>% select(c(1,3)) %>% drop_na(), by = c("V02B" = "MAJORITY CODE" )) %>% 
  rename("L_Maj" = ID) %>% 
  left_join(demog_cor %>% select(c(1,4)) %>% drop_na(), by = c("V04B" = "MINORITY CODE" )) %>% 
  rename("L_Min" = ID) %>% 
  mutate(R_Maj = as.numeric(V07B),
         R_Min = if_else(V09B != 8, as.numeric(V09B), 9)) %>% 
  select(c(1:7)) %>% 
  mutate(across(c(V02B, V04B), as.double)) %>% 
  pivot_longer(c(V02B, V04B), values_to = "Language", names_to = "Status") %>% 
  mutate(Ratio = if_else(Status == "V02B", V01B, V03B)) %>% 
  select(-c(4:6)) %>% 
  group_by(SSNO, YEAR, Language) %>% 
  mutate(n = n()) %>% 
  slice(1) %>% 
  select(-n) %>% 
  ungroup()

####


demog_new <- demog %>% 
  drop_na()

fx <- function(x1, x2, y){
  
  c1 <- demog_new %>% 
    filter(SSNO == x1 & YEAR == y) %>% 
    select(SSNO) %>%
    unique() %>% 
    pull()
  
  c2 <- demog_new %>% 
    filter(SSNO == x2 & YEAR == y) %>% 
    select(SSNO) %>% 
    unique() %>% 
    pull()
  
  if(length(c1) != 0 & length(c2) != 0){
    c1_ids <- demog_new %>% 
      filter(SSNO == x1 & YEAR == y) %>% 
      select(Language) %>% 
      pull()
    
    c2_ids <- demog_new %>% 
      filter(SSNO == x2 & YEAR == y) %>% 
      select(Language) %>% 
      pull()
    
    c_int <- intersect(c1_ids, c2_ids)
    
    
    if (length(c_int > 0)){
      dat <- rep(0, length(c_int))
      
      for (i in 1:length(c_int)){
        p1 <- demog_new %>% filter(SSNO == x1 & YEAR == y & Language == c_int[i]) %>% select(Ratio) %>% pull()
        p2 <- demog_new %>% filter(SSNO == x2 & YEAR == y & Language == c_int[i]) %>% select(Ratio) %>% pull()
        
        dat[i] <- sqrt((p1/100)*(p2/100))
      }
      
      return(sum(dat))
    }
    
    else return(0)
  }
  
  else return(NA)
}

dem_trial <- expand_grid(c1 = unique(demog_new$SSNO), c2 = unique(demog_new$SSNO), year = 1945:1990) %>% 
  filter(c1 != c2) %>% 
  sample_n(1, replace = F) %>% 
  mutate(dem_aff = pmap_dbl(list(c1, c2, year), fx)) 

dem_trial

demog_new %>% 
  filter(SSNO == dem_trial$c1 & YEAR == dem_trial$year)

demog_new %>% 
  filter(SSNO == dem_trial$c2 & YEAR == dem_trial$year)



demog_available <- demog_new %>% 
  select(SSNO, YEAR) %>% 
  group_by(SSNO) %>% 
  summarize(min = min(YEAR), max = max(YEAR))


demog_final <- expand_grid(c1 = unique(demog_new$SSNO), c2 = unique(demog_new$SSNO), year = 1945:1994) %>% 
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
availability <- foreach(i = 1:nrow(demog_final),
                        .combine = "c",
                        .packages = c("tidyverse")) %dopar%{
                          if (demog_final$year[i] %in% c(demog_available$min[demog_available$SSNO == demog_final$c1[i]]:demog_available$max[demog_available$SSNO == demog_final$c1[i]]) & 
                              demog_final$year[i] %in% c(demog_available$min[demog_available$SSNO == demog_final$c2[i]]:demog_available$max[demog_available$SSNO == demog_final$c2[i]])) {
                            return(1)
                          } else return(0)}
t2 <- Sys.time()

parallel::stopCluster(cl = my.cluster)



demog_final$avail <- availability

demog_final <- demog_final %>% filter(avail == 1)


n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

demog_affinity <- foreach(i = 1:nrow(demog_final),
                        .combine = "c",
                        .packages = c("tidyverse")) %dopar% 
  {fx(x1 = demog_final$c1[i], x2 = demog_final$c2[i], y = demog_final$year[i])}

parallel::stopCluster(cl = my.cluster)



demog_final$demog_aff <- demog_affinity

demog_final <- demog_final %>% drop_na()

save(demog_final, file = "demog_final.RData")
