library(tidyverse)
library(janitor)
library(skimr)

#make new data frames separated by what part of the bird is being measured 

mass <- read.csv('../Data/Bird_Measurements.csv') %>% 
  clean_names() %>% 
  select(-ends_with('_n')) %>% 
  select(-ends_with(c('_wing','_tail','_tarsus','_bill')))


tarsus<- read.csv('../Data/Bird_Measurements.csv') %>% 
  clean_names() %>% 
  select(-ends_with('_n')) %>% 
  select(-ends_with(c('_wing','_tail','_bill',
                      'm_mass','f_mass','unsexed_mass')))

wing<- read.csv('../Data/Bird_Measurements.csv') %>% 
  clean_names() %>% 
  select(-ends_with('_n')) %>% 
  select(-ends_with(c('_tarsus','_tail','_bill',
                      'm_mass','f_mass','unsexed_mass')))

bill<- read.csv('../Data/Bird_Measurements.csv') %>% 
  clean_names() %>% 
  select(-ends_with('_n')) %>% 
  select(-ends_with(c('_tarsus','_tail','_wing',
                      'm_mass','f_mass','unsexed_mass')))

tail<- read.csv('../Data/Bird_Measurements.csv') %>% 
  clean_names() %>% 
  select(-ends_with('_n')) %>% 
  select(-ends_with(c('_tarsus','_wing','_bill',
                      'm_mass','f_mass','unsexed_mass')))

mass <- 
mass %>% 
  pivot_longer(c(m_mass,f_mass,unsexed_mass),
               names_to = 'sex',
               values_to = 'mass') %>% 
  mutate(sex=sex %>% str_remove('_mass'))
wing <- 
wing %>% 
  pivot_longer(c(m_wing,f_wing,unsexed_wing),
                      names_to = 'sex',
                      values_to = 'wing') %>% 
  mutate(sex=sex %>% str_remove('_wing'))
tail <- 
tail %>% 
  pivot_longer(c(m_tail,f_tail,unsexed_tail),
               names_to = 'sex',
               values_to = 'tail') %>% 
  mutate(sex=sex %>% str_remove('_tail'))
bill <- 
bill %>% 
  pivot_longer(c(m_bill,f_bill,unsexed_bill),
               names_to = 'sex',
               values_to = 'bill') %>% 
  mutate(sex=sex %>% str_remove('_bill'))
tarsus <- 
tarsus %>% 
  pivot_longer(c(m_tarsus,f_tarsus,unsexed_tarsus),
               names_to = 'sex',
               values_to = 'tarsus') %>% 
  mutate(sex=sex %>% str_remove('_tarsus'))


full<- bill %>% 
  full_join(mass) %>% 
  full_join(tarsus) %>% 
  full_join(tail) %>%
  full_join(wing)
 return(full)

 }

saveRDS(full,'./clean_bird')

clean_bird_function()





names(df)
skim(df)







