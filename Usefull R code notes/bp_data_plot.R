library(tidyverse)
library(GGally)
#read in the RDS file 
dat <- readRDS('./cleaned_bp.rds')
# start manipulating data 
names(dat)
#run ggpairs to look for trends 
dat %>% 
  select(-c(pat_id,month_of_birth,day_birth,year_birth)) %>% 
  ggpairs(,cardinality_threshold = 33)
# start plotting data based on the trends we see 
dat %>% 
  ggplot(aes(x=visit))+
  geom_point(aes(y=systolic),color='red')+
  geom_point(aes(y=diastolic),color='black')+
  stat_summary(aes(y=diastolic))+
  stat_summary(aes(y=diastolic))+
  facet_wrap(~hispanic)

  
  dat %>% 
    ggplot(aes(x=visit))+
    geom_point(aes(y=systolic),color='red')+
    geom_point(aes(y=diastolic),color='black')+
  facet_wrap(~hispanic)
  
  
 # other ways to manipulate the data  
  dat %>% 
    filter(race=='Black'|sex=='Female')

  
  dat %>% group_by(race) %>% 
    summarise(mean_systolic= mean(systolic))