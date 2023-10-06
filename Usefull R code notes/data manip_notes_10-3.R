library(tidyverse)
library(janitor)
library(readxl)
path <- '../data/messy_bp.xlsx'
# this extracts the visit names
visits <- read_xlsx(path,skip=2,n_max = 0) %>% 
names()
# the original data is not in the corner this selects only the data
df <- read_xlsx(path,range = 'A4:M24') %>% 
  clean_names()

################################################################################
# this was the first attempt to clean the data it did not work
df %>%
  pivot_longer(starts_with('bp_'),
               values_to = 'bp') %>% 
  mutate(visit= case_when(name=='bp_8'~1,
                          name=='bp_10'~2,
                          name=='bp_12'~3)) %>% 
  pivot_longer(starts_with('hr_'),
               names_to = 'visit_2',
               values_to='heart_rate')
 
############working attempt####################################################################
# the purpose of this code is to separate the data into two data frames one for blood pressure 
#and one for heart rate we have to do this because if we rotate them one at a time in the same data frame then 
#the appointment numbers will be wrong
#1 remove all columns that start with hr_
#2 pivot longer so the the observations of what visit it was gets their own row
#3 mutate to create a new column where the bp_# becomes the visit number 
#4 get rid of the name column
#5 separate bp into two new columns so that we can make it numeric 
 bp<- df %>%
     select(-starts_with('hr_')) %>% 
     pivot_longer(starts_with('bp_'),
                  values_to = 'bp') %>% 
     mutate(visit= case_when(name=='bp_8'~1,
                             name=='bp_10'~2,
                             name=='bp_12'~3)) %>% 
     select(-name) %>% 
     separate(bp,into = c('systolic','diastolic'),convert = TRUE)
     
 # this one is near identical to the bp cleaning but we use heart rate and we dont need to seperate it  
 hr<- df %>%
     select(-starts_with('bp_')) %>% 
     pivot_longer(starts_with('hr_'),
                  values_to = 'hr') %>% 
     mutate(visit= case_when(name=='hr_9'~1,
                             name=='hr_11'~2,
                             name=='hr_13'~3)) %>% 
     select(-name) 
  #join the two data sets togeter    
     dat <- full_join(bp,hr)
   # add a new column that denoted birth-date in the POSIXct format (year month day)
   dat <- 
     dat %>% 
     mutate(birthdate=paste(year_birth,month_of_birth,day_birth,sep = '-')%>% 
     as.POSIXct()) 
   #white was written 3 different ways this fixes that 
   dat <- dat %>% 
     mutate(race=case_when(race=="WHITE"~"White",
                           race=="Caucasian"~"White",
                           TRUE~race))
   
   
################################################################################3
# save the dat object as an r data set
    saveRDS(dat,'cleaned_bp.rds')    
  
  