
library(skimr)
library(tidyverse)
library(gganimate)
library(janitor)
read_csv("../../Data/BioLog_Plate_Data.csv") 

#problems with data:
#1 column 'sample ID' includes a space change to 'sample_ID'
#2 time and absorption are variables and should have their own columns, likewise each observation time is an observation so should be in a row.
#3 hours are recorded as hr_ this is a character vector and will need to made numeric
#4 add new column that records if a sample was taken from water or soil
#5 the 'well' column should also be split into well_row and well_column as these can be seen as separate observations

 
dat_clean<- read_csv("../../Data/BioLog_Plate_Data.csv") %>% 
  clean_names() %>%
  mutate(Sample_Type=ifelse(grepl('^S',sample_id),
                            'Soil','water'))%>% 
  pivot_longer(starts_with('hr_'),
               names_to = 'Time_hr',
               values_to = 'Absorbance',
               names_prefix = 'hr_',
               names_transform = as.numeric) %>% 
  separate(well, into = c('remove','well_row', 'well_column'),sep ="") %>% 
  select(-remove)

#use clean data to create faceted plot
#plot needs:
#1 contains only dilutions of 0.1
#2 has a facet for each substrate
#2 2 lines representing sample type 
#3 title and axis labels 

 dat_clean %>%
   filter(dilution==0.1) %>% 
   
   ggplot(aes(x=Time_hr,y=Absorbance,
              color=Sample_Type))+
   facet_wrap(vars(substrate))+
   geom_smooth(se = FALSE)+
   theme_minimal()+
   ggtitle('Just dilution 0.1')
  
 #use clean data to make a plot that shows  
 #1 only data from Itaconic acid
 #2 data points are mean absorbency, by Sample ID, by dilution, by Hour, plotted against hours
 #3 faceted by dilution
 #4 separate line for each sample ID
 #5 add reveal animation
  dat_clean %>% 
  filter(substrate=='Itaconic Acid') %>% 
  group_by(sample_id,dilution,Time_hr) %>%  
  summarise(average_abs=mean(Absorbance)) %>%   
 
  ggplot(aes(Time_hr,average_abs,color=sample_id))+
    facet_wrap(vars(dilution))+
    geom_line()+
    theme_minimal()+
    transition_reveal(Time_hr)
####################################################################################  
 
#shown in class
 df <-   read_csv("../../Data/BioLog_Plate_Data.csv") %>% 
    clean_names() %>%
    pivot_longer(starts_with('hr_'),
                 names_to = 'hour',
                 values_to = 'abs',
                 names_prefix = 'hr_',
                 names_transform = as.numeric) %>% 
    mutate(type=case_when(sample_id=='soil_1'~'soil',
                     sample_id=='soil_2'~'soil',
                     sample_id=='Clear_Creek'~'water',
                     sample_id=='Waste_Water'~'water'))
  
  df %>% 
    filter(dilution==.1) %>% 
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  