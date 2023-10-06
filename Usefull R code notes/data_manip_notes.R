

#1:every variable gets its own column
#2:every observation gets its own row
#3:rectangular data

library(tidyverse)
table1#good
#table2'type' should be separated into two new columns named cases and population
table2 %>% 
  pivot_wider(names_from = type, values_from = count)


table3 %>% 
  separate(rate,into = c('cases','population'))
#############################
table4a %>% 
  pivot_longer(-country,names_to = 'year', values_to = 'cases')
table4b %>% 
  pivot_longer(-country,names_to = 'year', values_to = 'population')

a<-  table4a %>% 
  pivot_longer(-country,names_to = 'year', values_to = 'cases')
b <- table4b %>% 
  pivot_longer(-country,names_to = 'year', values_to = 'population')


full_join(a,b)

full_join( table4a %>% 
            pivot_longer(-country,names_to = 'year', values_to = 'cases'),
           table4b %>% 
            pivot_longer(-country,names_to = 'year', values_to = 'population')
           )
##########################
table5 %>% 
  mutate(date=paste0(century,year) %>% as.numeric()) %>% 
  select(-century,-year) %>% 
  separate(rate, into = c('cases','population'),convert = TRUE)

#9/28/23
#data was given and we were told to input it into exel chosen file name was pop_quiz_tidy.csv
#data was put in slightly wrong and needed to have a column for location and site # not just site
dat <- read.csv('./pop_quiz_tidy.csv')
dat_c<- dat %>%
  separate(site,into = c('location','site'))
#install.packages("readxl")
library(readxl)
#install.packages("janitor")
library(janitor)
library(lubridate)


dat_2 <- read_xlsx('./popquiz data.xlsx') %>% 
clean_names() 
  
names(dat_2)[1] <- 'site'
dat_2

#dont enter dates in exel
dates<- janitor::excel_numeric_to_date(as.numeric(dat_2$site[1:3]))
#pull month name abreviation to charracter 
part_1 <- lubridate::month(dates,label = TRUE,abbr = TRUE) %>% 
  str_to_upper()
#pull the site # from day of month
part_2<- lubridate::day(dates)
#paste them together seperated by -
final <- paste(part_1,part_2,sep = '-')
#put them back in dataframe
dat_2$site[1:3] <- final
#separate site into loaction and site
dat_2 <- dat_2 %>% 
separate(site,into = c('location', 'site')) %>% 
  pivot_longer(starts_with('week'),
               names_to = 'week',
               values_to = 'rel_abund',
               names_prefix = 'week_',
               names_transform = as.numeric)
dat_2
#new data
#read in data
df <- 
read_xlsx('./organized dataset.xlsx') %>% 
  clean_names()
#get rid of space in ' pool'
df$site <- 
df$site %>% 
  str_replace(' Pool','Pool')
# turn site into location and site columns
df<- df %>% 
  separate(site,into = c('location','site')) %>% 
  pivot_longer(starts_with('week'),
               names_to = 'week',
               values_to= 'rel_abund',
               names_prefix ='week_',
               names_transform=as.numeric)




#changes sewage pool and hatchery to SEP and HAT
#at this point the data is already tidy zahn just wanted it to be identical to the other dataframe
df <- df%>% 
  mutate(location=case_when(location=="SewagePool"~ 'SEP',
                                  location=="Hatchery"~'HAT'))




#old way I did assignment 6 part 1

dat_clean<- 
  dat %>% 
  mutate(Sample_Type=ifelse(grepl('^S',dat$`Sample ID`),
                            'Soil','water'))%>%  
  rename('24'=Hr_24,
         '48'=Hr_48,
         '144'=Hr_144,
         Sample_ID=`Sample ID`) %>% 
  pivot_longer(c('24','48','144'),
               names_to = 'Time_hr',
               values_to ='Absorbance') %>% 
  mutate(Time_hr=as.numeric(Time_hr))  
