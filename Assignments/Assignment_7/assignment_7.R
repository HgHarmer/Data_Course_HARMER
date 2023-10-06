library(tidyverse)
library(janitor)
library(GGally)
#1:every variable gets its own column
#observations
  #what county are you in?
  #what is the population of the county?
  #are you religious?
  #what proportion is religious or non religious?
 
      #does religious/non-religious deserve its own column, or for the purpose of this data set should non-religious
      #be treated as a religious group? If it is given its own column then it will look like a portion 
      #of all religious groups in each county are non-religious which makes no sense 
      #but if it's added to religion and proportion then it will be slightly harder to examine with ggpairs and other figures 
      #its probably better practice to treat it as a religion and 
      #remove 'religious' as an option so the proportions of each county add up to 1 

#if you're  religious what religion are you? 
#what proportion of the population belongs to each religion?
 
#2:every observation gets its own row
#3:rectangular data

dat_c <- read.csv('./Utah_Religions_by_County.csv')%>% 
  clean_names() %>% 
  pivot_longer(-c(county,pop_2010,religious),
               names_to ='religon',
               values_to= 'proportion') %>% 
  select(-religious)
#if everything worked correctly the proportion for each county should be 1
dat_c %>% 
  group_by(county) %>% 
  summarise(prop=sum(proportion)) %>% 
  view()
#all counties are within +/- 0.03 of 1 this is probably an error on the data collectors side 

names(dat_c)
#a look at general trends   
dat_c %>% ggpairs(cardinality_threshold = 29)
#no obvious patterns stick out 



#examining how population correlates to religious groups 
dat_c %>% 
  ggplot(aes(x=pop_2010,y=proportion))+
  geom_smooth(method = 'lm')+
  facet_wrap(vars(religon),scales ='free')
# a lot of these are really messy but population seems to correlate best with the 
#proportion of Muslims in the population 
dat_c %>% 
  filter(religon=="muslim") %>% 
  ggplot(aes(x=pop_2010,y=proportion))+
  geom_point()+
  labs(x='Population',y='Proportion Muslim')+
  geom_smooth()+
  ggtitle('Muslim VS. Population')

#the trend seems to be driven mostly by SLC 
dat_Mus<- dat_c %>% 
  filter(religon=="muslim")
cor(dat_Mus$proportion,dat_Mus$pop_2010)
cor(dat_Mus$proportion,dat_Mus$pop_2010)^2
#with a correlation of 0.7592709 and an r^2 value of 0.5764923, its not a particularly high correlation but
#it seems like there is a very slight positive correlation between population and the proportion of Muslims 


#now we want to explore relationships between proportions of non religious people and the proportions 
#of specific religions this is hard to do if they are in the same column 
#the easiest way I've found to compare them is to use a slightly messy data set that gives the proportion of non religious people its own column  

dat_nr <- read.csv('./Utah_Religions_by_County.csv')%>% 
  clean_names() %>% 
  pivot_longer(-c(county,pop_2010,religious,non_religious),
               names_to ='religon',
               values_to= 'proportion') %>% 
  pivot_longer(non_religious,
               names_to = 'nr',
               values_to = 'proportion_non_religious') %>% 
  select(-religious)

#this allows us to directly compare the each religion to the non religious column
#we can check the general trends to this new data set to see if it will show us any new trends in ggpairs
dat_nr %>%
  select(-c(county,religon,nr)) %>% 
  ggpairs(cardinality_threshold = 29)
# there's an interesting pattern in proportion vs proportion non-religious

#lets take a closer look 
dat_nr %>% 
  ggplot(aes(x=proportion_non_religious,y=proportion))+
  geom_smooth(method = 'lm')+
  facet_wrap(vars(religon),scales = 'free')
#based on the graphs generated LDS seems to be strongly negatively correlated with non religious, there also seems to be a decent 
#positive correlation between the episcopal church and non religious people

dat_nr %>% 
  filter(religon=="lds") %>% 
  ggplot(aes(x=proportion_non_religious,y=proportion))+
  geom_point()+
  labs(x='Proportion non-religious',y='Proportion LDS')+
  geom_smooth()+
  ggtitle('LDS VS. Non-Religious')


#this graph answers question 2, it appears that the proportion of LDS members in a county 
#negatively correlates with the proportion of non-religious people in that county 

dat_LDSvNR<- dat_nr %>% 
  filter(religon=="lds")

cor(dat_LDSvNR$proportion,dat_LDSvNR$proportion_non_religious)
cor(dat_LDSvNR$proportion,dat_LDSvNR$proportion_non_religious)^2

#there was an r^2 value of 0.7565012 and a correlation of -0.8697708 much better than Muslim vs pop

#taking a closer look at the episcopal church it seems to be mostly flat and then jump up at the end 
dat_nr %>% 
  filter(religon=="episcopal_church") %>% 
  ggplot(aes(x=proportion_non_religious,y=proportion))+
  geom_point()+
  labs(x='Proportion non-religious',y='Proportion LDS')+
  geom_smooth()+
  ggtitle('episcopal church VS. Non-Religious')

dat_episvNR<- dat_nr %>% 
  filter(religon=="episcopal_church")

cor(dat_episvNR$proportion,dat_episvNR$proportion_non_religious)
cor(dat_episvNR$proportion,dat_episvNR$proportion_non_religious)^2

#correlation is 0.5963883 and r^2 is  0.355679 so over all a pretty weak correlation even compared to Muslim vs pop























