library(tidyverse)
library(easystats)#this package includes the 'insight' package which also has a 'clean_names' function 
library(modelr)
library(janitor)
easystats::easystats_update()
2
#1. Read in the unicef data
#2. Get it into tidy format
U5MR <- read.csv('./unicef-u5mr.csv') %>% 
  janitor::clean_names() %>% 
  pivot_longer(-c(country_name,continent,region),
               names_to = 'year',
               values_to = 'u5mr') %>% 
  mutate(year=year %>% str_remove('u5mr_')%>% as.numeric())

#3. Plot each country’s U5MR over time 
U5MR %>% 
  ggplot(aes(x=year,y=u5mr,color=country_name))+
  facet_wrap(vars(continent))+
  geom_line()+
  theme(legend.position = 'none')
#4.Save this plot as LASTNAME_Plot_1.png (5 pts)

ggsave('HARMER_Plot_1.png')

#5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
U5MR %>% 
  group_by(continent,year) %>% 
  summarise(mean_u5mr=mean(u5mr,na.rm = TRUE)) %>% 
  ggplot(aes(year,mean_u5mr,color= continent))+
  geom_line(size=2)+
  theme_minimal()
#6. Save that plot as LASTNAME_Plot_2.png

ggsave('HARMER_Plot_2.png')

#7. Create three models of U5MR

m1 <- glm(data = U5MR,
    formula =u5mr~year)

m2 <- glm(data = U5MR,
    formula =u5mr~year+continent)

m3 <- glm(data = U5MR,
    formula =u5mr~year*continent)

preds <- add_predictions(U5MR,m1) %>% 
  rename(m1=pred) %>% 
  add_predictions(m2) %>%
  rename(m2=pred) %>% 
  add_predictions(m3) %>% 
  rename(m3=pred) %>% 
  pivot_longer(c(m1,m2,m3),
               names_to = 'model',
               values_to = 'prediction') 
#8. Compare the three models with respect to their performance

compare_performance(m1,m2,m3)
compare_performance(m1,m2,m3) %>% 
  plot
#model 3 is by far the best model 
#9 Plot the 3 models’ predictions
 preds %>% 
  ggplot(aes(x=year,y=prediction,color=continent))+
  geom_line(size=1.5)+
  facet_wrap(vars(model))


#10 Using your preferred model,predict what the U5MR would be for Ecuador in the year 2020. 
#The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. 
#How far off was your model prediction???

#10 a. prediction given for Americas by model 3
 predict.glm(object = m3,data.frame(year=2020,continent='Americas'))
#the model prediction given was -10.58018 
#this value is 23.58018 away from the real value
#10 b. a look at Ecuador's U5MR 
 U5MR %>% 
   filter(country_name=='Ecuador') %>% 
   ggplot(aes(year,u5mr))+
   geom_line()
#it looks like it's mostly linear from 1950-1990 but the trend slows after that 
U5MR %>% 
  filter(region=='South America') %>% 
   ggplot(aes(x=year,y=u5mr,color=country_name))+
   geom_line()+
   theme(legend.position = 'none')
# this trend seems to more or less match the rest of south america, 
# a better model may only take into account south america from 2000 onward. 

u5mr2000<- U5MR %>% 
  filter(year>=2000)

 m4 <- glm(data = u5mr2000,
           formula =u5mr~year*region)
 summary(m4)
 predict.glm(object = m4,data.frame(year=2020,region='South America'))
 
 
 # this gives a prediction of 14.38854, just 1.38854 off from reality
 
 compare_performance(m1,m2,m3,m4)
 compare_performance(m1,m2,m3,m4) %>% 
   plot
# model 4 seems to be a much better for model predicting values after the year 2000 

 
# another option is a logarithmic model 
 m5 <- glm(data = U5MR,
     formula =u5mr~log(year)*region)
 
 10^predict(m5,data.frame(year=2020, region='South America'))
 # this gives a value of basically 0, still better than m3 but not nearly as precise as m4
 
 compare_performance(m1,m2,m3,m5) %>% 
   plot
 # the comparison shows that model 5 is much better than 1,2,and 3, but m4 is still generally better at predicting u5mr after 2000 
 # m5 does seem to have a slightly better r^2 value than m4
 compare_performance(m1,m2,m3,m4,m5) %>% 
   plot
 
 
 
 
 
 
 
