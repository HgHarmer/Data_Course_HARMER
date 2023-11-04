library(tidyverse)
library(modelr)

non_lin<- read.csv('../../Data/non_linear_relationship.csv')


non_lin %>% 
  ggplot(aes(x=predictor,y=response))+
  geom_point()

para1<- glm(data = non_lin,
           response~poly(predictor,3))

#This model predicts too high of values for numbers below 1

para2<- glm(data = non_lin,
    response~I(predictor^3))

#this predicts too low for numbers below 1 
preds1<- add_predictions(data = non_lin,model = para1) 
preds2<- add_predictions(data = non_lin,model = para2)  

preds1 %>% 
ggplot(aes(x=predictor,y=response))+
  geom_point()+
  geom_line(aes(y=pred,color='red'))

preds2 %>% 
  ggplot(aes(x=predictor,y=response))+
  geom_point()+
  geom_line(aes(y=pred,color='red'))


#googlesheets4:: lets you enter password and username for Google sheets and pull data automatically 
googlesheets4::