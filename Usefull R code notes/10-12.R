library(tidyverse)
library(palmerpenguins)
library(modelr)
library(easystats)
penguins %>% 
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm))+
  geom_point()+
  geom_smooth(method = 'lm')
penguins <- penguins

glm(data= penguins,bill_depth_mm~bill_length_mm) 

# anallasis of vairance table tells you if how good the pvalue is GLM is genarally more usefull

m1<- glm(data= penguins,bill_depth_mm~bill_length_mm)

#picks the first one alphabeticaly as a baseline 

m2 <- glm(data= penguins,
          fomula=bill_depth_mm~bill_length_mm+species)

# also tells you if slope depends on species 

 m3<- glm(data=penguins,
    bill_depth_mm~bill_length_mm*species)


preds <-add_predictions(penguins,m2) 
# tells you how good the model is lower is better 
m1$aic
m2$aic
m3$aic
compare_models(m1,m2,m3)