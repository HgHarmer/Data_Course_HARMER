
easystats::easystats_update()
library(tidyverse)
library(palmerpenguins)
library(modelr)
library(easystats)
library(modelr)
penguins %>% 
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm))+
  geom_point()+
  geom_smooth(method = 'lm')
penguins <- penguins

glm(data= penguins,bill_depth_mm~bill_length_mm) 

# anallasis of vairance table tells you if how good the pvalue is GLM is genarally more usefull

m1<- glm(data= penguins,bill_depth_mm~bill_length_mm)

#picks the first one alphabeticaly as a baseline 

m2 <- glm(data=penguins,
         bill_depth_mm~bill_length_mm+species)

# also tells you if slope depends on species 

 m3<- glm(data=penguins,
    bill_depth_mm~bill_length_mm*species)


preds <-add_predictions(penguins,m2) 
# tells you how good the model is lower is better 
m1$aic
m2$aic
m3$aic
# a much better way to do it 
compare_performance(m1,m2,m3,m4)

compare_performance(m1,m2,m3,m4) %>% 
  plot

m4<- glm(data = penguins,
    bill_depth_mm~bill_length_mm*species+sex+island)
formula(m4)
predict(m4,newdata = x)
x <- data.frame(bill_length_mm=5000 ,species='Chinstrap' ,sex='male' ,island='Dream')

mpg <- mpg

mpg %>% ggplot(aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y~log(x))

y <- data.frame(displ=500)
m5 <- glm(data = mpg, formula = hwy~log(displ))

10^predict(m5,y)

Titanic %>%  as.data.frame() %>% 
  view
dat <- read.csv('../Data/GradSchool_Admissions.csv')
dat <- dat %>% 
  mutate(admit=as.logical(admit))
  
  
 #logistic regression, outcome is true false 
# logistic family=binomial
m6<- glm(data = dat,
    formula = admit~gre+gpa+rank,
    family = 'binomial')

add_predictions(dat,m6,type = 'response') %>% 
  ggplot(aes(x=gpa,y=pred,color=factor(rank)))+
  geom_smooth()





















