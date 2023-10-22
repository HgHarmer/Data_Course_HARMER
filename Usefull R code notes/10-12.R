

library(tidyverse)
library(palmerpenguins)
library(modelr)
library(easystats)
library(modelr)
library(MASS)
easystats::easystats_update()
2
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

m7<- glm(data = dat,
         formula = admit~gre*gpa*rank,
         family = 'binomial')

#finds best model for you
step<- stepAIC(m6)

step$formula

modbest <- glm(data = df,family = 'binomial',formula = step$formula)


penguins %>% 
  names()

c<- glm(data=penguins,
            formula = bill_length_mm~island*species* 
          bill_depth_mm*flipper_length_mm*body_mass_g*
          sex *year)
#an easier way to do it
better<- glm(data=penguins,
    formula = bill_length_mm~ .^2)


best <- stepAIC(better)

best$formula

best_mod <- glm(data = penguins,formula = best$formula)



#models should be trained on some data and then tested on another set 




penguins<- penguins %>% 
  mutate(sample=rbinom(nrow(penguins),1,.8))

train <- penguins %>% filter(sample==1)

test <- penguins %>% filter(sample==0)

mod_best <- glm(data = train,
                formula = best$formula)

predictions<- 
add_predictions(test,mod_best)

#calculate absolute difference 
predictions <- 
predictions %>% 
  mutate(resid=abs(pred-bill_length_mm))


mean(predictions$resid,na.rm = TRUE)

errors <-c()
for (i in 1:1000) {
predictions %>% 
  mutate(resid=abs(pred-bill_length_mm))
mean_err <- mean(predictions$resid,na.rm = TRUE)
errors[i] <-mean_err
}

dat <- data.frame(errors)

 dat %>% 
 ggplot(aes(x=errors))+
  geom_density()

install.packages('ranger')
library(ranger)

r_mod <- ranger(Species~.,data = iris)

pred <- predict(r_mod,iris)

data.frame(iris$Species,pred$predictions)
