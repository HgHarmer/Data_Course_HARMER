library(tidyverse)
library(GGally)
library(modelr)
library(easystats)
library(MASS)
easystats::easystats_update()
2

#1 load the “/Data/mushroom_growth.csv” data set

dat <- read.csv('../../Data/mushroom_growth.csv') 
dat %>% names()
# I decided to make testing and training subsets to test the models validity 
dat<- dat %>% 
  mutate(sample=rbinom(nrow(dat),1,.8))

train <- dat %>% filter(sample==1)

test <- dat %>% filter(sample==0)

#2 creates several plots exploring relationships between the response and predictors
dat %>% ggpairs()
# this graph shows a lot of interesting information, ostreotus, and cornucopiae seem to grow better with light 
#but ostreotus doesn't seem to be impacted by temperature nearly as much as cornucopiae which prefers a colder temp  
dat %>% 
  ggplot(aes(y=GrowthRate,x=Light,color=Temperature))+
  geom_point()+
  geom_smooth()+
  facet_wrap(vars(Species))

# nitrogen seems to have an ideal concentration of 20 for both species and the line seems to be parabolic
#and once again cornucopiae seems to be highly impacted by humidity 
# whereas ostreotus isn't as impacted by humidity, though it does seem to slightly  prefer it humid
dat %>% 
  ggplot(aes(y=GrowthRate,x=Nitrogen,color=Humidity))+
  geom_point()+
  geom_smooth()+
  facet_wrap(vars(Species))
#3 defines at least 4 models that explain the dependent variable “GrowthRate”
m1<- glm(data =train,
  GrowthRate~Species)

m2<- glm(data =train,
         GrowthRate~Species+Light+Humidity+Temperature+Nitrogen)

m3<- glm(data =train,
         GrowthRate~Species*Light*Humidity*Temperature*Nitrogen)

m4<- glm(data =train,
         GrowthRate~Species+Light+Humidity+Temperature)

m5 <-glm(data =train,
         GrowthRate~Species*Light*Humidity*Temperature) 
# 4 calculates the mean sq. error of each model

#model 3, the most complex model, gave the best mean sq. error
 error_sq <- test %>% 
  gather_residuals(m1,m2,m3,m4,m5) %>% 
  mutate(resid=resid^2) 

 error_sq %>% 
  group_by(model) %>% 
  summarise(mean(resid))
  
error_sq %>% 
  ggplot(aes(x=model,y=resid,fill=model))+
  geom_boxplot(alpha=.5)+
  geom_point2()

 #5 select the best model you tried
  compare_performance(m1,m2,m3,m4,m5)
  compare_performance(m1,m2,m3,m4,m5) %>% 
   plot
######m5 is a better model in terms of AIC,AICc and BIC and is only slightly worse than m3 in the other 3 categories 

  
  test_m5 <- add_predictions(test,m5) %>% 
    mutate(error=(GrowthRate-pred)^2)
  
#6 adds predictions based on new hypothetical values for the independent variables used in your model
r_data <- data.frame(Species=rbinom(n = 500,1,.5),
          Light=runif(n=500,min = 0,max = 30),
          Nitrogen=runif(n=500,min = 0,max = 65),
          Humidity=rbinom(n = 500,1,.5),
          Temperature=runif(n=500,min = 15,max = 35)) %>% 
 mutate(Species= case_when(Species==1~'P.ostreotus',
                           Species==0~'P.cornucopiae'),
        Humidity=case_when(Humidity==1~'High',
                           Humidity==0~'Low'))
  
r_pred <-add_predictions(r_data,m5)




  

 #7 plots these predictions alongside the real data
 
full <-full_join(dat,r_data)
 full_pred <- add_predictions(full,m5)  
 

 
 full_pred %>% 
     ggplot(aes(x=Light,y=GrowthRate))+
     geom_point(size=3,color='red')+
     geom_point(aes(y=pred))+
     geom_smooth(aes(y=pred))+
     facet_wrap(vars(Species))
   
 full_pred %>% 
   ggplot(aes(x=Nitrogen,y=GrowthRate))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 
 full_pred %>% 
   ggplot(aes(x=Temperature,y=GrowthRate))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 # in temp vs growth rate we see a weird x pattern lets take a closer look
 full_pred %>% 
   ggplot(aes(x=Temperature,y=GrowthRate,color=Humidity))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 names(full)
  #m5 predicts negative values for P.cornucopiae which isn't possible, 
 #also it is very bad at predicting growth based on nitrogen levels, this is probably partly because nitrogen wasn't added to this model
 # and partly because the relationship between nitrogen and growth rate seems to be parabolic and this model doesn't account for that. 
  
  
  
  
  
#how does this compare to the best model according to MASS?
 step <- stepAIC(m3)
   
 m6 <-glm(data=dat,step$formula)   
 compare_performance(m3,m4,m5,m6)
 compare_performance(m3,m4,m5,m6) %>% 
   plot  
 # m6 is seems to only be slightly better than the model i made 
 full_pred6 <- add_predictions(full,m6)
 # how do the graphs made by m6 compare to m5?
 full_pred6 %>% 
   ggplot(aes(x=Nitrogen,y=GrowthRate))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 
 full_pred6 %>% 
   ggplot(aes(x=Light,y=GrowthRate))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 
 full_pred6 %>% 
   ggplot(aes(x=Temperature,y=GrowthRate))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 
 full_pred6 %>% 
   ggplot(aes(x=Temperature,y=GrowthRate,color=Humidity))+
   geom_point(size=3,color='red')+
   geom_point(aes(y=pred))+
   geom_smooth(aes(y=pred))+
   facet_wrap(vars(Species))
 #they look pretty similar to the ones made by m5, the ranges seem to be slightly smaller 
 #this model is also bad at predicting the relationship between nitrogen and growth rate.
 
  