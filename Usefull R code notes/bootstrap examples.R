#basic bootstrap with propotions 
z <- c(rep(0,50),rep(1,70))
R <- 1000
reps <- numeric(R)

#for loop to take samples 
for (i in 1:R) {
  k <- sample(z,replace = TRUE)
  reps[i] <- mean(k, na.rm=TRUE )
}
hist(reps)
sd(reps)
#find standard error of bootstrap
mean(reps)-c(2,-2)*sd(reps)
#used to find specific confidence intervals
quantile(reps,c(0.025,0.975)) # 95%
quantile(reps,c(0.05,0.95)) # 90%
quantile(reps,c(0.01,0.99)) # 98%
quantile(reps,c(0.005,0.995)) # 99%

mean(z)


##########################################################
##########################################################
##############more complex bootstraps#####################
##########################################################
#bootstrapped difference of proportions 
m<- c(rep(0, 166), rep(1, 27))     
f <- c(rep(0, 153), rep(1, 16))     
mean(f)-mean(m)

# Begin bootstrapping
R<- 8000
reps<- numeric(R)

for (i in 1:R) {
  
  k<-sample(f,replace=TRUE)
  j<- sample(m,replace=TRUE)
  reps[i] <- mean(k, na.rm = TRUE) - mean(j, na.rm = TRUE)
}

#diffrence of means bootstrap
library(tidyverse)
data("ImmuneTea", package="Lock5Data")
attach(ImmuneTea)

xbar<-ImmuneTea$InterferonGamma %>% 
  subset(Drink=='Tea') %>% 
  mean()

xbar2<-ImmuneTea$InterferonGamma %>% 
  subset(Drink=='Coffee') %>% 
  mean() 
xdiff <- xbar-xbar2
##################################################
t<-ImmuneTea$InterferonGamma %>% 
  subset(Drink=='Tea')
c<- ImmuneTea$InterferonGamma %>% 
  subset(Drink=='Coffee')
R <- 1000
reps <- numeric(R)

for (i in 1:R) {
  
  k<-sample(t,replace=TRUE)
  l <- sample(c,replace = TRUE)
  reps[i] <- mean(k, na.rm = TRUE) - mean(l, na.rm = TRUE)
}
hist(reps)
######################################################
#bootstrap using boot package#########################
######################################################
library(boot)
#######BASIC BOOT#############
#the most basic bootstrap using z data from above
dat <- boot(z,
     statistic =function(z,i){mean(z[i])},
     R=1000)

dat<- Lock5Data::NutritionStudy
library(tidyverse)
dat %>% 
  filter(VitaminUse=='Regular') %>% 
  view()
