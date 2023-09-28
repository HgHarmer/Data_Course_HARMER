library(tidyverse)
library(ggplot2)
n <- 120
p <- .30
data <- replicate(10000,rbinom(n,1,p))
means <- colMeans(data)
head(means)

hist(means, xlim = c(0.20,1.0))
phat1 <- 36/120
abline(v=phat1,col="blue",lwd=2)
phat2 <- 37/120
abline(v=phat2,col="yellow",lwd=2) 
phat3 <- 90/120
abline(v=phat3,col="red",lwd=2)
phat4 <- 27/120
abline(v=phat4,col="green",lwd=2)
quantile(means,c(0.025,0.975))
