library(tidyverse)
mpg <- mpg
mpg %>% names()

mpg %>% 
  ggplot(aes(x=displ,y=hwy,color=factor(cyl)))+
  geom_smooth(method = 'lm')
mpg$hwy %>% mean()


m <- glm(data = mpg,
    formula =hwy ~ displ)

summary(m)



m <- glm(data = mpg,
    formula =hwy ~ displ+factor(cyl))


summary(m)

m$coefficients
library(modelr)

preds<- add_predictions(mpg,m)


