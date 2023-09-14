library(tidyverse)



N <- c(1:50)

DNA_Samples <- paste0('DNA_',N)
#DNA will be separated based on E-value 
E_value <- runif(50, min = 0, max = 1)
#in this case an E-value < .5 would be considered DNA
#of unknown origin however in a real data set the cutoff evalue 
# would probably be closer to around 1e-20~30
acceptable <- E_value<.5

df <- cbind(DNA_Samples,E_value, acceptable)

df_2 <- data.frame(df)


ggplot(df_2, aes(x="", fill=factor(acceptable))) +
         geom_bar(width = 1) +
         coord_polar("y", start=0)
