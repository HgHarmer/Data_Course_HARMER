#####

library(tidyverse)
#I Read the cleaned_covid_data.csv file into an R data frame####

dat <- read.csv("./cleaned_covid_data.csv")
attach(dat)
dat
#IISubset the data set to just show states that begin with “A”#### 
#and save this as an object called A_states####

A_States <- dat[grepl('^A',dat$Province_State),]

#IIICreate a plot of that subset showing Deaths over time, ####
#with a separate facet for each state. ####

ggplot(A_States, aes(x=as_date(Last_Update),
       Deaths,
       color=Province_State))+
  geom_point(size=.5)+
  facet_wrap(~Province_State,
             scales = 'free' )+
  geom_smooth(se=FALSE,
              color= 'black')+
  xlab('Date')+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.1,
                                   hjust=1,
                                   size = 6))

#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio ####
#for each state and save this as a new data frame object 
#called state_max_fatality_rate. #####
dat_2 <- unique(dat$Province_State)

a<-length(dat_2)

SMFR<- c()
# this forloop identifies all max case fatality ratios 
for (i in 1:a) {
  temp_1<-subset(dat, Province_State==dat_2[i])
  temp_2 <- subset(temp_1,
                   Case_Fatality_Ratio==max(temp_1$Case_Fatality_Ratio,
                   na.rm = TRUE))                         
SMFR<-rbind(SMFR, temp_2)
}

#previous for loop left 5 points in Alaska and 3 in New mexico that have identical max fatality ratios
#this for loop removes those leaving only one for each state

SMFR2 <- c()

for (i in 1:a) {
  temp_1<-subset(SMFR, Province_State==dat_2[i])
  if (dim(temp_1)[1] > 1) {
    temp_2<-temp_1[dim(temp_1)[1],]
  }
  else {
    temp_2<-temp_1
  }
  SMFR2<-rbind(SMFR2, temp_2)
}
#remove all extra columns and rename Case_Fatality_Ratio
SMFR2$Last_Update<- NULL
SMFR2$Confirmed<- NULL
SMFR2$Deaths<- NULL
SMFR2$Recovered<- NULL
SMFR2$Active <- NULL
SMFR2$Maximum_Fatality_Ratio <- SMFR2$Case_Fatality_Ratio
SMFR2$Case_Fatality_Ratio <- NULL

#order the data set from highest fatality ratio to lowest and save as state_max_fatality_rate

state_max_fatality_rate <- SMFR2[order(SMFR2$Maximum_Fatality_Ratio, decreasing = TRUE),]
#V. Use that new data frame from task IV to create another plot.####


ggplot(data = state_max_fatality_rate,
       aes(x=reorder(state_max_fatality_rate$Province_State,-state_max_fatality_rate$Maximum_Fatality_Ratio),
       y=state_max_fatality_rate$Maximum_Fatality_Ratio,
       las=2))+
  geom_bar(stat='identity')+
  xlab('State')+
  ylab('Max Fatality Ratio')+
theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.1,
                                   hjust=1,
                                   size = 6))



#VI. (BONUS 10 pts) Using the FULL data set,####
#plot cumulative deaths for the entire US over time
#You’ll need to read ahead a bit and use the dplyr package functions 
#group_by()and summarize() to accomplish this.####

dat_2 <- dat %>%
  group_by(Last_Update) %>%
  summarise(
    cumulitive_deaths = sum(Deaths, na.rm = TRUE))
 

ggplot(data = dat_2,
       aes(x=as.Date(Last_Update),
           y=cumulitive_deaths))+
  geom_point(size=.1)+
  xlab('Date')+
  ylab('Cumulative Deaths')+
theme(axis.text.x = element_text(angle = 90,
                                 vjust = 0.1,
                                 hjust=1,
                                 size = 6))+
  ggtitle('Cumulative Deaths from Covid-19')
 




   





