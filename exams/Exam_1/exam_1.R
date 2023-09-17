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
                                   size = 6))+
  ggtitle('Deaths in States That Start With A')

#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio ####
#for each state and save this as a new data frame object 
#called state_max_fatality_rate. #####

#isolate Provence_sate and case_fatality_ratio into new data frame
SMFR <- data.frame(Province_State,Case_Fatality_Ratio)
#creates a list of unique state names that will be used in the forloop
state_names <- unique(SMFR$Province_State)
#counts how many unique state names there are (this is important for the forloop)
a<-length(state_names)
#make a new object that we can fill in the for loop
SMFR_MAX <- c()
#this finds the max fatality rates for each state and puts it into the SMFR_MAX object
for (i in 1:a) {
  temp_1<-subset(SMFR, Province_State==state_names[i])
  temp_2 <- subset(temp_1,
                   Case_Fatality_Ratio==max(temp_1$Case_Fatality_Ratio,
                                            na.rm = TRUE))                         
  SMFR_MAX<-rbind(SMFR_MAX, temp_2)
}

#creates a new empty object called DISORDERED_state_max_fatality_rate 

DISORDERED_state_max_fatality_rate<- c()
#and then fills that object with only the unique entries SMFR_MAX
for (i in 1:a) {
  temp_1<-subset(SMFR_MAX, Province_State==state_names[i])
  if (dim(temp_1)[1] > 1) {
    temp_2<-temp_1[dim(temp_1)[1],]
  }
  else {
    temp_2<-temp_1
  }
  DISORDERED_state_max_fatality_rate<-rbind(DISORDERED_state_max_fatality_rate
                                            , temp_2)
}

#move the data to a new data set named state_max_fatality_rate but ordered by case_Fatality_Ratio in decreasing order
#this data set is ordered like we want the final data set to be but the row names are still from the original data frame
state_max_fatality_rate<- DISORDERED_state_max_fatality_rate[order(DISORDERED_state_max_fatality_rate$Case_Fatality_Ratio, 
                                                            decreasing = TRUE),]
#this step makes the row names make sense

rownames(state_max_fatality_rate) <- c(1:a)

#finally rename the Case_Fatality_Ratio column to Maximum_Fatality_Ratio

names(state_max_fatality_rate)[2] <- paste('Maximum_Fatality_Ratio')



#V. Use that new data frame from task IV to create another plot.####


ggplot(data = state_max_fatality_rate,
       aes(x=reorder(state_max_fatality_rate$Province_State,-state_max_fatality_rate$Maximum_Fatality_Ratio),
       y=state_max_fatality_rate$Maximum_Fatality_Ratio,
       las=2))+
  geom_bar(stat='identity')+
  xlab('State or Province')+
  ylab('Max Fatality Ratio')+
theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.1,
                                   hjust=1,
                                   size = 6))+
  ggtitle('Max Fatality Ratio By State')



#VI. (BONUS 10 pts) Using the FULL data set,plot cumulative deaths for the entire US over time####
#You’ll need to read ahead a bit and use the dplyr package functions 
#group_by()and summarize() to accomplish this.####

dat_CD <- dat %>%
  group_by(Last_Update) %>%
  summarise(
    cumulitive_deaths = sum(Deaths, na.rm = TRUE))
 

ggplot(data = dat_CD,
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
 
