library(tidyverse)
library(janitor)
library(googlesheets4)
library(purrr)
library(rrapply)
library(lubridate)
library(plotly)
library(rsconnect)
rsconnect::deployApp('./NHapp')
#############Functions###########################################################
p<-gs4_find(order_by='createdTime desc')

read_NH <- function(gsheet=1){

 p<-gs4_find(order_by='createdTime desc')
  t <- length(row.names(p))
  #read in names from drive 
  names<-which(p$name =='Names')
  names<-read_sheet(p[[3]][[names]]$webViewLink)
  names<- names$Names
  n_names <- length(names)
  set.seed(57)
  rnumb <- sample(n_names)
  code <- data.frame(names,as.character(rnumb))

  #read in sheet with auto correct for reading frame 
  
  sn <- length(sheet_names(p[[3]][[gsheet]]$webViewLink))
  full <- data.frame()

  for(i in 1:sn) {
  temp<- read_sheet(p[[3]][[gsheet]]$webViewLink,skip = 2,sheet = i) %>% 
    clean_names()
  ifelse(colnames(temp)[1]!='x1',
         temp<- read_sheet(p[[3]][[gsheet]]$webViewLink,skip = 1,sheet = i)%>% 
           clean_names(),
         temp)
  x=ncol(temp)
  
  
  for (b in 2:x) {
    ifelse(is.list(temp[b]),temp[b] <- 
             rrapply(temp[b],f=function(y)
               replace(y,is.null(y),NA)) %>% 
             unlist() %>% 
             as.numeric(),
           temp[b])}
  
  temp <- temp %>% remove_empty()%>% 
    mutate(sheet_n=i) %>% 
    mutate(date=as_date(gsub('T.*','',p[[3]][[gsheet]]$createdTime)))
  
  temp <- temp %>%mutate(x1=x1 %>% str_remove_all(coll('.'))) %>% 
    mutate(student=x1)  
    
  size <- nrow(temp)
  
  for (row in 1:size) {
    ifelse(temp$student[row]%in%names,temp$student[row],temp$student[row] <- NA ) 
  }
  temp <- temp %>% fill(student) %>% filter(!student==x1) %>% rename(subject=x1)
  ifelse(is_empty(full),full <- temp,full <- full_join(full,temp,relationship = "many-to-many"))
  }
  full <- full %>% 
    mutate(subject=subject %>% str_remove_all(':')) %>% 
    mutate(subject=subject %>% str_remove_all(coll('.')))%>%
    mutate(subject=subject %>% str_remove_all(coll(' '))) %>% 
    mutate(student=student%>%str_remove_all(coll('.'))) %>% 
    mutate(subject=tolower(subject)) %>% 
    # get rid of unimportant sufexes  
    mutate(temp=subject) %>% 
    mutate(subject=subject %>% str_remove_all('/|1.*|2.*|3.*|4.*|5.*|6.*|7.*|8.*|9.*|0.*'))%>% 
    mutate(subject=if_else(str_ends(subject,'gr'),str_sub(subject,end = -3L),subject))%>% 
    mutate(subject=if_else(str_ends(subject,'grade'),str_sub(subject,end = -6L),subject)) %>% 
    mutate(subject=if_else(subject=='',temp,subject)) %>% 
    select(-temp)
  
  #anon names 
  for (x in 1:n_names) {
    full<- full%>% 
      mutate(student=case_when(student==code[x,1]~code[x,2],TRUE~student))  
  }
  
  return(full)
}

shift_3 <- function(x,y=0){
n_col<- ncol(x)
n_row <- nrow(x)
for (i in 1:n_row) {
  ifelse(is.na(x$total[i]),x$total[i] <-as.numeric(x[i,n_col-2-y]), x$total[i])
  ifelse(is.na(x$days[i]),x$days[i] <-as.numeric(x[i,n_col-1-y]), x$days[i])
  ifelse(is.na(x$grade[i]),x$grade[i] <-as.numeric(x[i,n_col-y]), x$grade[i])
}
return(x)
}

decode <- function(y=nh){
p<-gs4_find(order_by='createdTime desc')
t <- length(row.names(p))
#read in names from drive 

names<-which(p$name =='Names')
names<-read_sheet(p[[3]][[names]]$webViewLink)
names<- names$Names
n_names <- length(names)
set.seed(57)
rnumb <- sample(n_names)
code <- data.frame(names,as.character(rnumb))
y$student <- as.character(y$student)
#decode
for (x in 1:n_names) {
y <- y%>% 
mutate(student=case_when(student==code[x,2]~code[x,1],TRUE~student))  
}
return(y)
}
anon <- function(y=full_NH){
  p<-gs4_find(order_by='createdTime desc')
  t <- length(row.names(p))
  #read in names from drive 
  
  names<-which(p$name =='Names')
  names<-read_sheet(p[[3]][[names]]$webViewLink)
  names<- names$Names
  n_names <- length(names)
  set.seed(57)
  rnumb <- sample(n_names)
  code <- data.frame(names,as.character(rnumb))
  y$student <- as.character(y$student)
  #decode
  for (x in 1:n_names) {
    y <- y%>% 
      mutate(student=case_when(student==code[x,1]~code[x,2],TRUE~student))  
  }
  return(y)
}
#################### data frames dec-oct ####################################################
oct <- read_NH(2)%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))

sep  <- read_NH(3) %>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))


aug  <- read_NH(4)%>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))
july <- read_NH(5)%>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))

june <- read_NH(6) %>% mutate(date=date-20)
#6 columns left on end 

may  <- read_NH(7) %>% shift_3(y=3) %>% 
  select(c(subject,total,days,grade,sheet_n,date,student))

apr  <- read_NH(8)%>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))

mar  <- read_NH(9)%>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))
feb  <- read_NH(10)%>% shift_3()%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))
jan  <- read_NH(11)%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))
dec  <- read_NH(12)%>% 
  select(c(subject,total,days,grade,sheet_n,date,student))

full_NH2 <- dec %>% full_join(jan) %>% 
  full_join(feb) %>% 
  full_join(mar) %>% 
  full_join(apr) %>% 
  full_join(may) %>% 
  full_join(june) %>% 
  full_join(july) %>% 
  full_join(aug) %>% 
  full_join(sep) %>% 
  full_join(oct)



full_NH <- full_NH %>% 
mutate(date=round_date(date,'month')) 

unique(full_NH$subject) 

write.csv(temp,'./nhdata.csv',row.names = FALSE)
#####################graph tests#############

nh$date <- as_date(nh$date)
full_NH %>% 
  filter(student==753)



p <- nh %>% 
  filter(student==753) %>%
  ggplot(aes(x=date,y=grade,color=subject,group=subject))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90,vjust = .5))+
  scale_x_date(date_breaks = '1 month',date_labels = '%B',limits =as_date(c( '2023-7-01','2023-10-01')))

               ggplotly(p)

nh <- read.csv('./nhdata.csv')
nh<- decode(nh)


temp <- nh %>% 
  mutate(student=case_when(student=='Avery D'~'Avery',TRUE~student),
         student=case_when(student=='Noah N'~'Noah',TRUE~student),
         student=case_when(student=='Max D'~'Max',TRUE~student))

temp <- anon(temp)




