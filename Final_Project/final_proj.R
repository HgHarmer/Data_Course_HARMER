library(tidyverse)
library(janitor)
library(googlesheets4)
library(purrr)
library(rrapply)

#This tells me information about all sheets found in the associated Google account
p<-gs4_find(order_by='createdTime desc')
#how many files are in Google drive
t <- length(row.names(p))
# how many pages are in a given work book?
sn <- length(sheet_names(p[[3]][[x]]$webViewLink))

#finds the google sheet that has student names 
names<- which(p$name =='Names')
names <- read_sheet(p[[3]][[names]]$webViewLink)
#makes an empty dataframe that will be filled 
full <- data.frame()





NH_data <- function(x=1,skip=2){
  p<-gs4_find(order_by='createdTime desc')
  t <- length(row.names(p))
  sn <- length(sheet_names(p[[3]][[x]]$webViewLink))
  names<- which(p$name =='Names')
  names <- read_sheet(p[[3]][[names]]$webViewLink)
  full <- data.frame()
  for(i in 1:sn) {
    temp<- read_sheet(p[[3]][[x]]$webViewLink,skip = skip,sheet = i)
    temp <- temp %>% clean_names() %>% remove_empty()%>% mutate(sheet=i)
    ifelse(is_empty(full),full <- temp,full <- full_join(full,temp,relationship = "many-to-many",))
    full <- full %>% mutate(date=as_date(gsub('T.*','',p[[3]][[x]]$createdTime)))
  }
  return(full)
  }

NH_data_skip1 <- function(x=1){
  p<-gs4_find(order_by='createdTime desc')
  t <- length(row.names(p))
  sn <- length(sheet_names(p[[3]][[x]]$webViewLink))
  full <- data.frame()
  for(i in 1:sn) {
    temp<- read_sheet(p[[3]][[x]]$webViewLink,skip = 1,sheet = i)
    temp <- temp %>% clean_names() %>% remove_empty()%>% mutate(sheet=i)
    ifelse(is_empty(full),full <- temp,full <- full_join(full,temp,relationship = "many-to-many",))
    full <- full %>% mutate(date=as_date(gsub('T.*','',p[[3]][[x]]$createdTime)))
  }
  return(full)
}

sep  <- NH_data(1)
aug  <- NH_data(2)
july <- NH_data(3)
june <- NH_data(4)
may  <- NH_data(5)
apr  <- NH_data(6)
mar  <- NH_data(7)
feb  <- NH_data(8)
jan  <- NH_data(9,1)
dec  <- NH_data(10)


####this code 
#1 checks if a column is a list
#2 if it is a list it will unlist it while maintaining NULL values as NA values 




#Counts columns 
x=ncol(temp)

#unlists all lists in a column and replaces NULL lists with NA values 
ifelse(is.list(temp[x]),temp[x] <- 
  rrapply(temp[x],f=function(y)
  replace(y,is.null(y),NA)) %>% 
  unlist() %>% 
  as.numeric(),
  temp[x]
  )
test

temp<- read_sheet(p[[3]][[9]]$webViewLink,skip = 1,sheet = 1) %>% clean_names()

tempb<- read_sheet(p[[3]][[9]]$webViewLink,skip = 2,sheet = 1)%>% clean_names()


temp2<- read_sheet(p[[3]][[9]]$webViewLink,skip = 1,sheet = 2)

# code to get the data into the correct reading frame

temp<- read_sheet(p[[3]][[9]]$webViewLink,skip = 2,sheet = 1)%>% 
  clean_names()
ifelse(colnames(temp)[1]!='x1',
       temp<- read_sheet(p[[3]][[9]]$webViewLink,skip = 1,sheet = 1)%>% 
         clean_names(),
       temp)

#code that turns vectors of lists into vectors of int
x=ncol(temp)
for (i in 2:x) {
ifelse(is.list(temp[i]),temp[i] <- 
         rrapply(temp[i],f=function(y)
                      replace(y,is.null(y),NA)) %>% 
                    unlist() %>% 
                    as.numeric(),
                  temp[i])
}
#get rid of empty columns and add time 
temp <- temp %>% 
  remove_empty() %>% 
  mutate(date=as_date(gsub('T.*','',p[[3]][[9]]$createdTime))) %>% 
  filter(!is.na(x1)) %>% 
mutate(names=case_when(is.na(grade)~x1),
       
       
       case_when())
                      
  fill(names)
  filter(!names==x1)
  
  
  
  #get a row for students and filter out all subjects 
temp <- temp %>% mutate(student=x1)
size <- nrow(temp)
for (i in 1:size) {
ifelse(temp$student[i]%in%names,temp$student[i],temp$student[i] <- NA ) 
}

temp <- temp %>% fill(student) %>% filter(!student==x1) %>% rename(subject=x1)
  






temp <- temp %>% select(-`student <- x1`) 
which(names[24]==temp$x1)
which(nam[24]==temp$x1)

x1==names[i,1]
nam[i]%in%temp$x1
which(nam[20]==temp$x1)
any(==names[3,1])




nam<- names$Names

contains(nam[1],temp$x1)

temp<- read_sheet(p[[3]][[1]]$webViewLink,skip = 2,sheet = 3)
  temp <- temp %>% clean_names() %>% remove_empty()
test <- full_join(temp,temp,relationship = "many-to-many",multiple = "all")
?full_join()

for(i in 1:x) {
  temp<- read_sheet(p[[3]][[1]]$webViewLink,skip = 2,sheet = i)
  temp <- temp %>% clean_names() %>% remove_empty()
  ifelse(is_empty(full),full <- temp,full <- full_join(full,temp))
  full <- full %>% mutate(date=as_date(gsub('T.*','',p[[3]][[i]]$createdTime)))
}  
sep <- full








full <- data.frame()
x <- length(sheet_names(p[[3]][[1]]$webViewLink))

for(i in 1:x) {
  temp<- read_sheet(p[[3]][[1]]$webViewLink,skip = 2,sheet = i)
  ifelse(is_empty(full),full <- temp,full <- full_join(full,temp))
  mutate(full,date=as_date(gsub('T.*','',p[[3]][[1]]$createdTime)))
}



x <- length(sheet_names(p[[3]][[1]]$webViewLink))

# we start with the first page in the Google sheets workbook
full <- read_sheet(p[[3]][[1]]$webViewLink,skip = 2,sheet = i)
#full join remaining sheets(2-x)
for (i in 2:x) {
  temp<- read_sheet(p[[3]][[1]]$webViewLink,skip = 2,sheet = i)
  ifelse(is_empty(full),full <- temp,full <- full_join(full,temp))
  
}
#clean names and remove empty columns
full <- full %>% 
  clean_names() %>% 
  remove_empty() %>% 
  mutate(date=as_date(gsub('T.*','',p[[3]][[1]]$createdTime)))


#this cleaning code is designed for September due to shifted cells 
NH<- full %>%
  mutate(total=ifelse(is.na(total),x9_25,total)) %>%
  mutate(days=ifelse(is.na(days),x9_26,days)) %>% 
  mutate(grade=ifelse(is.na(grade),x9_27,grade)) %>%
  mutate(names=case_when(grade==''~x1,
                         is.na(grade)~x1)) %>%  
  
  
  filter(!x1=='') %>% 
  mutate(names=case_when(names=='C/P:'~NA,
                         names==names~names)) %>% 
  fill(names) %>%
  filter(!names==x1) %>% 
  remove_empty('cols',cutoff = 0.5) %>%
  rename(subject=x1) %>% 
  mutate(subject=subject %>% str_remove(':')) 
  
  view() 
 

  
 

  
  
  
  for (i in 1:t) {
    x <- length(sheet_names(p[[3]][[i]]$webViewLink))
    for(y in 1:x) {
      temp<- read_sheet(p[[3]][[i]]$webViewLink,skip = 2,sheet = y)
      temp <- temp %>% clean_names() %>% remove_empty()
      ifelse(is_empty(full),full <- temp,full <- full_join(full,temp))
      full <- full %>% mutate(date=as_date(gsub('T.*','',p[[3]][[i]]$createdTime)))
    }
    ifelse(is_empty(NH),NH <- full,NH <- full_join(NH,full))
  }
  
  full <- full %>% 
    clean_names() %>% 
    remove_empty()
  
  
  
  for(i in 1:sn) {
    ifelse(p[[1]][[9]]=='Names', x <- x+1, x <- x)
    temp<- read_sheet(p[[3]][[9]]$webViewLink,skip = 2,sheet = i) %>% 
      clean_names()
    ifelse(colnames(temp)[1]!='x1',
           temp<- read_sheet(p[[3]][[9]]$webViewLink,skip = 1,sheet = i)%>% 
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
      mutate(sheet=i) %>% 
      mutate(date=as_date(gsub('T.*','',p[[3]][[9]]$createdTime))) %>% 
      mutate
    temp <- temp %>% mutate(student=x1)
    size <- nrow(temp)
    for (row in 1:size) {
      ifelse(temp$student[row]%in%names,temp$student[row],temp$student[row] <- NA ) 
    }
     temp <- temp %>% fill(student) %>% filter(!student==x1) %>% rename(subject=x1)
    ifelse(is_empty(full),full <- temp,full <- full_join(full,temp,relationship = "many-to-many"))
   
  }
  

  
  n

    
    p<-gs4_find(order_by='createdTime desc')
    t <- length(row.names(p))
    #read in names from drive 
    names<-which(p <- name =='Names')
    names<-read_sheet(p[[3]][[names]]$webViewLink)
    names<- names$Names
    n_names <- length(names)
    #read in sheet with auto correct for reading frame 
    
    sn <- length(sheet_names(p[[3]][[sheet]]$webViewLink))
    full <- data.frame()
    for(i in 1:sn) {
      ifelse(p[[1]][[sheet]]=='Names', x <- x+1, x <- x)
      temp<- read_sheet(p[[3]][[sheet]]$webViewLink,skip = 2,sheet = i) %>% 
        clean_names()
      ifelse(colnames(temp)[1]!='x1',
             temp<- read_sheet(p[[3]][[sheet]]$webViewLink,skip = 1,sheet = i)%>% 
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
        mutate(sheet=i) %>% 
        mutate(date=as_date(gsub('T.*','',p[[3]][[sheet]]$createdTime))) %>% 
        mutate
      temp <- temp %>% mutate(student=x1)
      size <- nrow(temp)
      for (row in 1:size) {
        ifelse(temp$student[row]%in%names,temp$student[row],temp$student[row] <- NA ) 
        temp <- temp %>% fill(student) %>% filter(!student==x1) %>% rename(subject=x1)
        ifelse(is_empty(full),full <- temp,full <- full_join(full,temp,relationship = "many-to-many"))
      }
      
    }
  
  
  
  aug <- read_NH(9)
  
 
  
  
  
  
  