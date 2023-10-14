library(tidyverse)
library(janitor)

#tidy the data 
full <-  read.csv('./New Hope Data SEPTEMBER - GH.csv',skip=2) %>% 
   full_join(read.csv('./New Hope Data SEPTEMBER - Kim.csv',skip=2)) %>%         
   full_join(read.csv('./New Hope Data SEPTEMBER - Tiff_Gayle.csv',skip=2)) %>% 
   full_join(read.csv('./New Hope Data SEPTEMBER - Zander.csv',skip=2)) %>% 
   clean_names() %>% 
   remove_empty('cols')
 

NH<- full %>%
  mutate(total=ifelse(is.na(total),x9_25,total)) %>%
  mutate(days=ifelse(is.na(days),x9_26,days)) %>% 
  mutate(grade=ifelse(grade=='',x9_27,grade)) %>%
  mutate(names=case_when(grade==''~x,
                         is.na(grade)~x))%>% 
  filter(!x=='') %>% 
  mutate(names=case_when(names=='C/P:'~NA,
                         names==names~names)) %>% 
  fill(names) %>%
  filter(!names==x) %>% 
  remove_empty('cols',cutoff = 0.5) %>%
  rename(subject=x) %>% 
  mutate(grade=grade %>% str_remove('%')%>% str_remove('#DIV/0!') %>% as.numeric() ) %>% 
  mutate(subject=subject %>% str_remove(':')) 
  
  view() 
 full<-read.csv('./New Hope Data SEPTEMBER - GH.csv',skip=2) %>% 
    full_join(read.csv('./New Hope Data SEPTEMBER - Kim.csv',skip=2)) %>%         
    full_join(read.csv('./New Hope Data SEPTEMBER - Tiff_Gayle.csv',skip=2)) %>% 
    full_join(read.csv('./New Hope Data SEPTEMBER - Zander.csv',skip=2)) %>% 
    clean_names() %>% 
    remove_empty('cols') %>% 
    mutate(month=as.Date('2023-09-01')) %>% 
    view()

  ymd
 
  write.csv(empty,'./empty.csv')
  read.csv('./New Hope Data SEPTEMBER - GH.csv',skip=2) %>% view()
  