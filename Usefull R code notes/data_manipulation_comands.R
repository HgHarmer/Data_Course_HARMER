library(tidyverse)
library(nycflights13)
dat <- flights
attach(dat)

#filter observations by value 
dat_feb <- filter(dat,month==2)
#reorder based on rows
dat_dep <- arrange(dat,-dep_time)
#pick out the data you want
dat_times <- select(dat,dep_time,arr_time)
#create new columns (this is a bad example because they aren't separated into hours and minutes yet)
diff_dat <- mutate(dat_times,arr_time-dep_time)
#you can create multiple new columns from an old one but similarly to select unmentioned columns are deleted 
 dat_times_2 <- dat_times %>% 
  transmute(
            dep_hour=dep_time%/%100,
            dep_min=dep_time%%100,
             hour_arr=arr_time%/%100,
             min_arr=arr_time%%100,
   )
#rename allows you to rename columns (new name comes first)
 dat_times_2<- dat_times_2 %>% 
   rename(arr_hour=hour_arr,
          arr_min=min_arr)
#summarize can be used to condense a column into one row  

 summarise(dat,delay=mean(dep_delay,na.rm = TRUE))
 
 
 
 
 