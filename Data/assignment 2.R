library(tidyverse)
list.files('C:/users/hghar/OneDrive/Desktop/Data_Course/Data', pattern = 'csv') 
cvs_files=list.files('C:/users/hghar/OneDrive/Desktop/Data_Course/Data', pattern = 'csv') 
length(cvs_files)
df=read.csv('wingspan_vs_mass.csv')
head(df)
list.files('C:/users/hghar/OneDrive/Desktop/Data_Course/Data', recursive = T)

