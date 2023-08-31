#assignment 2

list.files(path = "Data", pattern = 'csv')
cvs_files <- list.files('Data', pattern = 'csv')
length(cvs_files)
read.csv('./data/wingspan_vs_mass.csv')
df <- read.csv('./data/wingspan_vs_mass.csv')
head(df,n=5)
list.files(path='Data',
           pattern = "^b",#'^' means starts with
           recursive = TRUE,)



?read.csv
?startsWith
?list.files()






