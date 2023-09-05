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
#Write a command that displays the first line of each of 
#those “b” files (this is tricky… use a for-loop)
bfiles <- list.files(path='Data',
                     pattern = "^b",
                     recursive = TRUE,)
setwd("./Data")
for (x in bfiles) {print(head(read.csv(x),n=1))}
setwd("..")




##############################################################
?read.csv
?startsWith
?list.files( )
getwd()
#####################tests####################################3
bfiles


list.files(path='Data',
           pattern = "^b",#'^' means starts with
           recursive = TRUE,)



setwd("..")

for (x in read.csv(list.files(path='Data',
                   pattern = "^b",
                   recursive = TRUE,))) {print(head(x,1))}

     
     head(read.csv("Data/data-shell/creatures/basilisk.dat"),n=1)
  
bfiles
dir
for (x in bfiles) {print(head(read.csv(x),1))}


a="Data/data-shell/creatures/basilisk.dat" 
b="Data/data-shell/data/pdb/benzaldehyde.pdb"
c="Data/Messy_Take2/b_df.csv"
bfiles2 <- list.files(path='.',
                      pattern = "^b",#'^' means starts with
                      recursive = TRUE,)
list.files(path='.',
           pattern = "^b",#'^' means starts with
           recursive = TRUE,)
setwd("./Data")
bfiles
