#assignment 2

list.files(path = "Data", pattern = 'csv')
cvs_files <- list.files('Data', 
                        pattern = 'csv',
                        full.names = TRUE)
length(cvs_files)

read.csv('./data/wingspan_vs_mass.csv')

df <- read.csv('./data/wingspan_vs_mass.csv')

head(df,n=5)

list.files(path='Data',
           pattern = "^b",#'^' means starts with
           recursive = TRUE,
           full.names = TRUE)
#Write a command that displays the first line of each of 
#those “b” files (this is tricky… use a for-loop)

bfiles <- list.files(path='Data',
                     pattern = "^b",
                     recursive = TRUE,
                     full.names = TRUE)


for (x in bfiles) {print(readLines(x,n=1))}

#Do the same thing for all files that end in “.csv”

for (x in cvs_files) {print(readLines(x,n=1))}


##############################################################
?read.csv
?startsWith
?list.files( )
getwd()
?readLines()
#####################tests####################################
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
for (x in 1:5) {print(x*5)}
  



