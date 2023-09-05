####assignment 2####
#Write a command that lists all of the .csv files found in 
#the Data/ directory and stores that list in an object 
#called “csv_files”
list.files(path = "Data", pattern = 'csv')

cvs_files <- list.files('Data', 
                        pattern = 'csv',
                        full.names = TRUE)
##Find how many files match that description using the length() function
length(cvs_files)
#Open the wingspan_vs_mass.csv file and store the contents
#as an R object named “df” using the read.csv() function
read.csv('./data/wingspan_vs_mass.csv')

df <- read.csv('./data/wingspan_vs_mass.csv')
##Inspect the first 5 lines of this data set using the
##head() function
head(df,n=5)
#Find any files (recursively) in the Data/ directory 
#that begin with the letter “b” (lowercase)
list.files(path='Data',
           pattern = "^b",#'^' means starts with
           recursive = TRUE,
           full.names = TRUE) #full.names=TRUE includes the entire file path
#Write a command that displays the first line of each of 
#those “b” files (this is tricky… use a for-loop)

bfiles <- list.files(path='Data',
                     pattern = "^b",
                     recursive = TRUE,
                     full.names = TRUE)


for (x in bfiles) {print(readLines(x,n=1))}

#Do the same thing for all files that end in “.csv”

for (x in cvs_files) {print(readLines(x,n=1))}

#ctrl alt B enter runs all code above cursor

