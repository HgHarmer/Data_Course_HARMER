###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################


# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####
dat <- iris
# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150


dat[seq(2,150,2),]


# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

SepalLength<- as.character(iris$Sepal.Length)
SepalWidth<- as.character(iris$Sepal.Width)
PetalLength<- as.character(iris$Petal.Length)
PetalWidth<- as.character(iris$Petal.Width)
Species_<- as.character(iris$Species)
iris_chr<- cbind(Species_,SepalWidth,SepalLength,PetalLength,PetalWidth)
# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

sepal.area <- iris$Sepal.Length*iris$Sepal.Width

# 4.  Add Sepal.Area to the iris data frame as a new column

dat$sepal.area <- as.numeric(dat$Sepal.Length*dat$Sepal.Width)

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
      # (name it big_area_iris)
dat$sepal.area>20

big_area_iris <- dat[c(dat$sepal.area>20),]


# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated

