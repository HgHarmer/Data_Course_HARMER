library(tidyverse)





#LISTS a new class of object like a vector but any 
a <- 1:10
b <- letters
c <- c(TRUE,TRUE,FALSE)
x <- list(a,b,c)
#look at first list item and give me the third thing 
x[[1]][3]

x[[1:3]][1]

for (i in 1:3) {
  print(x[[i]][1])
}


#PURRR/MAP
#map can be used to look into lits 
map(x,1)
# this gives a list back if we use a modified map cpmmand we can change that 
map_chr(x,1)


y <- list(a=iris,
     b=mtcars,
     c=NULL)
map(y,class)
map(x,class)


#function that takes first and second columns and multiply them 
make_products <- 
function(x){
  if(!is.numeric(x[1])){
    x[,1] <- as.numeric(x[,1])
  }
  if(!is.numeric(x[2])){
    x[,2] <- as.numeric(x[,2])
  }
  
  newcol <- x[1]*x[2]
x['products'] <- newcol
return(x)
}
         
lapply(iris,as.character)         
map(y,make_products)

y$a$Sepal.Length <- as.character(y$a$Sepal.Length)





