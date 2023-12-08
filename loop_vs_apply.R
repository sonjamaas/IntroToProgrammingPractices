##Loop vs apply - first example

#create a matrix
m <- matrix(data=cbind(rnorm(30,0),rnorm(30,2),rnorm(30,5)),nrow=30, ncol=3)

#get the mean for all rows
mean(m[,1])
mean(m[,2])

#or with for loop
m.mean <- vector()
for(i in 1:ncol(m)){m.mean[i] <- mean(m[,1])}

#or with apply
#get mean for all rows
apply(m,1,mean)
#get the mean for all columns
apply(m,2,mean)
#stats to execute: mean or range, sum, fivenum etc.

#apply with own function
#function queries the length of x values below 0
apply(m,2,function(x) length (x[x<0]))
#only want to calculate the mean for values abov 0
apply(m,2,function(x) mean(x[x>0]))

#insert a list or a vector data and all values will be run through this function
sapply(1:3, function(x) x^2)

#or return a list
lapply(1:3, function(x) x^2)

#create new list with various entries
x <- list(a=1:10, beta=exp(-3:3), logic=c(TRUE,FALSE,FALSE,TRUE))

#return the mean of each list entry
lapply(x,mean)
