# Practices concerning sequences, tables, matrixes and data frames


X <- seq(1,100,by=2.5)       # create new sequence X from 1 to 100 with 2.5 steps
X[5]                         # get 5th position of sequence
X[4:10]                      # get 4th to 10th positions of sequence
X[length(X)]                 # get the last number of the sequence
X[-2:-10]                    # get the sequence without 2nd to 10th positions
X[-6]                        # get sequence without 6th position
X>20                         # get all numbers that are lower than 20
X[X<10|X>30]                 # get numbers of sequence between 10 and 30

X2 <- numeric(length(X))     # get an empty sequence X2 the length of X
X2[X<=30] <- 1               # assign the value 1 to all positions on the sequence smaller 30
X2[(X>30)&(X<70)] <- 2       # assign the value 2 to all positions on the sequence bigger 30 and smaller 70
X2[X>70] <- 3                # assign the value 3 to all positions on the sequence bigger 70

install.packages("car")
install.packages("carData")
library(car)

X3 <- recode(X,"0:30=1;30:70=2;else=3")   # create a sequence wich is a recode of X with the same values as X2

m1 <- matrix(c(4,7,3,8,9,2),nrow=2)       # create a matrix of the numeric vector divided into 2 rows,
# the matrix is filled with the numbers by column

m2 <- matrix(c(4,7,3,8,9,2),nrow=2,ncol=3,byrow=TRUE)   # create a matrix of the numeric vector with 2 rows and 3 
# columns, filling the matrix with the numbers by row

m1[,2]                                    # shows values of column 2 in m1
m1[2,]                                    # shows values of row 2 in m1
m1[2,2]                                   # shows value of the number in row 2 column to of m1
m2[2,2]                                   # shows value of the number in row 2 column to of m2


numbers_1 <- rnorm(80,mean=0,sd=1)        # variable number_1 becomes a normally distributed numeric value with 80 numbers, 
# it has a mean of 0 and a standard distribution of 1.

mat_1 <- matrix(numbers_1,nrow=20,ncol=4) # variable mat_1 becomes a matrix of numbers_1, with 20 rows and 4 columns

df_1 <- data.frame(mat_1)                 # variable df_1 becomes the into a data frame (similar as matrix but with numeric values and 
# characters) converted mat_1

names(df_1) <- c("var1","var2","var3","var4")   # the names of the columns of df_1 are being set with a character vector 

head(df_1)                                # get the first 6 rows of df_1
summary(df_1)                             # get a summary of df_1 (Min., 1st quart., Median, Mean, 3rd quart., Max.)

plot(df_1[,2])                            # plot column 2 of df_1

test <- data.frame(A=c(1,2,3),B=c("aB1","aB2","aB3"))    # variable test becomes a data frame with two columns A and B,
# A consisting of a numeric vector and B consisting of a character vector.
test[,1]                                  # three commands to get the first row of test
test[,"A"]
test$A
