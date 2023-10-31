getwd()

my.df <- read.table("tablePractice.csv", header=TRUE,sep="/")

my.df
head(my.df)

my.df2 <- read.csv("tablePractice.csv", header=TRUE,sep=";")
my.df2
head(my.df2)
summary(my.df2)
write.table(my.df2, file="tablePracticeExport.csv", sep = ";")

install.packages("datapasta")
library(datapasta)
x <- tibble::tribble(
       ~A,        ~B,      ~C,
  "hello",    "what",    "up",
  "ufaef",   "skdjf", "akejf",
   "sfkj", "getghth", "mnbmv"
  )
x

c("A\tB\tC", "1\thello\twhat\tup", "2\tufaef\tskdjf\takejf", "3\tsfkj\tgetghth\tmnbmv", "嬀閌Ȣ")
c("A\tB\tC",
  "1\thello\twhat\tup",
  "2\tufaef\tskdjf\takejf",
  "3\tsfkj\tgetghth\tmnbmv",
  "윀靸Ȣ")

install.packages("RCurl")
library(RCurl)
df <- read.csv("Steigerwald_sample_points_all_data_subset_withNames.csv")
df
head(df)
tail(df)
summary(df)
plot(df)
str(df)

X <- seq(1,100,by=2.5)
X
X[5]
X[4:10]
X[length(X)]
X[-2:-10]
X[-6]
X>20
X[X<10|X>30]
X2 <- numeric(length(X))
X2
X2[X<=30] <- 1
X2
X2[(X>30)&(X<70)] <- 2
X2
X2[X>70] <- 3
X2
install.packages("car")
install.packages("carData")
library(car)
X3 <- recode(X,"0:30=1;30:70=2;else=3")
X3

m1 <- matrix(c(4,7,3,8,9,2),nrow=2)
m1
m2 <- matrix(c(4,7,3,8,9,2),nrow=2,ncol=3,byrow=TRUE)
m2
m1[,2]
m1[2,]
m1[2,2]
m2[2,2]

numbers_1 <- rnorm(80,mean=0,sd=1)
mat_1 <- matrix(numbers_1,nrow=20,ncol=4)
mat_1
df_1 <- data.frame(mat_1)
names(df_1) <- c("var1","var2","var3","var4")
df_1
head(df_1)
summary(df_1)
head(mat_1)
plot(df_1[1,])

test <- data.frame(A=c(1,2,3),B=c("aB1","aB2","aB3"))
test[,1]
test[,"A"]
test$A
