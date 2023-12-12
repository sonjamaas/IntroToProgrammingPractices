# Practices concerning Data Import and display

getwd()                      # get working directory

my.df <- read.csv("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/lecture2/tablePractice.csv", 
    header=TRUE,sep=";")     # import a table from a .csv file
my.df                        # show table in console

head(my.df)                  # show head of table
summary(my.df)               # summarize table

write.table(my.df, file="tablePracticeExport.csv", sep = ";") ##export table as .csv

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
 
