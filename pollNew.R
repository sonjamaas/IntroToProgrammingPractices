# reading line by line
poll <- readLines("poll.md")
str(poll)                 #find out structure
typeof(poll)              #find out data type/mode (character, numeric)
dim(poll)                 #find out dimension
length(poll)              #find out length

poll[1]                   #fist element in poll
poll                      #show whole object

#filter/exclude empty elements
poll <- poll[poll != ""]          #show poll if not empty and change the variable 

#convert this vector into data frame
df <- as.data.frame(matrix(poll, byrow=T, ncol=4))   #matrix from poll vector, data added by row, 4 columns. Data frame around it, put it in new variable.
df
dim(df)      #find out dimensions
colnames(df) <- c("name","exp","lang","os")     #change column names
colnames(df)      #show column names

View(df)          #view the new data frame in a visualization

#clean name column
df$name <- gsub("#","",df$name)          #replace "#" with " " in column name with function subsidize
df$name <- trimws(df$name)      #trim white space function

#clean exp column
df$exp <- gsub("[*]","",df$exp)
df$exp <- trimws(df$exp)
df$exp[df$exp=="0-5"] <- "2.5"    #change the "0-5" to the mean of it
as.numeric(df$exp) #change character to numeric

#clean os
df$os <- gsub("[*]","",df$os)
df$os <- trimws(df$os)

#clean lang
df$lang <- gsub("[*]","",df$lang)
df$lang <- trimws(df$lang)
df$lang[df$lang=="no one"] <- "None"
df$lang <- tolower(df$lang) #make everything lower case

#task1: how can we make a vector from df lang that contains each language mentioned as a single element
#python, r, javascript, rstudio, r, python ....
#task2: visualization by wordcloud (install package workcloud, load it and try to make a word cloud from language vector)
df$lang
