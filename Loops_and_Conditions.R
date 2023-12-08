##Loops, conditions, etc

#if else statement
a <- 5

if(a>0)
{
  print("yes it is true, it is larger")
}

if (a!=5)
{
  print("number is not equal 5")
} else {
  print("number is equal 5")
}

##Nested if-else statements
set.seed(100)
abc <- sample(letters[1:5],1000,replace=T)
df <- data.frame(v1=abc, v2="blank",stringAsFactors=F)
head(df)

system.time({
  df$v2 <- ifelse(df$v1=="a","apple",
                  ifelse(df$v2=="b","ball",
                         ifelse(df$v1=="c","cat",
                                ifelse(df$v1=="d", "dog", "elephant"))))
})
head(df)

#alternative to nested if-else statements
for (i in 1:nrow(df)){
  val <- df$v1[i]
  df$v2[i] <- switch(val,
                     "a"="apple",
                     "b"="banana",
                     "c"="cat",
                     "d"="dog",
                     "e"="elephant")
}
head(df)

##check data automatically with while statement
j <- 0
while(j<1){
  print(j, digits=22)
  j <- j+0.1
}
