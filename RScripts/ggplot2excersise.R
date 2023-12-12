install.packages("ggplot2")
library(ggplot2)
x11()
x <- data.frame(x=1,y=1,label="ggplot2 introduction \n@ EAGLE")

ggplot(data=x, aes(x=x,y=y))+geom_text(aes(label=label, colour="pink"),size=15)


#creating data for excercise

x1 <- rnorm(1000,0,1)
x2 <- rnorm(1000,5,10)
x3 <- rep(c("catA","catB","catB","catC","catC","catC"),200)[1:1000]
x4 <- rnorm(1000,0,1)

df <- data.frame(a=x1,b=x2,c=x3,d=x4)
df
library(ggplot2)
ggplot(df,aes(a,b,colour=c))+
  geom_point(alpha=.5)+labs(title="First plot", x="x axis \n and a new line")


ggplot(df, aes(a))+ 
  geom_histogram(color="white")

ggplot(df, aes(a))+ 
  geom_density()


#combining plots
ggplot(df)+  
  geom_histogram (aes(a,after_stat(density)), fill="blue", colour="darkgrey")+ 
  geom_density(aes(a,after_stat(density)),colour="yellow")+
  geom_rug(aes(a))

ggplot(df,aes(c,color=c))+
  geom_point(stat="count",size=4)
ggplot(df)+
  geom_bar(aes(c))+
  coord_flip()
ggplot (df,aes(d,a))+
  geom_boxplot()
ggplot(df,aes(d,a))+
  geom_boxplot()+
  geom_jitter(alpha=.5,width=.3,color="hotpink")


