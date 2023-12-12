#Eagle skills Animation
install.packages("gganimate")
install.packages("gifski")#
install.packages("av")
install.packages("png")
library(png)
library (RCurl)
library(ggplot2)
library(gganimate)
library(gifski)
library(av)

df <- read.csv("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/EAGLE_course_ggplot - Form responses 1.csv")
summary(df)
head(df)

df1 <- data.frame(df[,c(2:6,10,14)],semester=1,courses=11)
df2 <- data.frame(df[,c(2:5,7,11,15)],semester=2,courses=15)
df3 <- data.frame(df[,c(2:5,8,12,16)],semester=3,courses=2)
df4 <- data.frame(df[,c(2:5,9,13,17)],semester=4,courses=3)

df.names <- c("eyecolour","haircolour","glasses","sex","eoExp","progExp","praesExp","semester","courses")

names(df1) <- df.names
names(df2) <- df.names
names(df3) <- df.names
names(df4) <- df.names

df <- rbind(df1,df2,df3,df4)

View(df)

p <- ggplot(data=df,aes(y=eoExp,x=progExp,color=eyecolour,size=sex))+
  geom_point(alpha=.8)

p

p+transition_time(semester)+
  ease_aes('linear')+
  labs(title="semester:{round(frame_time,1)}")+
  shadow_wake(wake_length=.1,alpha=FALSE)+
  enter_fade()+
  exit_fade()

anim_save("eagle_EO_vs_prog_experience.gif")

#boxplot fade by sex

p <- ggplot(data=df, aes(y=eoExp, x=sex,color=glasses))+
  geom_boxplot(alpha=.5,fill="green")+
  geom_jitter(col="blue",width=0.3,alpha=.5,size=3)

p

p+transition_time(semester)+
  ease_aes('linear')+
  labs(title="semester:{round(frame_time,1)}")+
  enter_fade()+
  exit_fade()+
  ease_aes('sine-in-out')

anim_save("eagle_EO_experiences_semester_boxplot_sex.gif")
