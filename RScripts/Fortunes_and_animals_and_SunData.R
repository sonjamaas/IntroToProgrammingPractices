????""
install.packages("fortunes")
library(fortunes)
fortune()
fortune("memory")
 install.packages("cowsay")
library(cowsay) 
say("Hello world") 

someone_say_hello <- function(){
  animal <- sample(names(animals),1)
  say(paste("Hello, I'm a ", animal, ".",collapse=""), by=animal)
}
someone_say_hello()

someone_say_my_fortune <- function(){
  animal <- sample(names(animals),1)
  say(paste(fortune(),collapse="\n"),by=animal)
}
someone_say_my_fortune()

install.packages("suncalc")
install.packages("V8")
library(suncalc)
library(V8)
getSunlightTimes(date=Sys.Date(),lat=49.782332,lon=9.970187,tz="CET")
