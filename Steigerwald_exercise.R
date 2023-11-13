#Steigerwald Practice
install.packages("RCurl")
library(RCurl)

getwd()
df <- read.csv("Steigerwald_sample_points_all_data_subset_withNames.csv")  #read the csv
head(df)
df[3,3]
df[9,8]
df[length(df)-1]
df[1:5,c('LUCAS_LC','S2.1')]
View(df)

df[,3:12]
library(car)
install.packages("carData")
library(carData)


#PLotting
plot(df[df["SRTM"]>400,c("TimeScan.NDVImax","SRTM")])

library(ggplot2)
#ggplot2
ggplot(df, aes(x=L8.ndvi, y=L8.savi))+
 # geom_point(size=2)+
  geom_point(aes(color=LCname),size=2)+
  facet_grid(.~LCname)

pdf("landcover_vs_Lasavi_ndvi_pdf")


ggplot(df,aes(x=LCname,y=TimeScan.NDVImax))+
  geom_point(aes(color=LCname),size=2)

ggplot(df,aes(factor(LCname),color='red')) +
  geom_point(aes(y=TimeScan.NDVImax),shape="triangle",color='black') +
  geom_point(aes(y=TimeScan.NDVImin),shape="square",color='yellow')

