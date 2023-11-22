#Steigerwald Practice
install.packages("RCurl")
library(RCurl)

#explore the dataset
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
install.packages("paletteer")
library(paletteer)
install.packages("ggthemes")
library(ggthemes)
install.packages("viridis")
library(viridis)
library(ggplot2)

#PLotting
plot(df[df["SRTM"]>400,c("TimeScan.NDVImax","SRTM")])

#create empty plot (no geom added)
ggplot(df,aes(x=L8.ndvi,y=L8.savi))

#create scatterplot
ggplot(df,aes(x=L8.ndvi,y=L8.savi))+
  geom_point()

#add information using colour
ggplot(df,aes(x=L8.ndvi,y=L8.savi, colour=SRTM))+
  geom_point()

#adding smoothed lines
ggplot(df,aes(x=L8.ndvi,y=L8.savi, colour=SRTM))+
  geom_point()+
  geom_smooth()

#split the plot by landcover
ggplot(df,aes(x=L8.ndvi,y=L8.savi, colour=SRTM))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~LCname)

#plot SAVI values per landcover class
ggplot()+
  geom_point(data=df,aes(LCname, L8.savi))

#add colour
ggplot()+
  geom_point(data=df,aes(LCname, L8.savi,colour=SRTM))

#try boxplot with point "jitter"
ggplot(df,aes(x=LCname, y=L8.savi))+
  geom_boxplot(alpha=.5)

#to see number of points, add jitter
ggplot(df,aes(x=LCname, y=L8.savi))+
  geom_boxplot(alpha=.5)+
  geom_point(aes(color=SRTM),alpha=.7, size=1.5, position=position_jitter((width=.25)))

#further options
ggplot(df,aes(x=LCname, y=L8.savi))+
  geom_jitter()

#adding colour
ggplot(df,aes(x=LCname, y=L8.savi,color=SRTM))+
  geom_jitter()

#further options
ggplot(df,aes(x=LCname, y=L8.savi))+
  geom_violin()

ggplot(df,aes(x=TimeScan.NDVIavg, fill=LCname))+
  geom_density(alpha=.3)

#combining geoms
ggplot(df,aes(x=LCname, y=L8.savi,color=SRTM))+
  geom_jitter(aes(alpha=SRTM, size=TimeScan.NDVIsd,colour=L8.ndvi))+
  geom_boxplot(alpha=.5)

#recreate cover image
library(ggplot2)
#ggplot2
ggplot(df, aes(x=L8.ndvi, y=L8.savi))+
 # geom_point(size=2)+
  geom_point(aes(color=LCname),size=2)+
  facet_grid(.~LCname)

#export plot as pdf
pdf("landcover_vs_Lasavi_ndvi.pdf",width=12,height=4) 
ggplot(df, aes(x=L8.ndvi, y=L8.savi))+
  # geom_point(size=2)+
  geom_point(aes(color=LCname),size=2)+
  facet_grid(.~LCname)
dev.off()

#compare minimal and maximal NDVI values of different surfaces
ggplot(df,aes(x=LCname,y=TimeScan.NDVImax))+
  geom_point(aes(color=LCname),size=2)

#compare minimal and maximal NDVI
ggplot(df,aes(factor(LCname),color='red')) +
  geom_point(aes(y=TimeScan.NDVImax),shape="triangle",color='black') +
  geom_point(aes(y=TimeScan.NDVImin),shape="square",color='yellow')


install.packages("sf")
library(sf)

#data frame to spatial object
projcrs <- st_crs(32632)
df.sf <- st_as_sf(x=df,
                  coords=c("x","y"),
                  crs=projcrs) #UTM WGS84 32N, epsg:32632
plot(df.sf)

st_write(df.sf,"C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/steigerwaldExerciseSF/my_shapefile.shp")
st_write(df.sf,"C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/steigerwaldExerciseSF/my_shapefile.gpkg")
