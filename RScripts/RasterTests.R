library(terra)

band_4 <- rast("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/Data/West_Timor_Clipped/LC81110672015230LGN00_B4_clip.TIF")
band_5 <- rast("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/Data/West_Timor_Clipped/LC81110672015230LGN00_B5_clip.TIF")
band_6 <- rast("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/Data/West_Timor_Clipped/LC81110672015230LGN00_B6_clip.TIF")

allbands <- c(band_4,band_5,band_6)

writeRaster(allbands,datatype='FLT4S',filename='new_data.tif', overwrite=TRUE)

?dataType
