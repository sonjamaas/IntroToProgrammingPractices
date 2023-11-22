

x1 <- c("forest","forest","forest","forest","forest",
        "agriculture","agriculture","agriculture","agriculture","agriculture",
        "water","water","water","water","water",
        "urban","urban","urban","urban","urban",
        "wetland","wetland","wetland","wetland","wetland")
x2 <- rep(c("forest","agriculture","water","urban","wetland"),5)
x3 <- c(25,0,0,0,0,
        0,25,0,0,0,
        0,0,25,0,0,
        0,0,0,25,0,
        0,0,0,0,25)
x4 <- rep(c(2023),25)

head(m1)
df <- data.frame(x1,x2,x3,x4)
head(df)

names(df) <- c("orig_cover","new_cover","flow","year")

View(df)

#install.packages("circlize")
library(circlize)
adjacencyData <- with(df, table(x1,x2))
done <- chordDiagram(adjacencyData, transparency = .5)

