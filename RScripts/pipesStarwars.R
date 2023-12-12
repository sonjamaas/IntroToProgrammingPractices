install.packages("pak")
pak::pak("tidyverse/dplyr")

#filter all droids
starwars %>% 
  filter(species == "Droid")

#select all rows that have something to do with color
starwars %>% 
  select(name, ends_with("color"))

#calculate the BMI
starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)

#arrange by mass (descending)
starwars %>% 
  arrange(desc(mass))

#get mean mass of the species
starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    mass > 50
  )

View(starwars)
plot(starwars$mass,starwars$height)
library(ggplot2)
ggplot(starwars,aes(x=name,y=mass))+
  geom_bar()
