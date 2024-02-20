# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: 
install.packages("tidytuesdayR")
library(tidytuesdayR)
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-07-07')
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

coffee_ratings <- tuesdata$coffee_ratings

# Or read in the data manually

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

plot(coffee_ratings$aroma)

library(janitor)
install.packages("janitor")

raw_arabica <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv") %>% 
  janitor::clean_names()

raw_robusta <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/robusta_data_cleaned.csv",
                        col_types = cols(
                          X1 = col_double(),
                          Species = col_character(),
                          Owner = col_character(),
                          Country.of.Origin = col_character(),
                          Farm.Name = col_character(),
                          Lot.Number = col_character(),
                          Mill = col_character(),
                          ICO.Number = col_character(),
                          Company = col_character(),
                          Altitude = col_character(),
                          Region = col_character(),
                          Producer = col_character(),
                          Number.of.Bags = col_double(),
                          Bag.Weight = col_character(),
                          In.Country.Partner = col_character(),
                          Harvest.Year = col_character(),
                          Grading.Date = col_character(),
                          Owner.1 = col_character(),
                          Variety = col_character(),
                          Processing.Method = col_character(),
                          Fragrance...Aroma = col_double(),
                          Flavor = col_double(),
                          Aftertaste = col_double(),
                          Salt...Acid = col_double(),
                          Balance = col_double(),
                          Uniform.Cup = col_double(),
                          Clean.Cup = col_double(),
                          Bitter...Sweet = col_double(),
                          Cupper.Points = col_double(),
                          Total.Cup.Points = col_double(),
                          Moisture = col_double(),
                          Category.One.Defects = col_double(),
                          Quakers = col_double(),
                          Color = col_character(),
                          Category.Two.Defects = col_double(),
                          Expiration = col_character(),
                          Certification.Body = col_character(),
                          Certification.Address = col_character(),
                          Certification.Contact = col_character(),
                          unit_of_measurement = col_character(),
                          altitude_low_meters = col_double(),
                          altitude_high_meters = col_double(),
                          altitude_mean_meters = col_double()
                        )) %>% 
  janitor::clean_names() %>% 
  rename(acidity = salt_acid, sweetness = bitter_sweet,
         aroma = fragrance_aroma, body = mouthfeel,uniformity = uniform_cup)


all_ratings <- bind_rows(raw_arabica, raw_robusta) %>% 
  select(-x1) %>% 
  select(total_cup_points, species, everything())

install.packages("skimr")
library(skimr)

all_ratings %>% 
  skimr::skim()

all_ratings %>% 
  write_csv("2020/2020-07-07/coffee_ratings.csv")

install.packages("dataMaid")
library(dataMaid)
makeDataReport(coffee_ratings)

library(ggplot2)
ggplot(data=coffee_ratings, mapping = aes(x=coffee_ratings$flavor,y=coffee_ratings$aftertaste))+
  geom_point()
