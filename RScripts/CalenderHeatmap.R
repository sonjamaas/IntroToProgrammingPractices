# install needed packages
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ragg")) install.packages("ragg")
if(!require("lubridate")) install.packages("lubridate")

# packages
library(tidyverse)
library(lubridate)
library(ragg)
library(ggplot2)

# import the data
dat_pr <- read_csv("C:/Users/sonja/Documents/Dokumente/Studium/Master/Intro_to_programming/GitPractice/GitPractices/Data/precipitation_santiago.csv")
dat_pr

dat_pr <- dat_pr %>% 
  complete(date = seq(ymd("2020-01-01"), 
                      ymd("2020-12-31"), 
                      "day")) %>%
  mutate(weekday = wday(date, label = T, week_start = 1), 
         month = month(date, label = T, abbr = F),
         week = isoweek(date),
         day = day(date))

# Add Dates
dat_pr <- mutate(dat_pr, 
                 week = case_when(month == "December" & week == 1 ~ 53,
                                  month == "January" & week %in% 52:53 ~ 0,
                                  TRUE ~ week),
                 pcat = cut(pr, c(-1, 0, .5, 1:5, 7, 9, 15, 20, 25, 30, 300)),
                 text_col = ifelse(pcat %in% c("(15,20]", "(20,25]", "(25,30]", "(30,300]"), 
                                   "white", "black")) 

dat_pr  

# color ramp
pubu <- RColorBrewer::brewer.pal(9, "PuBu")
col_p <- colorRampPalette(pubu)

theme_calendar <- function(){
  
  theme(aspect.ratio = 1/2,
        
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        
        legend.position = "top",
        legend.text = element_text(hjust = .5),
        legend.title = element_text(size = 9, hjust = 1),
        
        plot.caption =  element_text(hjust = 1, size = 8),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = .5, size = 26, 
                                  face = "bold", 
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(hjust = .5, size = 16)
  )
}

ggplot(dat_pr, aes(weekday, -week, fill = pcat)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = day, colour = text_col), size = 2.5) +
  guides(fill = guide_colorsteps(barwidth = 25, 
                                 barheight = .4,
                                 title.position = "top")) +
  scale_fill_manual(values = c("white", col_p(13)),
                    na.value = "grey90", drop = FALSE) +
  scale_colour_manual(values = c("black", "white"), guide = FALSE) + 
  facet_wrap(~ month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "How is 2020 being in Santiago?", 
       subtitle = "Precipitation",
       caption = "Data: Meteogalicia",
       fill = "mm") +
  theme_calendar()

