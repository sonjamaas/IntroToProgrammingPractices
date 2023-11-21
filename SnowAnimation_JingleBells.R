#Create falling Snow Animation
# PACKAGES ####
pkg <- c("here", "tidyverse", "gganimate", "animation")
sapply(pkg, function(x){
  if (!x %in% installed.packages()){install.packages(x)}
  library(x, character.only = TRUE)
})

# CUSTOM FUNCTIONS ####
map_to_range <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}

# CONSTANTS ####
N <- 500 # number of flakes
TIMES <- 100 # number of loops
XPOS_DELTA <- 0.01
YSPEED_MIN = 0.005
YSPEED_MAX = 0.03
FLAKE_SIZE_COINFLIP = 5
FLAKE_SIZE_COINFLIP_PROB = 0.1
FLAKE_SIZE_MIN = 4
FLAKE_SIZE_MAX = 20

# INITIALIZE DATA ####
set.seed(1)

size <- runif(N) + rbinom(N, FLAKE_SIZE_COINFLIP, FLAKE_SIZE_COINFLIP_PROB) # random flake size
yspeed <- map_to_range(size, YSPEED_MIN, YSPEED_MAX)

# create storage vectors
xpos <- rep(NA, N * TIMES)
ypos <- rep(NA, N * TIMES)

# loop through simulations
for(i in seq(TIMES)){
  if(i == 1){
    # initiate values
    xpos[1:N] <- runif(N, min = -0.1, max = 1.1)
    ypos[1:N] <- runif(N, min = 1.1, max = 2)
  } else {
    # specify datapoints to update
    first_obs <- (N * i - N + 1)
    last_obs <- (N * i)
    # update x position 
    # random shift
    xpos[first_obs:last_obs] <- xpos[(first_obs-N):(last_obs-N)] - runif(N, min = -XPOS_DELTA, max = XPOS_DELTA)
    # update y position
    # lower by yspeed
    ypos[first_obs:last_obs] <- ypos[(first_obs-N):(last_obs-N)] - yspeed
    # reset if passed bottom screen
    xpos <- ifelse(ypos < -0.1, runif(N), xpos) # restart at random x
    ypos <- ifelse(ypos < -0.1, 1.1, ypos) # restart just above top
  }
}


# VISUALIZE DATA ####
cbind.data.frame(ID = rep(1:N, TIMES)
                 ,x = xpos
                 ,y = ypos 
                 ,s = size
                 ,t = rep(1:TIMES, each = N)) %>%
  # create animation
  ggplot() +
  geom_point(aes(x, y, size = s, alpha = s), color = "white", pch = 42) +
  scale_size_continuous(range = c(FLAKE_SIZE_MIN, FLAKE_SIZE_MAX)) +
  scale_alpha_continuous(range = c(0.2, 0.8)) +
  coord_cartesian(c(0, 1), c(0, 1)) +
  theme_void() +
  theme(legend.position = "none", 
        panel.background = element_rect("black")) +
  transition_time(t) +
  ease_aes('linear') ->
  snow_plot

snow_anim <- animate(snow_plot, nframes = TIMES, width = 600, height = 600)
snow_anim


#Play Jingle Bells Tune
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)