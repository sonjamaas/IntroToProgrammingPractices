# EAGLE -> EARTH Observation Animation
# based on script by https://gist.github.com/emitanaka/ef1d1b6ade5e57acc4734bc4bfbcc0cd
# adapted to EAGLE MSc. program and Earth Observation by Martin Wegmann

library(readbitmap)
library(shiny)
library(gganimate)
library(dplyr)
library(ggplot2)


png("text1.png")
plot.new()
text(x=0.5,y=0.5,"Hello there",cex=3)
dev.off()

png("text2.png")
plot.new()
text(x=0.5,y=0.5,"General Kenobi?!",cex=3)
dev.off()

# import text files (black text, white background)
# b1 <- read.bitmap("~/eagle.png")
b1 <- read.bitmap("text1.png")
# b2 <- read.bitmap("~/eagle_EO.png")
#b2 <- read.bitmap("eo.png")
b2 <- read.bitmap("text2.png")

# adapt imported files to suit ggplot2 requirements
x1 <- 1 - b1[,,1]
x2 <- 1 - b2[,,1]

df_text1 <- data.frame(row=as.vector(col(x1)), 
                       col=8 - as.vector(row(x1)),
                       value=as.vector(x1)) %>% 
  filter(value==1)


df_text2 <- data.frame(row=as.vector(col(x2)), 
                    col=8 - as.vector(row(x2)),
                    value=as.vector(x2)) %>% 
  filter(value==1) 
plot_df <- bind_rows(
  df_text1%>% mutate(idx=1),
  df_text2 %>% mutate(idx=2)
)

# create the plot with the two text parts
p <- ggplot(plot_df, aes(row, col)) + geom_tile(fill="hotpink", width=1, height=1) + coord_equal() + xlab("") + ylab("") + theme(axis.line = element_line(colour = "black"),
                                                                                                                              panel.grid.major = element_blank(),
                                                                                                                              panel.grid.minor = element_blank(),
                                                                                                                              panel.border = element_blank(),
                                                                                                                              panel.background = element_blank())  + theme_void()
# check plot - should show overlapping text
p  

# create the actual animation
text_anim <- p +
  transition_states(
    states            = idx, # variable in data
    transition_length = 1,   # all states display for 1 time unit
    state_length      = .5    # all transitions take 1 time unit
  ) +
  enter_fade() +             # How new blocks appear
  exit_fade() +              # How blocks disappear
  ease_aes('sine-in-out')    # Tweening movement

text_anim

# save the animation with specific frames, size etc.
save_animation(animate(text_anim, fps=20, nframes=50, width=2800, height=400), "hello.gif")
