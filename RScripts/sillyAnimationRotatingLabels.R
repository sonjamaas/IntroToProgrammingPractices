library(ggplot2) # requires 2.3.0
# install.packages("devtools")
# devtools::install_github("tidyverse/ggplot2")

library(purrr)

make_plot <- function(frame) {
  ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) +
    geom_point() +
    scale_color_brewer(
      palette = 2, type = "qual", name = "cyl",
      guide = guide_legend(
        direction = "horizontal",
        label.position = "bottom",
        keyheight = grid::unit(5, "pt")
      )
    ) +
    theme_bw() +
    theme(legend.text = element_text(angle = 8*frame, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(angle = 270 - 2*frame),
          axis.text.x = element_text(angle = -3*frame),
          axis.text.y = element_text(angle = 4*frame),
          axis.title.x = element_text(angle = frame),
          axis.title.y = element_text(angle = 90 - 2*frame))
}

# function for saving images
plot_save<-function(t = 1200){
  # add 50000 to index so images are in the right order (10 comes after 9)
  file_path = paste0("plot-", 50000+t, ".png")
  ggsave(filename=file_path, width=5, height=4, make_plot(t))
}

# make gif
install.packages("magick")
library(magick)
install.packages("magrittr")
library(gifski)

png_files <- list.files("C:/Users/sonja/OneDrive/Dokumente/EAGLE_Msc/Semester1/Intro_to_Programming/GitPractices/IntroToProgrammingPractices/pngFilesSillyAnimation", 
                        pattern = ".*png$", 
                        full.names = TRUE)
gifski(png_files, gif_file = "SillyAnimation.gif", width = 800, height = 600, delay = .05)
