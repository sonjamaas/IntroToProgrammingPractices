install.packages("tidyverse")
library(tidyverse)
library(magick)

# the speed of involute increases uniformly; in order to make the lengths between
# steps equal, we need to calculate the square root
resolution_of_involute <- 10000L
t <- sqrt(seq(0, resolution_of_involute) / resolution_of_involute)

coil_turns <- 17L
max_r <- coil_turns * 2 * pi

# waviness of the coil
wave_frequency <- 100
wave_height_base <- pi

# download and read the image
img_file <- tempfile(fileext = ".png")
download.file("https://hoxo-m.com/img/team/makiyama.png", destfile = img_file, mode = "wb")
img_orig <- image_read(img_file)

# convert to grayscale
img_bw <- img_orig %>% 
  image_convert(type = "grayscale") %>%
  image_modulate(brightness = 160) %>% 
  image_contrast(10)

# the width and height of the output
w <- 320
h <- 320

# the width and height of the original image
info <- image_info(img_bw)
w_orig <- info$width
h_orig <- info$height

# the width and height of the zoomed image
scale <- 30
w_big <- w_orig * scale
h_big <- h_orig * scale

# zoom image
img_bw_big <- image_resize(img_bw, sprintf("%fx%f", w_big, h_big))

# place the small image on the center of the big image
img <- image_composite(img_bw_big, img_bw,
                       offset = sprintf("%+f%+f",
                                        (w_big - w_orig)/2, (h_big - h_orig)/2))
img

draw_hoxom <- function(rotation = 0, zoom = 1) {
  
  # unwavy involute
  d <- tibble(
    radius = 2 * pi * t * coil_turns,
    phi = radius - rotation,
    .x = cos(phi) + radius * sin(phi),
    .y = sin(phi) - radius * cos(phi)
  )
  
  # crop and resize the image at the specified zoom level
  g <- sprintf("%fx%f%+f%+f",
               w_big / zoom,
               h_big / zoom,
               (w_big - w_big / zoom) / 2,
               (h_big - h_big / zoom) / 2)
  
  blackness <- img %>%
    image_crop(g) %>%
    image_resize(sprintf("%fx%f", w, h)) %>%
    image_data("gray")
  
  # calculate which pixel each point falls in
  x_idx <- as.integer(scales::rescale(d$.x, from = c(-max_r, max_r), to = c(1L, dim(blackness)[2])))
  y_idx <- as.integer(scales::rescale(d$.y, from = c(-max_r, max_r), to = c(dim(blackness)[3], 1L)))
  
  # determine the wave height based on the blackness
  wave_height <- (255 - as.numeric(blackness[cbind(1, x_idx, y_idx)])) / 256 * wave_height_base
  
  # wavy involute  
  d_wavy <- d %>% 
    mutate(
      x = .x + wave_height * sin(phi * wave_frequency) * sin(phi),
      y = .y - wave_height * sin(phi * wave_frequency) * cos(phi)
    )
  
  p <- ggplot(d_wavy) +
    geom_path(aes(x, y)) +
    theme_minimal() +
    coord_equal(
      # 0.85 is for zoom
      xlim = c(-max_r, max_r) * 0.85,
      ylim = c(-max_r, max_r) * 0.85
    ) +
    theme_void()
  
  print(p)
}


imgs <- image_graph(w, h, res = 72)

steps <- 100
for (i in seq_len(steps)) {
  message("step", i)
  
  # zoom level needs to be powered to make it looks the constant zoom speed.
  draw_hoxom(2 * pi * i / steps, 1 + (scale - 1) / steps^2 * i^2)
}
dev.off()

ani <- image_animate(imgs, fps = 50)
print(ani)