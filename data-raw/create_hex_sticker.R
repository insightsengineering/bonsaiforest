library(hexSticker)
library(magick)
library(rsvg)
library(svglite)

green <- "lightgreen"
brown <- "brown"

out_path <- file.path(getwd(), "man/figures/logo-large")
out_file <- paste(out_path, "png", sep = ".")

x <- sticker(
  subplot = "data-raw/bonsai.svg",
  s_x = 1,
  s_y = 0.8,
  s_width = 0.8,
  s_height = 0.8,
  package = "bonsaiforest",
  url = "github.com/insightsengineering/bonsaiforest",
  u_size = 1.4,
  u_color = brown,
  p_x = 1.0,
  p_y = 1.4,
  p_color = brown,
  p_size = 6,
  p_family = "mono",
  h_fill = green,
  h_color = brown,
  h_size = 2,
  spotlight = FALSE,
  l_y = 1,
  l_x = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.3,
  dpi = 2000,
  white_around_sticker = TRUE,
  filename = out_file
)

# But therefore we now need to postprocess the image by transforming white
# to transparent background.
out_img <- magick::image_read(out_file)
out_img <- magick::image_transparent(out_img, color = "white")
magick::image_write(out_img, path = out_file)

usethis::use_logo(out_file)
