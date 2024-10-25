## Create Hex
library(tidyverse)
library(scales)
library(cropcircles)


## CREATE STRIPES
fi <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

dat <- read_csv(file = fi, skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()


gg <- ggplot(data = dat, aes(x = year, y = 1, fill = t_diff)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_stepsn(
    colors = c("#08306B", "white", "#67000D"),
    values = rescale(c(min(dat$t_diff), 0, max(dat$t_diff))),
    n.breaks = 12
  ) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = seq(1890, 2020, 30)) +
  theme_void()
ggsave("data-raw/stripes.svg", plot = gg, dpi = 1000)


## CROP IMAGE TO HEXAGON
img_cropped <- hex_crop(
  images = "data-raw/stripes.svg",
  border_colour = "black",
  border_size = 0,
  to = "data-raw/stripes_cropped.png"
)


## CREATE STICKER
hs <- hexSticker::sticker("data-raw/stripes_cropped.png",
  package = "hotrstuff",
  p_y = 1,
  p_x = 1,
  p_color = "black",
  p_size = 100,
  p_fontface = "bold",
  s_x = 1,
  s_y = 1,
  s_width = 0.835,
  s_height = 0.835,
  # h_fill = "#9FE2BF",
  h_color = "black", # "grey40",
  dpi = 1000,
  asp = 1,
  filename = file.path("data-raw", "logo.png")
  # filename = file.path("man", "figures", "logo.png")
)
