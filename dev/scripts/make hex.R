
library(tidyverse)
library(ggpath)
library(magick)
library(cropcircles)

# IMAGES ------------------------------------------------------------------

x <- "https://cropper.watch.aetnd.com/cdn.watch.aetnd.com/sites/2/2016/11/Alone_Season3_Daniel_Wowak_Bio.jpg?w=840"
image_read(cropcircles::circle_crop(x))

cropcircles::hex_crop("dev/images/square.jpg", to = "dev/images/hex1 crop.png")

ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey40")
  )

ggsave("dev/images/bg.png", width = 6, height = 6)
cropcircles::hex_crop("dev/images/bg.png", to = "dev/images/bg hex.png")

ggsave("dev/images/bg white.png", width = 6, height = 6)
cropcircles::hex_crop("dev/images/bg white.png", to = "dev/images/bg white hex.png")

x1 <- -0.8662
x2 <- 0.8662
y1 <- 1
y2 <- -1
a <- 2*0.1338

ggplot() +
  geom_from_path(aes(0, 0, path = "dev/images/bg hex.png"), width = 1) +
  geom_from_path(aes(0, 0, path = "dev/images/bg white hex.png"), width = 0.99) +
  geom_from_path(aes(0, 0, path = "dev/images/hex1 crop.png"), width = 0.95) +
  coord_fixed() +
  xlim(x1, x2) +
  ylim(y1, y2) +
  theme_void()

ggsave("dev/images/alone hex.png", width = 12, height = 12)
