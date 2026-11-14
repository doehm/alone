# plots for scrolly

img <- "dev/analysis/AU03/images/cast.jpg"

image_read(img)

library(traceR)
library(tidyverse)
library(ggpubr)
library(magick)
library(showtext)
library(shadowtext)
library(glue)

font_add_google("Barlow Condensed", "jose", regular.wt = 300, bold.wt = 700)
showtext_auto()

ft <- "jose"

asp <- 1

# df <- trace_image(img)
#
# df |>
#   mutate(
#     x = powerglove::min_max(x, 0.07, 0.93),
#     y = powerglove::min_max(y, 0.63, 0.66)
#   ) |>
#   write_rds("dev/analysis/AU03/coords.rds")

df <- read_rds("dev/analysis/AU03/coords.rds") |>
  bind_cols(
    survivalists |>
      filter(
        version == "AU",
        season == 3
      ) |>
      slice(c(9, 10, 3, 1, 6, 5, 7, 4, 2, 8))
  ) |>
  mutate(
    x = powerglove::min_max(x, 0.08, 0.92),
    yend = min(y) + days_lasted/max(days_lasted)*0.2,
    x = ifelse(name == "Corinne", x-0.02, x)
  )

for(k in 0:10) {

  df |>
    filter(result >= 10-k+1) |>
    ggplot() +
    background_image(image_read(img)) +
    geom_segment(aes(x = x, xend = x, y = y+0.005, yend = yend), colour = "white", linewidth = 0.25, lineend = "round") +
    geom_text(aes(x, yend+0.07, label = name), family = ft, size = 20, colour = "white", fontface = "bold") +
    geom_text(aes(x, yend+0.025, label = glue("{days_lasted} days")), family = ft, size = 12, colour = "white") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()

  ggsave(glue("dev/analysis/AU03/images/cast-plots/cast-{k}.png"), height = asp*1366, width = asp*2048, units = "px")

}

dat <- survivalists |>
  filter(version == "AU") |>
  arrange(desc(days_lasted)) |>
  slice_head(n = 10)

for(k in 1:10) {
  num <- str_pad(k, pad = "0", width = 2)
  cat(glue("| {num}. {dat$name[k]} - {dat$days_lasted[k]} days"), "\n")
}

dat <- survivalists |>
  filter(version %in% c("AU", "US")) |>
  arrange(desc(days_lasted)) |>
  mutate(rank = n()-rank(days_lasted, ties.method = "max")+1) |>
  filter(rank <= 20)

for(k in 1:nrow(dat)) {
  num <- str_pad(dat$rank[k], pad = "0", width = 2)
  cat(glue("| {num}.\t{dat$name[k]} (S{dat$season[k]}) - {dat$days_lasted[k]} days"), "\n")
}
