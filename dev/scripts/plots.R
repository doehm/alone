
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)

pal <- c("#231942", "#3D3364", "#584E87", "#7566A0", "#937CB6", "#AAA1BC",
  "#BDD0B7", "#BFE7B2", "#A6DCAE", "#8ACFAB", "#5FBFAB", "#35B0AB")

pal1 <- pal[c(2, 11)]

txt <- "grey20"
line <- "grey80"
bg <- "white"

font_add_google("Karla", "karla")
showtext_auto()
ft <- "karla"

df <- expand_grid(
  days_lasted = 0:max(survivalists$days_lasted),
  gender = unique(survivalists$gender)
) |>
  left_join(
    survivalists |>
      count(days_lasted, gender),
    by = c("days_lasted", "gender")
  ) |>
  left_join(
    survivalists |>
      count(gender, name = "N"),
    by = "gender"
  ) |>
  group_by(gender) |>
  mutate(
    n = replace_na(n, 0),
    n_lasted = N-cumsum(n),
    p = n_lasted/N
  )


g1 <- df |>
  ggplot(aes(days_lasted, p, colour = gender, fill = gender)) +
  geom_line() +
  # xlim(0, 160) +
  scale_colour_manual(values = pal1) +
  labs(
    x = "Days lasted",
    y = "Proportion remaining",
    colour = "Gender",
    fill = "Gender",
    title = "Survival curves",
    subtitle = "There is some evidence that, on average, women tend to survive longer than men"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 64, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 30, t = 10), lineheight = 0.3),
    plot.margin = margin(t = 50, b = 170, l = 50, r = 50),
    plot.caption = element_markdown(size = 36, hjust = 0.5, margin = margin(t = 20)),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.ticks = element_line(colour = line),
    axis.line = element_line(colour = line),
    panel.grid = element_line(colour = line, linetype = 3)
  )

g2 <- survivalists |>
  ggplot(aes(gender, days_lasted, colour = gender, fill = gender)) +
  geom_boxplot(alpha = 0.25) +
  geom_jitter(width = 0.2, pch = 1, size = 3) +
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = pal1) +
  labs(
    colour = "Gender",
    fill = "Gender"
  ) +
  coord_flip() +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "none"
  )

g1 +
  inset_element(g2, left = 0, right = 1, bottom = -0.7, top = -0.2)

ggsave("dev/images/boxplots.png", height = 8, width = 12)



# items -------------------------------------------------------------------

pal <- c('#1B2624', '#3C413A', '#626262', '#7A7A7B', '#929293', '#ACABAB', '#D2D2D2', '#EBEBED')

loadouts |>
  count(item) |>
  mutate(item = forcats::fct_reorder(item, n, max)) |>
  ggplot(aes(item, n)) +
  geom_col(fill = pal[1]) +
  geom_text(aes(item, n + 3, label = n), family = ft, size = 12, colour = txt) +
  coord_flip() +
  labs(
    title = "Most popular loadout items"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 24),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 72, face = "bold", margin = margin(b = 30, t = 20), hjust = 0.25),
    plot.subtitle = element_text(margin = margin(b = 30, t = 20), lineheight = 0.3),
    plot.margin = margin(t = 0, b = 20, l = 20, r = 70),
    plot.caption = element_markdown(size = 36, hjust = 0.5, margin = margin(t = 20)),
    axis.text.y = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10), hjust = 1),
    axis.text.x = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.ticks = element_line(colour = line),
    axis.line = element_line(colour = line),
    panel.grid = element_line(colour = line, linetype = 3)
  )

ggsave("dev/images/items.png", height = 8, width = 8)

