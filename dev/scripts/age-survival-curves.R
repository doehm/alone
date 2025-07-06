

# notes ----------------------------------------------------------------------

# survival function
# S(t) = 1-exp(-lambda*t)
# f(t) = lambda*exp(-lambda*t) - exponential model


# 📚 libraries ------------------------------------------------------------

library(bayesplot)
library(survival)
library(survminer)
library(ggbump)
library(coxme)
library(tidyverse)
library(rstanarm)
library(rstan)
library(showtext)
library(ggchicklet)
library(brms)
library(tidyverse)

# 🎨 fonts and palettes ---------------------------------------------------

pal <- c('#1A2523', '#373D37', '#5B5C5B', '#808182', '#B9B8B7', '#E8E8EA')
pal <- c('#5C0A98', '#7C338C', '#9D5C81', '#BD8575', '#DEAE6A', '#FFD75F', '#CBDB72', '#98DF86', '#65E39A', '#32E7AE', '#00ECC2')
lakes <- c("#788FCE", "#e07a5f", "#8854B6", "#f2cc8f", "#81b29a", "#f4f1de", "#3d405b")
pal <- lakes[c(1, 2, 3, 5)]
txt <- "grey20"
bg <- "white"
line <- "grey85"

font_add_google("Barlow", "bar", regular.wt = 200, bold.wt = 600)
showtext_auto()

ft <- "bar"

# 🤼 wrangle --------------------------------------------------------------

df <- survivalists |>
  filter(
    !(season == 4 & version == "US"),
    version != "US Frozen"
  ) |>
  mutate(
    season = as.factor(season),
    winner = as.numeric(!is.na(reason_tapped_out)),
    censored = case_when(
      is.na(reason_tapped_out) ~ 0,
      name == "Nicole Apelian" & season == 5 ~ 0,
      TRUE ~ 1
    ),
    reason_cat = case_when(
      reason_category == "Health" & !medically_evacuated ~ "Health",
      medically_evacuated ~ "Medically evacuated",
      TRUE ~ "Personal"
    ),
    days_lasted0 = ifelse(days_lasted == 0, 1, days_lasted),
    gender = factor(gender),
    age_grp = case_when(
      age < 30 ~ "18-29",
      age < 40 ~ "30-39",
      age < 50 ~ "40-49",
      TRUE ~ "50+"
    ),
    age_grp = factor(age_grp)
  )

df_grid <- df |>
  group_by(age_grp) |>
  summarise(max_days = max(days_lasted0))

df_surv <- map_dfr(1:nrow(df_grid), \(k){
  tibble(
    days_lasted = 0:df_grid$max_days[k],
    age_grp = df_grid$age_grp[k]
  )
  }) |>
  left_join(
    df |>
      filter(winner == 1) |>
      count(days_lasted, age_grp),
    join_by(days_lasted, age_grp)
  ) |>
  left_join(
    df |>
      count(age_grp, name = "N"),
    join_by(age_grp)
  ) |>
  group_by(age_grp) |>
  mutate(
    n = replace_na(n, 0),
    n_alive = N-cumsum(n),
    p = n_alive/N,
    n_tapped = cumsum(n),
    pt = (n_alive-n)/n_alive,
    st = cumprod(pt),
    lambda = n_tapped/days_lasted,
    x = n/(n_alive*(n_alive-n)),
    sd = st*sqrt(cumsum(x))
  ) |>
  arrange(age_grp, days_lasted)

df_censored <- df |>
  filter(censored == 0) |>
  left_join(df_surv, by = c("days_lasted", "age_grp")) |>
  select(days_lasted, age_grp, st)

# 📊 plot -----------------------------------------------------------------

df_surv |>
  ggplot(aes(days_lasted, st, colour = age_grp)) +
  geom_bump(size = 0.5) +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Survivalists aged 18-29 are more likely to tap early than
those over 50 - but they become similar from day 50",
    caption = "Season winners are right censored as we don't know how long they may have survived",
    x = "Days",
    colour = "Age Group",
    fill = "Age Group"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.text.y = element_text(hjust= 1),
    panel.grid = element_line(colour = line, linetype = 3),
    legend.position = "top"
  )

ggsave("dev/images/AU03/age-grp-survival-curves.png", height = 6, width = 8)


# 🤼 wrangle --------------------------------------------------------------

df_surv <- survivalists |>
  mutate(grp = factor(version)) |>
  make_surv_data()

df_censored <- df_surv |>
  filter(censored == 0) |>
  select(days_lasted, grp, st)

df_surv |>
  ggplot(aes(days_lasted, st, colour = grp)) +
  geom_bump(linewidth = 0.5) +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Survivalists tend to tap out, or are medically evacuated
earlier in Alone Australian compared to Alone US",
    caption = "Season winners are right censored as we don't know how long they may have survived",
    x = "Days",
    colour = "Version"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.text.y = element_text(hjust= 1),
    panel.grid = element_line(colour = line, linetype = 3),
    legend.position = "top"
  )

ggsave("dev/images/AU03/version-survival-curves.png", height = 6, width = 8)

# 🤼 wrangle --------------------------------------------------------------

df_surv <- survivalists |>
  mutate(grp = factor(gender)) |>
  make_surv_data()

df_censored <- df_surv |>
  filter(censored == 0) |>
  select(days_lasted, grp, st)

df_surv |>
  ggplot(aes(days_lasted, st, colour = grp)) +
  geom_bump(linewidth = 0.5) +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_colour_manual(values = pal[3:4]) +
  scale_fill_manual(values = pal[3:4]) +
  labs(
    title = "There's no difference in the Survival rate between men and women
despite there never being a female winner in the US series",
    caption = "Season winners are right censored as we don't know how long they may have survived",
    x = "Days",
    colour = "Gender"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.text.y = element_text(hjust= 1),
    panel.grid = element_line(colour = line, linetype = 3),
    legend.position = "top"
  )

ggsave("dev/images/AU03/gender-survival-curves.png", height = 6, width = 8)

# 🤼 reason --------------------------------------------------------------

df_surv <- survivalists |>
  mutate(grp = factor(reason_category)) |>
  filter(grp %in% c("Health", "Personal")) |>
  make_surv_data()

df_censored <- df_surv |>
  filter(censored == 0) |>
  select(days_lasted, grp, st)

df_surv |>
  ggplot(aes(days_lasted, st, colour = grp)) +
  geom_bump(linewidth = 0.5) +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Survivalists will tap out for personal reasons before
they tap out for health reasons",
    caption = "Season winners are right censored as we don't know how long they may have survived",
    x = "Days",
    colour = "Reason Category"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.text.y = element_text(hjust= 1),
    panel.grid = element_line(colour = line, linetype = 3),
    legend.position = "top"
  )

ggsave("dev/images/AU03/reason-survival-curves.png", height = 6, width = 8)




