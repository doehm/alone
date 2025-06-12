

# AU plots ----------------------------------------------------------------

library(ggtext)

# 🤼 wrangle --------------------------------------------------------------

df <- survivalists |>
  filter(
    version == "AU"
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
    )
  )

df_N <- df |>
  count(season, name = "N")

df_surv <- expand_grid(
  days_lasted = 0:max(df$days_lasted),
  season = unique(df$season)
) |>
  left_join(
    df |>
      filter(winner == 1) |>
      count(days_lasted, season),
    join_by(days_lasted, season)
  ) |>
  left_join(
    df_N,
    join_by(season)
  ) |>
  group_by(season) |>
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
  arrange(days_lasted)

df_censored <- df |>
  filter(censored == 0) |>
  left_join(df_surv, join_by(days_lasted, season)) |>
  select(days_lasted, st, season)

# 📊 plot -----------------------------------------------------------------

font_add_google("Barlow", "barlow")
ft <- "barlow"
txt <- "grey20"
bg <- "white"
accent <- "grey20"

caption <- "<span style='font-family:fa-brands; color:grey20'>&#xf09b;</span><span style='color:white'>-</span>doehm<span style='color:white'>-</span><span style='font-family:fa-brands; color:grey20'>&#xe671;</span><span style='color:white'>-</span>@danoehm.bsky.social"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

showtext_auto()

df_surv |>
  ggplot() +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3, colour = "grey") +
  geom_line(aes(days_lasted, st, group = season), colour = "grey") +
  geom_line(
    aes(days_lasted, st, group = season),
    df_surv |>
      filter(
        season == 3,
        days_lasted <= 84
      ),
    colour = "darkblue",
    linewidth = 2
  ) +
  geom_point(
    aes(days_lasted, st),
    df_censored |>
      filter(season == 3),
    pch = 15, size = 3, colour = "darkblue") +
  geom_richtext(
    aes(days_lasted, st, label = first_name),
    df |>
      filter(season == 3) |>
      left_join(
        df_surv |>
          filter(season == 3) |>
          mutate(st = lag(st)) |>
          select(days_lasted, st),
        join_by(days_lasted)
      ) |>
      mutate(
        # days_lasted = ifelse(first_name == "Timber", 80, days_lasted),
        # days_lasted = ifelse(first_name == "William", 88, days_lasted)
      ),
    family = ft, colour = "white", fill = "darkblue", label.colour = NA, size = 8, fontface = "italic"
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent(seq(0, 1, 0.1)), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +

  theme_minimal() +
  labs(
    title = "Alone Australia Season 3 Survival Curve",
    subtitle = "Season 3 was a record breaking season \n- the longest time until first tap at 16 days and\n- longest time survived at 76 days.",
    x = "Days",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_markdown(margin = margin(t = 10), hjust = 0.5, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )

ggsave("dev/images/AU03/survival-curve.png", width = 8, height = 6)
