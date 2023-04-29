# 🤯 header ------------------------------------------------------------------

# Full code for blog post

# 📚 libraries ------------------------------------------------------------

library(tidyverse)
library(survival)
library(ggbump)
library(rstan)
library(showtext)
library(ggchicklet)

# 🎨 fonts and palettes ---------------------------------------------------

pal <- c('#1A2523', '#373D37', '#5B5C5B', '#808182', '#B9B8B7', '#E8E8EA')
pal_gender <- c(Female = "#CD5555", Male = "#2F4F4F")
txt <- "grey20"
bg <- "white"
line <- "grey85"

font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle --------------------------------------------------------------

df <- survivalists |>
  filter(
    season != 4,
    version == "US"
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
    gender = factor(gender)
  )

df_surv <- expand_grid(
  days_lasted = 0:max(df$days_lasted),
  gender = unique(df$gender)
) |>
  left_join(
    df |>
      filter(winner == 1) |>
      count(days_lasted, gender),
    by = c("days_lasted", "gender")
  ) |>
  left_join(
    df |>
      count(gender, name = "N"),
    by = "gender"
  ) |>
  group_by(gender) |>
  mutate(
    n = replace_na(n, 0),
    n_alive = N-cumsum(n),
    p = n_alive/N,
    n_tapped = cumsum(n),
    pt = (n_alive-n)/n_alive,
    st = cumprod(pt),
    lambda = n_tapped/days_lasted,
    x = n/(n_alive*(n_alive-n)),
    sd = st*sqrt(cumsum(x)),
    st = ifelse(is.na(st) & gender == "Female", 0, st)
  ) |>
  arrange(gender, days_lasted)

df_censored <- df |>
  filter(censored == 0) |>
  left_join(df_surv, by = c("days_lasted", "gender")) |>
  select(days_lasted, gender, st)


# 🧪 inference ----------------------------------------------------------

# runs in stan
inference_tests <- function(reason) {
  dfx <- switch(
    reason,
    "overall" = df,
    "health" = df |>
      filter(reason_category == "Health" & !medically_evacuated),
    "personal" = df |>
      filter(reason_category == "Personal"),
    "medevac" = df |>
      filter(medically_evacuated)
  )

  # data
  dat_reason <- list(
    N_m = sum(dfx$gender == "Male"),
    N_f = sum(dfx$gender == "Female"),
    df = t.test(days_lasted ~ gender, data = dfx)$parameter,
    days_m = dfx$days_lasted[dfx$gender == "Male"],
    days_f = dfx$days_lasted[dfx$gender == "Female"]
  )

  # model
  mod_reason <- stan(file = "dev/scripts/alone-survival-analysis/reason.stan", data = dat_reason)
  pars <- extract(mod_reason, pars = c("mu_days_m", "mu_days_f"))
  d <- pars$mu_days_f - pars$mu_days_m
  list(
    d = d,
    reason = reason,
    p = sum(d > 0)/length(d),
    p_val = 2*(1-pnorm(mean(d)/sd(d))),
    ci = quantile(d, c(0.025, 0.1, 0.5, 0.9, 0.975)),
    mu_post = tibble(
      gender = c("Male", "Female"),
      mu_post = c(median(pars$mu_days_m), median(pars$mu_days_f))
    ),
    t_test = t.test(days_lasted ~ gender, data = dfx)
  )
}

test_overall <- inference_tests("overall")
test_pers <- inference_tests("personal")
test_health <- inference_tests("health")
test_medievac <- inference_tests("medevac")

# 1.1 contestant breakdown ------------------------------------------------

survivalists |>
  count(season, gender) |>
  pivot_wider(names_from = gender, values_from = n)

df |>
  filter(season != 1) |>
  count(result, gender)

df_mean <- df |>
  group_by(gender) |>
  summarise(mean = mean(days_lasted)) |>
  mutate(y = 1:2) |>
  left_join(test_overall$mu_post, by = "gender")

df |>
  ggplot() +
  geom_jitter(aes(days_lasted, gender, colour = gender), df, size = 5, alpha = 0.5, height = 0.1) +

  geom_rect(aes(xmin = mu_post, xmax = mean, ymin = y-0.15, ymax = y+0.15, fill = gender), df_mean, alpha = 0.2) +

  geom_segment(aes(x = mean, xend = mean, y = y-0.2, yend = y+0.2, colour = gender), df_mean, size = 2) +
  geom_text(aes(mean, y + 0.3, label = round(mean, 1), colour = gender), df_mean, family = ft, size = 12, lineheight = 0.3) +

  geom_segment(aes(x = mu_post, xend = mu_post, y = y-0.2, yend = y+0.2, colour = gender), df_mean, size = 1, linetype = 2) +
  geom_text(aes(mu_post-1, y - 0.3, label = paste(round(mu_post, 1), "\nPosterior mean"), colour = gender), df_mean, family = ft, size = 8, lineheight = 0.3, hjust = 0) +

  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  labs(
    title = "Mean number of days lasted",
    subtitle = "Women on Alone have survived 12 days longer than men. The winners would
need to survive 2.2x longer to balance the means.",
    colour = "Gender",
    fill = "Gender",
    x = "Days",
    caption = "Includes bayes adjusted mean for perspective. Prior on the mean assumed to be ~N(39, 14)."
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )


# 1.2 📊 results ----------------------------------------------------------

df |>
  filter(season != 1) |>
  count(result, gender) |>
  ggplot(aes(-result, n, fill = gender)) +
  geom_chicklet(radius = grid::unit(6, "pt")) +
  scale_fill_manual(values = pal_gender) +
  scale_x_continuous(breaks = -(1:10), labels = scales::ordinal(1:10)) +
  labs(
    title = "Season results",
    subtitle = "4 women have finished second out of 7 seasons, excluding season 1 which was an all male season.
2 out of the 4 women were pulled from the game for medical reasons.",
    x = "Result",
    fill = "Gender",
    caption = "Season 4 is removed due to the change in format and survivalists competing in pairs.
Season 1 is removed due to being an all male season."
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )


# 2 🚿 reasons for tapping out --------------------------------------------

# 2.1 👪  personal --------------------------------------------------------

for_blog <- TRUE

df_mean_fam <- df |>
  filter(reason_category == "Personal") |>
  group_by(gender) |>
  summarise(mean = mean(days_lasted)) |>
  mutate(y = 1:2) |>
  left_join(test_pers$mu_post, by = "gender")

df |>
  ggplot() +
  geom_jitter(aes(days_lasted, gender), filter(df, reason_category != "Personal" | is.na(reason_category)), size = 5, alpha = 0.5, height = 0.1, colour = "grey50") +
  geom_jitter(aes(days_lasted, gender, colour = gender), filter(df, reason_category == "Personal"), size = 5, alpha = 0.9, height = 0.1) +

  geom_rect(aes(xmin = mu_post, xmax = mean, ymin = y-0.15, ymax = y+0.15, fill = gender), df_mean_fam, alpha = 0.2) +

  geom_segment(aes(x = mean, xend = mean, y = y-0.2, yend = y+0.2, colour = gender), df_mean_fam, size = 2) +
  geom_text(aes(mean, y + 0.3, label = round(mean, 1), colour = gender), df_mean_fam, family = ft, size = 12, lineheight = 0.3) +

  geom_segment(aes(x = mu_post, xend = mu_post, y = y-0.2, yend = y+0.2, colour = gender), df_mean_fam, size = 1, linetype = 2) +
  geom_text(aes(mu_post-1, y - 0.3, label = paste(round(mu_post, 1), "\nPosterior mean"), colour = gender), df_mean_fam, family = ft, size = 8, lineheight = 0.3, hjust = 0) +

  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  labs(
    #     title = "Tapped out for personal reasons",
    #     subtitle = "There have been 4 women (21%) and 24 men (39%) that have tapped out due
    # to personal reasons such as missing their family.",
    colour = "Gender",
    fill = "Gender",
    x = "Days",
    caption = "Personal reasons include anything that isn't medical / health related.
Prior on the mean assumed to be ~N(39, 14)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )


# 2.2 🏥 health -----------------------------------------------------

df_mean_med <- df |>
  filter(
    reason_category == "Health",
    !medically_evacuated
  ) |>
  group_by(gender) |>
  summarise(mean = mean(days_lasted)) |>
  mutate(y = 1:2) |>
  left_join(test_health$mu_post, by = "gender")

df |>
  ggplot() +

  geom_jitter(aes(days_lasted, gender), filter(df, reason_category != "Health" | medically_evacuated | is.na(reason_category)), size = 5, alpha = 0.5, height = 0.1, colour = "grey50") +
  geom_jitter(aes(days_lasted, gender, colour = gender), filter(df, reason_category == "Health", !medically_evacuated), size = 5, alpha = 0.9, height = 0.1) +

  geom_rect(aes(xmin = mu_post, xmax = mean, ymin = y-0.15, ymax = y+0.15, fill = gender), df_mean_med, alpha = 0.2) +

  geom_segment(aes(x = mean, xend = mean, y = y-0.2, yend = y+0.2, colour = gender), df_mean_med, size = 2) +
  geom_text(aes(mean, y + 0.3, label = round(mean, 1), colour = gender), df_mean_med, family = ft, size = 12, lineheight = 0.3) +

  geom_segment(aes(x = mu_post, xend = mu_post, y = y-0.2, yend = y+0.2, colour = gender), df_mean_med, size = 1, linetype = 2) +
  geom_text(aes(mu_post-1, y - 0.3, label = paste(round(mu_post, 1), "\nPosterior mean"), colour = gender), df_mean_med, family = ft, size = 8, lineheight = 0.3, hjust = 0) +

  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  labs(
        title = "Tapped out for health reasons",
        subtitle = "4 out of 5 women that tapped out due to medical or health reasons
    survived at least 73 days",
    caption = "This excludes those that were removed due to a medical assessment and subsequently evacuated.
Prior on the mean assumed to be ~N(39, 14)",
    colour = "Gender",
    fill = "Gender",
    x = "Days"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )

# 2.3 🏥 medical evacuations -----------------------------------------------------

df_mean_evac <- df |>
  filter(medically_evacuated) |>
  group_by(gender) |>
  summarise(mean = mean(days_lasted)) |>
  mutate(y = 1:2) |>
  left_join(test_medievac$mu_post, by = "gender")

df |>
  ggplot() +
  geom_jitter(aes(days_lasted, gender), filter(df, !medically_evacuated), size = 5, alpha = 0.5, height = 0.1, colour = "grey50") +
  geom_jitter(aes(days_lasted, gender, colour = gender), filter(df, medically_evacuated), size = 5, alpha = 0.9, height = 0.1) +

  geom_rect(aes(xmin = mu_post, xmax = mean, ymin = y-0.15, ymax = y+0.15, fill = gender), df_mean_evac, alpha = 0.2) +

  geom_segment(aes(x = mean, xend = mean, y = y-0.2, yend = y+0.2, colour = gender), df_mean_evac, size = 2) +
  geom_text(aes(mean, y + 0.3, label = round(mean, 1), colour = gender), df_mean_evac, family = ft, size = 12, lineheight = 0.3) +

  geom_segment(aes(x = mu_post, xend = mu_post, y = y-0.2, yend = y+0.2, colour = gender), df_mean_evac, size = 1, linetype = 2) +
  geom_text(aes(mu_post-1, y - 0.3, label = paste(round(mu_post, 1), "\nPosterior mean"), colour = gender), df_mean_evac, family = ft, size = 8, lineheight = 0.3, hjust = 0) +

  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  labs(
    title = "Medical Evacuations",
    subtitle = "11 men (18%) and 10 women (53%) have been medically evacuated due to injury, starvation or other serious
medical concerns that were deemed too risky to allow them to remain.",
    caption = "Prior on the mean assumed to be ~N(39, 14)",
    colour = "Gender",
    fill = "Gender",
    x = "Days"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )


# 3.1 📉 kaplan-meier survival curves -------------------------------------

df_surv |>
  ggplot(aes(days_lasted, st, colour = gender)) +
  geom_bump(size = 0.5) +
  geom_ribbon(aes(x = days_lasted, ymin = pmax(st-1.96*sd, 0), ymax = pmin(st+1.96*sd, 1), fill = gender), alpha = 0.2, colour = NA) +
  geom_point(aes(days_lasted, st), df_censored, pch = 15, size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  labs(
    title = "Survival Curves",
    subtitle = "There is some evidence that women tend to survive longer than men on average.
However, there is a higher rate of tap outs by women from day 70",
    caption = "Season winners are right censored as we don't know how long they may have survived",
    x = "Days",
    colour = "Gender",
    fill = "Gender"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.text.y = element_text(hjust= 1),
    panel.grid = element_line(colour = line, linetype = 3)
  )


# 3.2 🪵 log rank test ----------------------------------------------------

survdiff(Surv(days_lasted, event = censored) ~ gender, df)


# 3.3 🪵🔔 log-normal survival curves -------------------------------------

# not the best way to do it but the way I have done it

# set data
cens_dat <- list(
  N_m = sum(df$censored == 1 & df$gender == "Male"),
  N_m_cens = sum(df$censored == 0 & df$gender == "Male"),
  N_f = sum(df$censored == 1 & df$gender == "Female"),
  N_f_cens = sum(df$censored == 0 & df$gender == "Female"),
  days_m = df$days_lasted0[df$censored == 1 & df$gender == "Male"],
  days_m_cens = df$days_lasted0[df$censored == 0 & df$gender == "Male"],
  days_f = df$days_lasted0[df$censored == 1 & df$gender == "Female"],
  days_f_cens = df$days_lasted0[df$censored == 0 & df$gender == "Female"]
)

# run model
mod_cens <- stan("dev/code/lognormal-censored.stan", data = cens_dat)

# extract parameters
pars <- extract(mod_cens, pars = c("mu_m", "mu_f", "sigma_m", "sigma_f"))

# take draws and evaluate uncertainty
df_sim <- map_dfr(c("m", "f"), ~{
  mu_x <- pars[[glue("mu_{.x}")]]
  sigma_x <- pars[[glue("sigma_{.x}")]]
  map_dfr(0:100, function(day) {
    z <- 1-plnorm(day, mu_x, sigma_x)
    tibble(
      gender = .x,
      days_lasted = day,
      St_2_5 = quantile(z, 0.025),
      St_50 = quantile(z, 0.50),
      St_97_5 = quantile(z, 0.975)
    )
  })
}) |>
  mutate(gender = ifelse(gender == "f", "Female", "Male"))

# plot
df_surv |>
  left_join(df_sim, by = c("days_lasted", "gender")) |>
  ggplot() +
  geom_line(aes(days_lasted, st, colour = gender), linetype = 2) +
  geom_point(aes(days_lasted, st, colour = gender), df_censored, pch = 15, size = 3) +
  geom_ribbon(aes(days_lasted, ymin = St_2_5, ymax = St_97_5, fill = gender), alpha = 0.25) +
  geom_line(aes(days_lasted, St_50, colour = gender), linewidth = 0.8) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = scales::percent(seq(0, 1, 0.25))) +
  scale_x_continuous(breaks = seq(0, 100, 10), labels = seq(0, 100, 10)) +
  scale_colour_manual(values = pal_gender) +
  scale_fill_manual(values = pal_gender) +
  theme_minimal() +
  labs(
    title = "Survival Curves",
    subtitle = "Parametric survival curves assuming a log-normal distibution.",
    colour = "Gender",
    fill = "Gender",
    x = "Days",
    caption = "Error bands are 95% credible interval estimate from Bayesian model in Stan."
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 32),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 56, face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 5, t = 5), lineheight = 0.3),
    plot.caption = element_text(margin = margin(t = 10), hjust = 0, size = 24, lineheight = 0.3),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    panel.grid = element_line(colour = line, linetype = 3)
  )

# 4 🚺 probability of a female winner ----------------------------------------

winner_days <- rep(NA, 40000)
sim_winner <- function(n_male, n_female, N) {
  gender <- c(rep("Male", n_male), rep("Female", n_female))
  winner <- map_chr(1:N, ~{
    y <- c(
      sample(pred_y$y_m, n_male),
      sample(pred_y$y_f, n_female)
    )
    winner_days[.x] <<- max(y)
    gender[which.max(y)]
  })

  table(winner)/N
}

# balanced
sim_winner(5, 5, 40000)

# the best we've seen
sim_winner(7, 3, 40000)
