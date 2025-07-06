
make_surv_data <- function(df0) {

  df <- survivalists |>
    filter(
      !(season == 4 & version == "US"),
      version != "US Frozen"
    ) |>
    mutate(
      winner = as.numeric(!is.na(reason_tapped_out)),
      censored = case_when(
        is.na(reason_tapped_out) ~ 0,
        name == "Nicole Apelian" & season == 5 ~ 0,
        TRUE ~ 1
      ),
      days_lasted0 = ifelse(days_lasted == 0, 1, days_lasted)
    ) |>
    inner_join(
      df0 |>
        select(season, id, grp),
      join_by(season, id)
    )

  df_grid <- df |>
    group_by(grp) |>
    summarise(max_days = max(days_lasted0))

  map_dfr(1:nrow(df_grid), \(k){
    tibble(
      days_lasted = 0:df_grid$max_days[k],
      grp = df_grid$grp[k]
    )
  }) |>
    left_join(
      df |>
        filter(winner == 1) |>
        count(days_lasted, grp),
      join_by(days_lasted, grp)
    ) |>
    left_join(
      df |>
        count(grp, name = "N"),
      join_by(grp)
    ) |>
    group_by(grp) |>
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
    arrange(grp, days_lasted) |>
    left_join(
      df |>
        filter(censored == 0) |>
        select(days_lasted, grp, censored),
      join_by(days_lasted, grp)
    ) |>
    ungroup()

}


