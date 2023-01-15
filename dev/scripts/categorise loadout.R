
# categorise loadout ------------------------------------------------------

df_items <- df_ls$loadouts |>
  mutate(
    item_lower = tolower(item_detailed),
    item = case_when(
      str_detect(item_lower, "saw") ~ "Saw",
      str_detect(item_lower, "ax") ~ "Axe",
      str_detect(item_lower, "sleeping bag") ~ "Sleeping bag",
      str_detect(item_lower, "ferro rod") ~ "Ferro rod",
      str_detect(item_lower, "knife") ~ "Knife",
      str_detect(item_lower, "fishing line") ~ "Fishing gear",
      str_detect(item_lower, "multitool") ~ "Multitool",
      str_detect(item_lower, "rations") ~ "Rations",
      str_detect(item_lower, "trapping wire|snare wire|snaring wire") ~ "Trapping wire",
      str_detect(item_lower, "tarp") ~ "Tarp",
      str_detect(item_lower, "gill net|gillnet") ~ "Gill net",
      str_detect(item_lower, "pot") ~ "Pot",
      str_detect(item_lower, "hooks") ~ "Fishing gear",
      str_detect(item_lower, "bivy") ~ "Bivy bag",
      str_detect(item_lower, "paracord") ~ "Paracord",
      str_detect(item_lower, "bow and|arrows") ~ "Bow and arrows",
      str_detect(item_lower, "canteen") ~ "Canteen",
      str_detect(item_lower, "frying pan") ~ "Frying pan",
    )
  ) |>
  select(-item_lower)

write_sheet(
  ss = key,
  data = df_items,
  sheet = "loadouts"
)


# done --------------------------------------------------------------------

df_ls$loadouts |>
  count(item) |>
  ggplot(aes(item, n)) +
  geom_col()

df_ls$survivors |>
  count(reason_category) |>
  ggplot(aes(reason_category, n)) +
  geom_col()


df_ls$episodes |>
  ggplot(aes(episode_number_overall, viewers, colour = as.factor(season))) +
  geom_line()



library(rvest)

get_imdb_ratings <- function(.season) {
  # get url for season version
  url <- glue("https://www.imdb.com/title/tt4803766/episodes?season={.season}")

  # read page
  page <- read_html(url) |>
    html_elements(".info")

  # extract data
  rating <- page |>
    html_elements(".ipl-rating-star__rating") |>
    html_text()

  df_out <- map_dfr(1:length(page), function(.ep) {

    # title
    title <- page[.ep] |>
      html_nodes("a") |>
      html_attr("title") |>
      head(1)

    episode <- page[.ep] |>
      html_nodes("meta") |>
      html_attr("content")

    airdate <- page[.ep] |>
      html_element(".airdate") |>
      html_text() |>
      str_trim() |>
      dmy()

    rating <- page[.ep] |>
      html_elements(".ipl-rating-star__rating") |>
      html_text() |>
      head(1) |>
      as.numeric()

    n_ratings <- page[.ep] |>
      html_elements(".ipl-rating-star__total-votes") |>
      html_text() |>
      head(1) |>
      str_extract("[:digit:]+") |>
      as.numeric()

    tibble(
      season = .season,
      episode_title = title,
      episode = episode,
      episode_date = airdate,
      imdb_rating = rating,
      n_ratings = n_ratings
    )

  }) |>
    mutate(episode = as.numeric(episode))

  df_out
}

df <- map_dfr(1:9, ~get_imdb_ratings(.x))


df_eps <- df_ls$episodes |>
  left_join(
    df |>
      select(-episode_date, -episode_title),
    by = c("season", "episode")
  )


write_sheet(
  ss = key,
  data = df_eps,
  sheet = "episodes"
)
