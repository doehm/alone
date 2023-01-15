
library(tidyverse)
library(rvest)
library(glue)

# get survivor details ----------------------------------------------------

survivor_details <- function(survivor, season) {
  survivor_url <- tolower(str_replace_all(survivor, " ", "-"))
  url <- glue("https://www.history.com/shows/alone/cast/{survivor_url}")
  page <- read_html(url)

  x <- page |>
    html_element(".main-article") |>
    html_elements("p")

  # get load out
  start <- which(str_sub(as.character(x), 4, 5) == "1." | str_sub(as.character(x), 4, 5) == "1)")
  end <- which(str_sub(as.character(x), 4, 6) == "10." | str_sub(as.character(x), 4, 6) == "10)")

  if(length(start) == 0 & length(end) == 0) {
    end <- length(x)
    start <- end - 9
    loadout <- x[start:end] |>
      html_text()
  } else if(length(end) == 0) {
    loadout <- x[start] |>
      html_text() |>
      str_extract_all("(?<=\\.\\s|\\)\\s).*")
    loadout <- loadout[[1]]
  } else {
    loadout <- x[start:end] |>
      html_text() |>
      str_extract("(?<=\\.\\s|\\)\\s).*")
  }

  # get profession
  prof_id <- which(str_detect(as.character(x), "Profession:"))
  profession <- x[prof_id] |>
    html_text() |>
    str_remove("Profession: ")

  list(
    season = season,
    survivor = survivor,
    profession = profession,
    loadout = tibble(
      item_number = 1:length(loadout),
      item = loadout
    )
  )
}



# loopy loop --------------------------------------------------------------

survivors <- df_ls$survivors |>
  filter(season != 4) |>
  # mutate(
  #   url = ifelse(is.na(url), tolower(str_replace_all(name, " ", "-")), url),
  #   url = str_remove_all(url, "'"),
  #   url = ifelse(season == 5, paste0(url, "-redemption"), url),
  # ) |>
  select(season, name, url)

details_ls <- list()
for(k in 1:nrow(survivors)) {
  cat("Collecting: ", survivors$name[k], "\n")
  x <- snakecase::to_snake_case(survivors$name[k])
  details_ls[[x]] <- survivor_details(survivors$url[k], survivors$season[k])
}

survivor_details("ted-and-jim-baird")
survivor <- "ted and jim baird"

df_loadout <- imap_dfr(details_ls, ~{
  .x$loadout |>
    mutate(
      season = .x$season,
      name = snakecase::to_title_case(.y)
      ) |>
    select(season, name, item_number, item)
  })

write_sheet(df_loadout, ss = key, sheet = "loadouts")



# fixing city and state ---------------------------------------------------

df_survivors <- df_ls$survivors |>
  mutate(
    city = str_trim(map_chr(city_state, ~str_split(.x, ",")[[1]][1])),
    state = str_trim(map_chr(city_state, ~str_split(.x, ",")[[1]][2]))
  ) |>
  select(season, name, age, gender, city, state, everything(), city_state)

write_sheet(df_survivors, ss = key, sheet = "survivors")



# profession --------------------------------------------------------------

df_profession <- imap_dfr(details_ls, ~{
    tibble(
      season = .x$season,
      name = snakecase::to_title_case(.y),
      profession = .x$profession
    )
})
