

df_ls <-


# episode tables ----------------------------------------------------------


episodes_url <- "https://en.wikipedia.org/wiki/List_of_Alone_episodes#Season_2_(2016)_-_Vancouver_Island"
page <- read_html(episodes_url)

tbls <- page |>
  html_table()

# make table

make_table <- function(index) {
  df <- tbls[[index]] |>
    clean_names() |>
    set_names(c("episode_number_overall", "episode", "title", "air_date", "viewers_us", "x"))

  id <- seq(2, nrow(df), 2)

  quote_auth <- df$episode_number_overall[id] |>
    str_remove_all("Beginning quote: ") |>
    str_remove_all('\"')

  quote <- str_extract(quote_auth, ".*(?=\\-)") |>
    str_trim()

  auth <- str_extract(quote_auth, "(?<=\\-).*") |>
    str_trim()

  date <- df$air_date[id-1] |>
    str_extract("(?<=\\().*(?=\\))") |>
    ymd()

  title <- df$title[id-1] |>
    str_remove_all('\"')

  viewers <- df$viewers_us[id-1] |>
    str_sub(1, 5) |>
    as.numeric()

  eps_overall <- df$episode_number_overall[id-1] |>
    as.numeric()

  tibble(
    version = "US",
    season = index-1,
    episode_number_overall = eps_overall,
    episode = 1:length(eps_overall),
    title = title,
    air_date = date,
    viewers = viewers,
    quote = quote,
    author = auth
  )
}


df <- map_dfr(2:10, make_table)

df$date0 <- paste0("'", df$air_date)

export_workbook(df, sheet = "episodes", file = "temp.xlsx")
