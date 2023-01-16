
library(googledrive)
library(googlesheets4)
library(janitor)

# get data from google sheets ---------------------------------------------

read_gs_data <- function() {
  key <- read_rds("keys/alone.rds")
  tbls <- c("survivors", "episodes", "seasons", "loadouts")
  map(
    tbls,
    ~{
      read_sheet(ss = key, sheet = .x) |>
        clean_names()
    }
  ) |>
    set_names(tbls)
}

df_ls <- read_gs_data()

survivors <- df_ls$survivors
episodes <- df_ls$episodes
seasons <- df_ls$seasons
loadouts <- df_ls$loadouts

save(survivalists, file = "data/survivalists.rda")
save(episodes, file = "data/episodes.rda")
save(seasons, file = "data/seasons.rda")
save(loadouts, file = "data/loadouts.rda")
