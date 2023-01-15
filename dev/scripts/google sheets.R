
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





