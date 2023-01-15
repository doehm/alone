
library(tidyverse)
library(janitor)
library(googledrive)
library(googlesheets4)

df_other <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/u/1/d/1u55ZMRbVcmHB-N_H0K5_kuPZul32hbMkxxKW1KoCVyo/htmlview",
  sheet = "Sheet1"
  )

df_profession <- df_other |>
  clean_names() |>
  mutate(name = paste(first, last)) |>
  select(season, name, profession_other = profession)

df_survivors <- df_ls$survivors |>
  left_join(df_profession, by = c("name", "season")) |>
  mutate(profession = coalesce(profession, profession_other)) |>
  select(-profession_other)

write_sheet(
  ss = key,
  data = df_survivors,
  sheet = "survivors"
)
