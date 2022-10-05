
library(tidyverse)
library(lubridate)


## Data from https://www.kaggle.com/datasets/yujisato/spotify-us-weekly-top200
spotify_long <- read_csv("spotify_us_weekly_top200.csv")

spotify_long <- spotify_long |>
  mutate(
    date = mdy(date),
    week = week(date)
  ) |>
  filter(
    year(date) == 2020,
    Position <= 50
  )  

spotify <- spotify_long |>
  select(`Track Name`, Artist, week, Position) |> 
  pivot_wider(
    names_from = "week",
    values_from = "Position",
    names_prefix = "week"
  )

save(spotify, file = "../data/spotify.rda")
write_csv(spotify, file = "spotify.csv")
