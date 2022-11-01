library(tidyverse)

anes <- read_csv("anes_timeseries_2020_csv_20220210.csv", show_col_types = FALSE)

anes <- anes |>
  rename(
    state = V203001,
    district = V203002,
    pid7 = V201231x,
    pres_vote = V202073,
    sci_therm = V202173,
    rural_therm = V202184,
    favor_voter_id = V201357,
    envir_doing_more = V201401
  ) |>
  select(
    state, district, pid7, pres_vote, sci_therm, rural_therm,
    favor_voter_id, envir_doing_more
  ) |>
  mutate(
    across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)),
    across(ends_with("therm"), ~ ifelse(.x > 100, NA, .x)),
    pres_vote = case_when(
      pres_vote == 1 ~ "Biden",
      pres_vote == 2 ~ "Trump",
      pres_vote > 2 & pres_vote < 10 ~ "Other",
      pres_vote > 10 ~ as.character(NA)
    ),
    favor_voter_id = if_else(favor_voter_id == 1, 1, 0),
    envir_doing_more = if_else(envir_doing_more == 1, 1, 0)
  ) |>
  drop_na()
  
save(anes, file = "../data/anes.rda")
write_csv(anes, "anes.csv")
