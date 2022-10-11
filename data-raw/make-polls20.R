library(tidyverse)
library(lubridate)


## polling data from 538:
## https://github.com/fivethirtyeight/data/tree/master/polls
polls20 <- read_csv("president_polls_historical.csv")

state_df <- tibble(
  state = state.name,
  state_po = state.abb
) |>
  bind_rows(tibble(state = "District of Columbia", state_po = "DC"))

national_polls20 <- polls20 |>
  mutate(
    end_date = mdy(end_date),
  ) |>
  filter(
    is.na(state),
    answer %in% c("Biden", "Trump"),
    end_date > "2020-06-01",
    population == "lv"
    ) |>  
  select(end_date, pollster, sample_size, answer, pct) |>
  pivot_wider(
    names_from = answer,
    values_from = pct,
    values_fn = mean,
    ) |>
  rename(biden = Biden, trump = Trump)

save(national_polls20, file = "../data/national_polls20.rda")
write_csv(national_polls20, file = "national_polls20.csv")


polls20 <- polls20 |>
  mutate(
    end_date = mdy(end_date),
    days_left = as.numeric(ymd("2020-11-03") - end_date)
  ) |>
  left_join(state_df) |>
  select(-state) |>
  rename(state = state_po) |>  
  filter(
    !is.na(state),
    answer %in% c("Biden", "Trump"),
    end_date > "2020-06-01",
    population == "lv"
    ) |>  
  select(end_date, state, days_left, pollster, sample_size, answer, pct) |>
  pivot_wider(
    names_from = answer,
    values_from = pct,
    values_fn = mean,
    ) |>
  rename(biden = Biden, trump = Trump)

save(polls20, file = "../data/polls20.rda")
write_csv(polls20, file = "polls20.csv")


## electoral votes from older data
## See https://www.archives.gov/electoral-college/allocation
pres16 <- read_csv("pres16.csv") |>
  select(state, ev)

## data from: https://electionlab.mit.edu/data
pres20 <- read_csv("1976-2020-president.csv") |>
  filter(year == 2020)

pres20 <- pres20 |>
  mutate(
    candidate = case_when(
      party_simplified == "DEMOCRAT" ~ "biden",
      party_simplified == "REPUBLICAN" ~ "trump",
      TRUE ~ "other"
      )
  ) |>
  group_by(state_po, candidate) |>
  summarize(across(c(candidatevotes, totalvotes), sum)) |>  
  mutate(
    pct = 100 * (candidatevotes / totalvotes),
    ) |>
  rename(state = state_po) |>
  select(state, candidate, pct) |>  
  pivot_wider(
    names_from = candidate,
    values_from = pct
  ) |>  
  inner_join(pres16) |>
  select(state, ev, biden, trump, other)


save(pres20, file = "../data/pres20.rda")
write_csv(pres20, file = "pres20.csv")
