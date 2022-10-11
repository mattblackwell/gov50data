library(tidyverse)
library(lubridate)


## polling data from 538:
## https://github.com/fivethirtyeight/data/tree/master/polls
polls20 <- read_csv("president_polls_historical.csv")

polls20 <- polls20 |>
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),    
    ) |>
  filter(
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
