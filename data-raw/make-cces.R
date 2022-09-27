library(tidyverse)
library(haven)


## data from https://doi.org/10.7910/DVN/E9N6PH
cces <- read_dta("CES20_Common_OUTPUT_vv.dta")


cces_2020 <- cces |>
  filter(tookpost == 2) |> ## both wave answerers
  mutate(
    gender = as_factor(gender),
    educ = as_factor(educ),
    race = as_factor(race),
    pid3 = as_factor(pid3),
    turnout_self = if_else(zap_labels(CC20_401) == 5, 1, 0),
    pres_vote = as_factor(CC20_410)
    ) |>
  select(gender, race, educ, pid3, turnout_self, pres_vote)


write_csv(cces_2020, file = "cces_2020.csv")
save(cces_2020, file = "../data/cces_2020.rda")
