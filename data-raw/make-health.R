library(tidyverse)

health <- read_csv("health-raw.csv") |>
  rename(
    active_calories = active.calories,
    steps_lag = steps.lag,
    calorie_lag = calorie.lag
  )

save(health, file = "../data/health.rda")
write_csv(health, file = "health.csv")
