library(tidyverse)


class_years <- read_csv("classyears_raw.csv", show_col_types = FALSE) |>
  rename(year = Level)

save(class_years, file = "../data/class_years.rda")
write_csv(class_years, file = "class_years.csv")
