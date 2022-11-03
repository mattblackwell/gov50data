library(tidyverse)

trains <- read_csv("boston.csv", show_col_types = FALSE)

save(trains, file = "../data/trains.rda")
write_csv(trains, "trains.csv")
