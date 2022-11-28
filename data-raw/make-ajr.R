library(tidyverse)
library(haven)

ajr <- read_dta("ajr.dta")

save(ajr, file = "../data/ajr.rda")
write_csv(ajr, file = "ajr.csv")
