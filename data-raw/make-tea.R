library(tidyverse)

set.seed(02140)

cups <- rep(c("milk-first", "tea-first"), each = 4)

tea <- tibble(
  truth = sample(cups),
  guess = truth
)

save(tea, file = "../data/tea.rda")
write_csv(tea, file = "tea.csv")
