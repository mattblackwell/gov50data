library(tidyverse)
library(haven)
## data from https://doi.org/10.7910/DVN/43E9R9
newspapers <- read_dta(
  "1992-1997BESPanel.dta"
)
newspapers <- newspapers %>%
  mutate(
    age = rage92,
    vote_lab_92 = as.numeric(vote92 == 2), 
    vote_lab_97 = as.numeric(vote97 == 2),
    to_labour = as.numeric(whpapr96 %in% c(4, 5, 8, 10)),
    male = as.numeric(rsex92 == 1),
    parent_labour = as.numeric(
      dadvot92 == 2 | mumvot92 == 2
    ),
    work_class = as.numeric(
      srsoc192  == 2 | srsoc292 == 2
    )    
  )  %>%
  filter(
    !is.na(vote_lab_97),
    vote92 != -1,
    vote97 != -1
  ) %>%
  select(c(
    to_labour,
    vote_lab_92,
    vote_lab_97,
    age,
    male,
    parent_labour,
    work_class
  ))

save(newspapers, file = "../data/newspapers.rda")
write.csv(newspapers, file = "newspapers.csv", row.names = FALSE)
