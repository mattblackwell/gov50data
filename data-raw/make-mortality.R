## Under 5 mortality data from the World Bank
## https://data.worldbank.org/indicator/SH.DYN.MORT
library(tidyverse)

mortality <- read_csv("API_SH.DYN.MORT_DS2_en_csv_v2_4522456.csv",
                      skip = 4,
                      show_col_types = FALSE)

mortality <- mortality |>
  select(-`Indicator Code`, -`...67`) |>
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    indicator = `Indicator Name`
  )

save(mortality, file = "../data/mortality.rda")
write_csv(mortality, file = "mortality.csv")
