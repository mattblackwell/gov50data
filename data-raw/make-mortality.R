## Under 5 mortality data from the World Bank
## https://data.worldbank.org/indicator/SH.DYN.MORT
library(tidyverse)

mortality <- read_csv("API_SH.DYN.MORT_DS2_en_csv_v2_4522456.csv",
                      skip = 4,
                      show_col_types = FALSE)

regions <- c(
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Arab World",
  "Central Europe and the Baltics",
  "Caribbean small states",
  "East Asia & Pacific (excluding high income)",
  "Early-demographic dividend",
  "East Asia & Pacific",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia",
  "Euro area",
  "European Union",
  "Fragile and conflict affected situations",
  "High income",
  "Heavily indebted poor countries (HIPC)",
  "IBRD only",
  "IDA & IBRD total",
  "IDA total",
  "IDA blend",
  "IDA only",
  "Not classified",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean",
  "Least developed countries: UN classification",
  "Low income",
  "Lower middle income",
  "Low & middle income",
  "Late-demographic dividend",
  "Middle East & North Africa",
  "Middle income",
  "Middle East & North Africa (excluding high income)",
  "North America",
  "OECD members",
  "Other small states",
  "Pre-demographic dividend",
  "Pacific island small states",
  "Post-demographic dividend",
  "South Asia",
  "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa",
  "Small states",
  "East Asia & Pacific (IDA & IBRD countries)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Upper middle income",
  "World"
)

mortality <- mortality |>
  select(-c(`Indicator Code`, `1960`:`1971`, `2021`, `...67`)) |>
  rename(
    country = `Country Name`,
    country_code = `Country Code`,
    indicator = `Indicator Name`
  ) |>
  filter(!(country %in% regions)) 

save(mortality, file = "../data/mortality.rda")
write_csv(mortality, file = "mortality.csv")
