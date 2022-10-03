library(tidyverse)


## https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh/data
## Filtered to 9/28/2022
covid <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")

covid <- covid |>
  rename(
    one_dose_5plus_pct = Administered_Dose1_Recip_5PlusPop_Pct,
    one_dose_65plus_pct = Administered_Dose1_Recip_65PlusPop_Pct,    
    fips = FIPS,
    county = Recip_County,
    state = Recip_State
  ) |>
  mutate(
    booster_5plus_pct = 100 * (Booster_Doses_5Plus / Census2019_5PlusPop),
  ) |>  
  select(fips, county, state, one_dose_5plus_pct, one_dose_65plus_pct, booster_5plus_pct)

## data from: https://electionlab.mit.edu/data
votes <- read_csv("countypres_2000-2020.csv")

votes <- votes |>
  filter(party == "DEMOCRAT") |>
  mutate(
    dem_pct = 100 * (candidatevotes / totalvotes),
    ) |>
  rename(fips = county_fips) |>  
  select(fips, year, dem_pct) |>
  group_by(fips, year) |>
  summarize(across(dem_pct, sum)) |>  
  pivot_wider(
    names_from = year,
    names_prefix = "dem_pct_",
    values_from = "dem_pct"
  )



covid_votes <- covid |>
  inner_join(votes) |>
  select(fips:booster_5plus_pct, dem_pct_2000, dem_pct_2020)

save(covid_votes, file = "../data/covid_votes.rda")
write_csv(covid_votes, file = "covid_votes.csv")
