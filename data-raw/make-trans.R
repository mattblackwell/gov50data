library(tidyverse)
library(haven)

# Kalla and Broockman (2016): Durably reducing transphobia: a field
# experiment on door-to-door canvassing
# https://doi.org/10.7910/DVN/WKR39N
trans <- read_dta("broockman_kalla_replication_data.dta")
trans <- trans %>%
  filter(
    !is.na(treat_ind),
    !is.na(vf_age),
    !is.na(miami_trans_law_t0),
    !is.na(miami_trans_law_withdef_t3),
    !is.na(therm_trans_t3),
    !is.na(therm_trans_t0)
  ) %>%
  mutate(
    nondiscrim_law_t0 = as.numeric(miami_trans_law_t0 > 0),
    nondiscrim_law_t3 = as.numeric(miami_trans_law_withdef_t3 > 0)
  ) %>%
  select(vf_age, vf_female, vf_vg_14, vf_vg_12, treat_ind,
         vf_racename, vf_democrat, nondiscrim_law_t0, nondiscrim_law_t3) %>%
  rename_with(~ gsub("vf_", "", .x, fixed = TRUE)) %>%
  rename(
    voted_gen_14 = vg_14,
    voted_gen_12 = vg_12,
    nondiscrim_post = nondiscrim_law_t3,
    nondiscrim_pre = nondiscrim_law_t0
  )

save(trans, file = "../data/trans.rda")
write_csv(trans, "trans.csv")
