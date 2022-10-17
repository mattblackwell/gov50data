library(tidyverse)

## We could probably build this by hand by taking data directly from
## the FRED data, gallup, and wikipedia. 

midterms <- read_csv("midterms_raw.csv", show_col_types = FALSE) |>
  rename(
    rdi_change = rdi.change,
    seat_change = seat.change    
  )

save(midterms, file = "../data/midterms.rda")
write_csv(midterms, file = "midterms.csv")
