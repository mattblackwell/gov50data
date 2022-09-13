
## Cleaning data from Martin and McCrain (2019) "Local News and
## National Politics" APSR
## Original data from: https://doi.org/10.7910/DVN/G3X4EW
## Too large for repo, download from DVN and place
## `localnews_replication/` folder here. 

library(tidyverse)
library(haven)
library(lubridate)


## DV = content measures
load("data-raw/localnews_replication/data/topicmodel/segments.RData")
t_weights <- read_csv(
  "data-raw/localnews_replication/data/topicmodel/document_probability_t15.csv",
  col_names = paste("topic", 0:14, sep = ""),
  progress = FALSE
)
topic_desc <- read_csv(
  "data-raw/localnews_replication/data/topicmodel/tveyes_lda_t15_words.csv"
) %>%
  mutate(topic_desc = gsub("\\W+", "_", topic_desc))

colnames(t_weights) <- topic_desc$topic_desc

segments <- cbind(segments[1:nrow(t_weights), ], t_weights)

# merge with demographics
load("data-raw/localnews_replication/data/clean/tveyes_stations.RData")
segments <- left_join(segments, ungroup(stations)) %>%
  mutate(sinclair = replace(sinclair, is.na(sinclair), 0),
         sinclair2017 = replace(sinclair2017, is.na(sinclair2017), 0))

load("data-raw/localnews_replication/data/tveyes/tveyes_ratings_final.RData")
segments <- semi_join(segments, ungroup(all_ratings))

# collapse to daily, merge with slant measure
load("data-raw/localnews_replication/data/text_slant/tv_ideology.RData")
tv_ideology <- tv_ideology %>% ungroup %>% select(-year)

days <- segments %>% 
  group_by(station_id, dmacode, callsign, affiliation, date) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  left_join(tv_ideology)

days <- ungroup(days) %>% 	
  mutate(post = as.numeric(as.numeric(sub("(\\d+)/.*", "\\1", date)) > 8),
         timeslot = paste(hour, ifelse(min >=30, "30", "00"), sep=":"),
         weekday = wday(mdy(date), label=T),
         month = month(mdy(date), label=T),
         national_politics = national_politics_domestic_policy +
           national_politics_trump_scandals + national_politics_foreign_policy,
         local_politics = local_politics_infrastructure +
           local_politics_education) %>% 
  filter(month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

days <- days %>% 
  group_by(dmacode) %>% 
  mutate(any_sinclair_acq = as.numeric(max(sinclair2017) > 0)) %>%
  ungroup() %>%
  filter(any_sinclair_acq  == 1)


news <- days |>
  ungroup() |>
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>  
  select(callsign, affiliation, date, weekday, ideology, national_politics,
         local_politics, sinclair2017, post,
         month) |>
  arrange(date, callsign, affiliation)

write_csv(news, "data-raw/news.csv")
save(news, file = "data/news.rda")
