# A script to download the Trump Score senate votes dataset from FiveThirtyEight

# Author: Mac Bagwell
# Version: 2020-01-31

# Source: https://projects.fivethirtyeight.com/congress-trump-score/

# Libraries
library(tidyverse)

# Parameters
url <- "https://projects.fivethirtyeight.com/congress-tracker-data/csv/vote_predictions.csv"
file_raw <- here::here("c01-own/data-raw/votes.csv")
file <- here::here("c01-own/data/votes.rds")
date_formats <- c("Ymd HMS z", "Ymd HMS")
tz <- "America/New_York"
party_recode <- c("Democrat" = "D", "Republican" = "R", "Independent" = "I")
last_name_recode <- c("Menéndez" = "Menendez")
col_types <- 
  cols_only(
    congress = col_integer(),
    bill_id = col_character(),
    roll_id = col_character(),
    chamber = col_factor(),
    voted_at = col_character(),
    bioguide = col_character(),
    vote = col_factor(),
    trump_position = col_factor(),
    last_name = col_character(),
    state = col_character(),
    party = col_factor(),
    agree = col_integer(),
    predicted_probability = col_double()
  )
#===============================================================================
url %>% 
  download.file(file_raw)

url %>% 
  read_csv(col_types = col_types) %>%
  filter(chamber == "senate") %>% 
  mutate(
    voted_at = 
      voted_at %>% 
      lubridate::parse_date_time(date_formats, tz = tz),
    party = 
      party %>% 
      fct_recode(!!! party_recode),
    last_name =
      recode(last_name, !!! last_name_recode),
    vote = 
      vote %>% 
      fct_lump(n = 2)
  ) %>% 
  write_rds(file)
