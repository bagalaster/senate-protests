# A script to download the Trump Score averages dataset from FiveThirtyEight

# Author: Mac Bagwell
# Version: 2020-02-01

# Source: https://projects.fivethirtyeight.com/congress-trump-score/

# Libraries
library(tidyverse)

NAME <- "member_averages"

# Parameters
url <- "https://projects.fivethirtyeight.com/congress-tracker-data/csv/averages.csv"
file_raw <- here::here(str_glue("c01-own/data-raw/{NAME}.csv"))
file <- here::here(str_glue("c01-own/data/{NAME}.rds"))
party_recode <- c("Democrat" = "D", "Republican" = "R", "Independent" = "I")
col_types <- 
  cols_only(
    congress = col_integer(),
    chamber = col_factor(),
    bioguide = col_character(),
    last_name = col_character(),
    state = col_character(),
    party = col_factor(),
    votes = col_integer(),
    agree_pct = col_double(),
    predicted_agree = col_double(),
    net_trump_vote = col_double()
  )
#===============================================================================

url %>% 
  download.file(file_raw)

url %>% 
  read_csv(col_types = col_types) %>%
  filter(chamber == "senate", congress == 115) %>%
  mutate(
    party = fct_recode(party, !!! party_recode),
    last_name = recode(last_name, `Menéndez` = "Menendez", .default = last_name)
  ) %>% 
  write_rds(file)
