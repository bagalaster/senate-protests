# A script to join the protest data and ACS population estimates

# Author: Mac Bagwell
# Version: 2020-02-02

# Libraries
library(tidyverse)

# Parameters
protest_file <- here::here("c01-own/data/protests.rds")
population_file <- here::here("c01-own/data/population.rds")
file <- here::here("c01-own/data/protests_population.rds")
#===============================================================================

protest_file %>% 
  read_rds() %>% 
  left_join(
    population_file %>% 
      read_rds(),
    by = "state"
  ) %>% 
  write_rds(file)
