# A script ot download the population by state 

# Source: ACS5 Survey, 2018

# Author: NMac
# Version: 2020-02-02

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters
year <- 2018
file <- here::here(str_glue("c01-own/data/population.rds"))
var_names <- c(population = "B01001_001")
tidy_var_names <- 
  c(
    geoid = "GEOID",
    name = "NAME",
    population = "population"
  )
#===============================================================================

get_acs(
  geography = "state",
  variables = var_names,
  year = year  
) %>% 
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>% 
  select(!!! tidy_var_names) %>% 
  right_join(
    tibble(
      name = state.name,
      state = state.abb
    ),
    by = "name"
  ) %>% 
  select(state, population) %>% 
  write_rds(file)
