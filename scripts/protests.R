# A script to download the protest dataset from the Crowd-Counting Consortium

# Author: Mac Bagwell
# Version: 2020-02-01

# Source: https://sites.google.com/view/crowdcountingconsortium/view-download-the-data?authuser=0

# Libraries
library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets4)
library(rvest)

# Parameters
NAME <- "protests"
SHEET_MASK <- "docs.google.com/spreadsheets"
LINK_INDEXES <- 3:25
landing_url <- 
  "https://sites.google.com/view/crowdcountingconsortium/view-download-the-data?authuser=0"
finalized_data_css_selector <- "#h\\.p_sU9ek5V6QKs2"
row_css_selector <- "div.CjVfdc"
hlink_css_selector <- "a.dhtgD.aw5Odc"
col_type_sheets <- "---c-T-iii----i"
xls_temp_file <- here::here("c01-own/data-raw/temp.xls")
file_raw <- here::here(str_glue("c01-own/data-raw/{NAME}.csv"))
file <- here::here(str_glue("c01-own/data/{NAME}.rds"))

pro_anti_trump_recode <- 
  c(
    `0` = "anti", 
    `1` = "pro", 
    `-1` = "neutral", 
    .missing = NA_character_, 
    .default = NA_character_
  )

estimate_attendance <- function(estimate, low, high) {
  case_when(
    !is.na(estimate) ~ estimate %>% as.double(),
    is.na(high) & is.na(low) ~ NA_real_,
    is.na(low) ~ high %>% as.double(),
    is.na(high) ~ low %>% as.double(),
    TRUE ~ exp((log(low) + log(high)) / 2)
  )
}

#' Checks if a link is to a google sheet or to the location of
#' an xlsx file in google drive; CCC is inconsistent in which format
#' the data is in
is_googlesheet <- function(sheet_url) {
  sheet_url %>% 
    str_detect(SHEET_MASK)
}

date_convert <- function(x) {
  if ("POSIXct" %in% class(x)) {
    x %>% 
      as_datetime() %>% 
      as_date()
  } else {
    x %>% 
      map_dbl(
        ~ ifelse(is.null(.) || !("POSIXct" %in% class(.)), NA_real_, .)
      ) %>% 
      as_datetime() %>% 
      as_date()
  }
}

integer_convert <- function(x) {
  if ("numeric" %in% class(x) || "integer" %in% class(x)) {
    as.integer(x)
  } else {
    x %>% 
      map(~ ifelse(is.null(.), NA_integer_, .)) %>% 
      map_int(as.integer)
  }
}

clean_tibble <- function(tbl) {
  tbl %>% 
    select(
      state = StateTerritory,
      date = Date,
      estimate_low = EstimateLow,
      estimate_high = EstimateHigh,
      estimate = BestGuess,
      pro_anti_trump = matches("^Pro[ ]?\\(2\\)")
    ) %>% 
    mutate_at(
      vars(starts_with("estimate")),
      integer_convert
    ) %>% 
    mutate(
      state = str_to_upper(state),
      pro_anti_trump = 
        (integer_convert(pro_anti_trump) - 1L) %>% 
        recode(!!! pro_anti_trump_recode),
      date = date_convert(date)
    ) %>% 
    filter(
      state %in% state.abb,
      date >= "2017-02-01",
      date <= "2018-12-31"
    )
}

read_xlsx_from_drive <- function(sheet_url) {
  tmp <- tempfile(fileext = ".xlsx")
  sheet_url %>%
    drive_download(
      tmp, 
      overwrite = TRUE
    )
  tryCatch(
    expr = {
      tmp %>% 
        readxl::read_xlsx(sheet = 2) %>% 
        clean_tibble()
    },
    error = function(e) {
      tmp %>% 
        readxl::read_xlsx() %>% 
        clean_tibble()
    }
  )
}

#' NOTE: This function manually codes a number of rows to read
#' for one of the sheets because this is the only one which
#' has extraneous values. Passing column types to read_sheet is
#' impossible because the table schema are inconsistent
read_link <- function(sheet_url) {
  if (is_googlesheet(sheet_url)) {
    sheet_url %>%
      read_sheet(sheet = 2L) %>%
      clean_tibble()
  } else {
    sheet_url %>% 
      read_xlsx_from_drive()
  }
}

scrape_links <- function(landing_url) {
  landing_url %>% 
    read_html() %>%
    html_nodes(finalized_data_css_selector) %>% 
    html_nodes(row_css_selector) %>% 
    html_node(hlink_css_selector) %>% 
    html_attr("href") %>% 
    magrittr::extract(LINK_INDEXES) 
}

#===============================================================================

drive_auth()

df <- 
  landing_url %>% 
  scrape_links() %>% 
  map_dfr(read_link) %>% 
  mutate(
    attendance = 
      estimate_attendance(estimate, estimate_low, estimate_high)
  )

df %>% 
  select(state, date, pro_anti_trump, attendance) %>% 
  write_rds(file)
