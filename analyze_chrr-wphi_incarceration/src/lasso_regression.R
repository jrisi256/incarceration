library(here)
library(mlr3)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

##################################################################
##                         Read in data                         ##
##################################################################
incarceration <-
  read_csv(here("incarceration_trends.csv")) %>%
  select(total_jail_adm_rate, total_prison_adm_rate, year, fips, state) %>%
  mutate(
    fips =
      if_else(str_length(fips) == 4, paste0("0", fips), as.character(fips))
  ) %>%
  pivot_longer(
    cols = c("total_jail_adm_rate", "total_prison_adm_rate"),
    names_to = "variable",
    values_to = "values"
  ) %>%
  filter(!is.na(values)) %>%
  rename(full_fips = fips, state_abb = state, start_year = year) %>%
  mutate(end_year = start_year, range = F)

state_names <-
  read_csv(here("clean_chrr-wphi", "output", "county_state_names.csv")) %>%
  select(full_fips, state_abb)

health_end_year <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "dedupe_start_end",
      "cross_nonsplit_late_end_year-min.csv"
    )
  ) %>%
  filter(stem == "raw_value", !is.na(values), race == "all") %>%
  select(
    -stem,
    -race,
    -cross_longitudinal_match,
    -dedupe,
    -dedupe_start_end_year,
    -release_year
  ) %>%
  full_join(state_names, by = c("full_fips"), multiple = "all") %>%
  mutate(range = if_else(end_year - start_year != 0, T, F))

#################################################################
##                          Join data                          ##
#################################################################
join_end_year <-
  bind_rows(health_end_year, incarceration) %>%
  select(-start_year, -range) %>%
  pivot_wider(names_from = variable, values_from = values) %>%
  rename(children_in_single_parent_households = `children_in_single-parent_households`,
         year = end_year)

child_mortality <-
  join_end_year %>%
  select(full_fips, year, state_abb, child_mortality, total_prison_adm_rate) %>%
  filter(!is.na(child_mortality), !is.na(total_prison_adm_rate)) %>%
  mutate(
    across(
      c("child_mortality", "total_prison_adm_rate"),
      function(col) {col / sd(col)}
    )
  ) %>%
  arrange(full_fips, year) %>%
  group_by(full_fips) %>%
  mutate(
    across(
      .cols = c("total_prison_adm_rate"),
      .fns = 
        list(
          lag_2yr = function(col) {lag(col)},
          lag_3yr = function(col) {lag(col, n = 2)},
          lag_5yr = function(col) {lag(col, n = 3)},
          lag_6yr = function(col) {lag(col, n = 4)}
        )
    )
  ) %>%
  ungroup()


  pivot_longer(
    cols = c("child_mortality", "total_prison_adm_rate"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(value = value / sd(value)) %>%
