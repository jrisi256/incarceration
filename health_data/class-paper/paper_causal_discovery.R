# Load packages-----------------------------------------------------------------
library(plm)
library(lme4)
library(here)
library(dplyr)
library(readr)
library(lmtest)
library(stargazer)

# Load data---------------------------------------------------------------------
data <-
  read_csv(here("nosrati_replication", "input", "county_data_nosrati.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips),
    year = as.factor(year),
    log_education = education
  ) %>%
  select(-county)

data_causal_graph <-
  data %>%
  filter(if_all(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education", "px", "drugs"
    ),
    ~ !is.na(.x)
  )) %>%
  # group_by(fips) %>%
  # mutate(across(
  #   c(
  #     "income", "county_jail_adm", "county_prison_adm", "county_crime",
  #     "blacks", "hispanics", "race_other", "log_education", "px"
  #   ),
  #   ~ .x - mean(.x)
  # )) %>%
  filter(year %in% c(2012, 2013)) %>%
  group_by(fips) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  arrange(fips) %>%
  group_by(fips) %>%
  mutate(across(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education", "px"
    ),
    list(lag = lag)
  )) %>%
  ungroup() %>%
  filter(if_all(everything(), ~ !is.na(.x))) %>%
  select("drugs", matches("lag")) %>%
  select(-blacks_lag, -hispanics_lag, -race_other_lag)

write_csv(data_causal_graph, here("replication", "src", "data_causal_graph.csv"))

data_causal_graph <-
  data_causal_graph %>%
  mutate(across(
    c(
      "income_lag", "county_jail_adm_lag", "county_prison_adm_lag", "px_lag",
      "county_crime_lag", "log_education_lag"
    ),
    ~ scale(.x)[, 1]
  ))

ols <- lm(log(drugs) ~ county_prison_adm_lag + px_lag, data = data_causal_graph)
ols2 <- lm(log(drugs) ~ county_prison_adm_lag + income_lag + px_lag, data = data_causal_graph)
ols3 <- lm(log(drugs) ~ ., data = data_causal_graph)
