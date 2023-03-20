library(here)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tidycensus)

incarceration_trends <-
  read_csv(here("replication", "input", "incarceration_trends.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips),
    year = as.character(year)
  ) %>%
  select(total_jail_adm_rate, total_prison_adm_rate, year, fips) %>%
  arrange(fips, year) %>%
  # mutate(across(matches("jail|prison"), list(lag = ~ lag(.)))) %>%
  mutate(total_prison_adm_rate_2yrlag = lag(total_prison_adm_rate, n = 2)) %>%
  filter(year == "2016" | year == "2018") %>%
  mutate(
    total_prison_adm_rate_2yrlag =
      if_else(year == "2016", NA_real_, total_prison_adm_rate_2yrlag)
  ) %>%
  rename(incarceration_year = year)

# Read in data dictionary-------------------------------------------------------
# Health data dictionary--------------------------------------------------------
data_dictionary <-
  read_csv(here(
    "replication", "input", "PLACES_and_500_Cities__Data_Dictionary.csv"
  )) %>%
  mutate(variable = paste0(MeasureID, "_AdjPrev")) %>%
  arrange(variable) %>%
  filter(MeasureID %in% c("DEPRESSION", "MHLTH"))

# Read in health data-----------------------------------------------------------
health_data_2020 <- 
  read_csv(here("replication", "input", "health_data_2020.csv")) %>%
  select(MHLTH_AdjPrev, CountyFIPS) %>%
  mutate(health_year = 2018) %>%
  rename(fips = CountyFIPS, mhlth = MHLTH_AdjPrev)

health_data_2021 <-
  read_csv(here("replication", "input", "health_data_2021.csv")) %>%
  filter(MeasureId %in% c("DEPRESSION", "MHLTH"),
         Data_Value_Type == "Age-adjusted prevalence") %>%
  select(Year, LocationID, Data_Value, MeasureId) %>%
  pivot_wider(names_from = MeasureId, values_from = Data_Value) %>%
  filter(nchar(LocationID) == 5) %>%
  rename(fips = LocationID, health_year = Year, mhlth = MHLTH, depression = DEPRESSION) %>%
  mutate(health_year = as.character(health_year))

health_data_2022 <-
  read_csv(here("replication", "input", "health_data_2022.csv")) %>%
  filter(MeasureId %in% c("DEPRESSION", "MHLTH"),
         Data_Value_Type == "Age-adjusted prevalence") %>%
  select(Year, LocationID, Data_Value, MeasureId) %>%
  pivot_wider(names_from = MeasureId, values_from = Data_Value) %>%
  filter(nchar(LocationID) == 5) %>%
  rename(fips = LocationID, health_year = Year, mhlth = MHLTH, depression = DEPRESSION) %>%
  mutate(health_year = as.character(health_year))

final_data_2020 <-
  full_join(
    health_data_2020, incarceration_trends,
    by = "fips", multiple = "all"
  ) %>%
  arrange(fips, incarceration_year) %>%
  mutate(total_jail_adm_rate_2yrlag = lag(total_jail_adm_rate)) %>%
  filter(health_year == incarceration_year) %>%
  rename(year = health_year) %>%
  select(-incarceration_year, -total_prison_adm_rate)

final_data_2021 <-
  full_join(health_data_2021, incarceration_trends,
    by = "fips", multiple = "all"
  ) %>%
  arrange(fips, incarceration_year) %>%
  mutate(total_jail_adm_rate_3yrlag = lag(total_jail_adm_rate)) %>%
  rename(total_prison_adm_rate_3yrlag = total_prison_adm_rate_2yrlag,
         total_jail_adm_rate_1yrlag = total_jail_adm_rate) %>%
  filter(incarceration_year == "2018") %>%
  rename(year = health_year) %>%
  select(-incarceration_year, -total_prison_adm_rate)

final_data_2022 <-
  full_join(health_data_2022, incarceration_trends,
            by = "fips", multiple = "all"
  ) %>%
  arrange(fips, incarceration_year) %>%
  mutate(total_jail_adm_rate_4yrlag = lag(total_jail_adm_rate)) %>%
  rename(total_prison_adm_rate_4yrlag = total_prison_adm_rate_2yrlag,
         total_jail_adm_rate_2yrlag = total_jail_adm_rate) %>%
  filter(incarceration_year == "2018") %>%
  rename(year = health_year) %>%
  select(-incarceration_year, -total_prison_adm_rate)

ols_2020_2yrlag <- lm(mhlth ~ total_jail_adm_rate_2yrlag + total_prison_adm_rate_2yrlag, data = final_data_2020)
ols_2020 <- lm(mhlth ~ total_jail_adm_rate + total_prison_adm_rate_2yrlag, data = final_data_2020)
