library(here)
library(dplyr)
library(tidyr)
library(readr)
library(fixest)
library(ggplot2)
library(stringr)
library(factoextra)
library(tidycensus)

incarceration_pre <-
  incarceration_trends <-
  read_csv(here("replication", "input", "incarceration_trends.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips),
    year = as.character(year)
  ) %>%
  select(total_jail_adm_rate, total_prison_adm_rate, year, fips)
  
incarceration_trends <-
  incarceration_pre %>%
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
    health_data_2020, incarceration_trends, by = "fips", multiple = "all"
  ) %>%
  arrange(fips, incarceration_year) %>%
  mutate(total_jail_adm_rate_2yrlag = lag(total_jail_adm_rate)) %>%
  filter(health_year == incarceration_year) %>%
  rename(year = health_year) %>%
  select(-incarceration_year, -total_prison_adm_rate)

final_data_2021 <-
  full_join(
    health_data_2021, incarceration_trends, by = "fips", multiple = "all"
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

ols_2020_2yrlag <-
  lm(
    mhlth ~
      scale(total_jail_adm_rate_2yrlag) + scale(total_prison_adm_rate_2yrlag),
    data = final_data_2020
  )

ols_2020 <-
  lm(
    mhlth ~
      scale(total_jail_adm_rate) + scale(total_prison_adm_rate_2yrlag),
    data = final_data_2020
  )

# census api key should be in my email

key_table <-
  tibble(
    variable = c(
      "S1701_C03_001", "S1702_C02_001", "S1902_C02_008", "DP02_0008P",
      "S2301_C04_001", "S0101_C02_022", "DP05_0071P", "DP02_0092P",
      "DP04_0046P", "DP04_0051P", "DP04_0052P", "DP04_0053P", "DP04_0054P",
      "DP04_0055P", "DP04_0056P", "S2701_C03_001"
    ),
    label = c(
      "prcnt_poverty_individual", "prcnt_poverty_family",
      "prcnt_publicasst_household", "prcnt_femalehhold_household",
      "prcnt_unemployed_16older", "prcnt_under18_individual",
      "prcnt_hispanic_individual", "prcnt_foreign_individual",
      "prcnt_homeown_housingunit", "prcnt_moved2017_ownedhouses",
      "prcnt_moved201516_ownedhouses", "prcnt_moved201014_ownedhouse",
      "prcnt_moved200009_ownedhouses", "prcnt_moved199099_ownedhouse",
      "prcnt_moved1989_ownedhouses", "prcnt_insured_individual"
    )
  )

# https://www.census.gov/acs/www/data/data-tables-and-tools/data-profiles/2018/
census_table_sub <- load_variables(year = "2018", dataset = "acs5/subject")
census_table_data <- load_variables(year = "2018", dataset = "acs5/profile")

census_vars <-
  get_acs(
    geography = "county",
    variables = c(
      "S1701_C03_001", "S1702_C02_001", "S1902_C02_008", "DP02_0008P",
      "S2301_C04_001", "S0101_C02_022", "DP05_0071P", "DP02_0092P",
      "DP04_0046P", "DP04_0051P", "DP04_0052P", "DP04_0053P", "DP04_0054P",
      "DP04_0055P", "DP04_0056P", "S2701_C03_001"
    ),
    year = 2018,
    survey = "acs5"
  )

census_vars_clean <-
  census_vars %>%
  full_join(key_table, by = "variable") %>%
  select(-variable) %>%
  pivot_wider(names_from = "label", values_from = c("estimate", "moe")) %>%
  select(-matches("poverty_individual|moved")) %>%
  select(matches("estimate|GEOID")) %>%
  filter(if_all(everything(), ~!is.na(.x)))

pca <-
  census_vars_clean %>%
  select(-GEOID, -estimate_prcnt_insured_individual) %>%
  prcomp(center = T, scale. = T)
pcaDat <- get_pca(pca)
fviz_pca_biplot(pca, label = "var")
fviz_pca_var(pca)

fviz_screeplot(pca, addlabels = T, choice = "eigenvalue")
fviz_screeplot(pca, addlabels = T, choice = "variance")

final_data <-
  census_vars_clean %>%
  bind_cols(pca$x) %>%
  rename(fips = GEOID) %>%
  full_join(final_data_2020, by = "fips")
  
ols_2020_2yrlag <-
  lm(
    mhlth ~
      scale(total_jail_adm_rate_2yrlag) + scale(total_prison_adm_rate_2yrlag) +
      scale(estimate_prcnt_insured_individual) + PC1 + PC2 + PC3,
    data = final_data
  )

# county_rankings_2015 <-
#   read_csv(here("replication", "input", "analytic_data2015.csv")) %>%
#   select(matches("5-digit FIPS Code|mental health days raw value")) %>%
#   rename(fips = `5-digit FIPS Code`, mhlth = `Poor mental health days raw value`) %>%
#   filter(str_sub(fips, 3, 5) != "000" & fips != "fipscode") %>%
#   mutate(mhlth = as.numeric(mhlth),
#          year = "2013")

county_rankings_2016 <-
  read_csv(here("replication", "input", "analytic_data2016.csv")) %>%
  select(matches("5-digit FIPS Code|mental health days raw value|frequent mental distress raw value|Excessive drinking raw value")) %>%
  rename(fips = `5-digit FIPS Code`, mhlth = `Poor mental health days raw value`,
         mhlth_distress = `Frequent mental distress raw value`,
         drinking = `Excessive drinking raw value`) %>%
  filter(str_sub(fips, 3, 5) != "000" & fips != "fipscode") %>%
  mutate(mhlth = as.numeric(mhlth),
         mhlth_distress = as.numeric(mhlth_distress),
         drinking = as.numeric(drinking),
         year = "2014")

county_rankings_2017 <-
  read_csv(here("replication", "input", "analytic_data2017.csv")) %>%
  select(matches("5-digit FIPS Code|mental health days raw value|frequent mental distress raw value|Excessive drinking raw value")) %>%
  rename(fips = `5-digit FIPS Code`, mhlth = `Poor mental health days raw value`,
         mhlth_distress = `Frequent mental distress raw value`,
         drinking = `Excessive drinking raw value`) %>%
  filter(str_sub(fips, 3, 5) != "000" & fips != "fipscode") %>%
  mutate(mhlth = as.numeric(mhlth),
         mhlth_distress = as.numeric(mhlth_distress),
         drinking = as.numeric(drinking),
         year = "2015")

county_rankings_2018 <-
  read_csv(here("replication", "input", "analytic_data2018.csv")) %>%
  select(matches("5-digit FIPS Code|mental health days raw value|frequent mental distress raw value|Excessive drinking raw value")) %>%
  rename(fips = `5-digit FIPS Code`, mhlth = `Poor mental health days raw value`,
         mhlth_distress = `Frequent mental distress raw value`,
         drinking = `Excessive drinking raw value`) %>%
  filter(str_sub(fips, 3, 5) != "000" & fips != "fipscode") %>%
  mutate(mhlth = as.numeric(mhlth),
         mhlth_distress = as.numeric(mhlth_distress),
         drinking = as.numeric(drinking),
         year = "2016")

county_rankings <-
  bind_rows(list(
    county_rankings_2016, county_rankings_2017, county_rankings_2018
  ))
  
county_rankings_coefv <-
  county_rankings %>%
  group_by(fips) %>%
  summarise(mean = mean(drinking, na.rm = T),
            sd = sd(drinking, na.rm = T),
            coefv = sd / mean) %>%
  ungroup()

county_rankings_final_data <-
  county_rankings %>%
  inner_join(incarceration_pre, by = c("year", "fips"))

ols_fixed <-
  feols(drinking ~ total_jail_adm_rate + total_prison_adm_rate | fips + year,
        cluster = ~ fips + year,
        data = county_rankings_final_data)
