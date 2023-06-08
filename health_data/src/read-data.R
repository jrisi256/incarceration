################################################################# Load packages
library(here)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

################################# Read in state and county health rankings data
years <- 2010:2023
data_dir <- here("health_data", "src", "data")
files <- here(data_dir, paste0("health-data_", years, ".csv"))
names(files) <- years
health_data_list <- map(files, read_csv)

############################ Concatenate all the data into one large data frame
health_data_dfs <- map_dfr(health_data_list, function(df) {
  df %>%
    select(-"County Ranked (Yes=1/No=0)") %>%
    filter(`5-digit FIPS Code` != "fipscode") %>%
    pivot_longer(
      cols = -matches("FIPS|State|Name|Release Year"),
      names_to = "variables",
      values_to = "values"
    ) %>%
    rename(state_fips = "State FIPS Code", county_fips = "County FIPS Code",
           full_fips = "5-digit FIPS Code", state_abb = "State Abbreviation",
           county_name = "Name", release_year = "Release Year") %>%
    separate_wider_regex(
      variables,
      c(
        variable = ".+",
        " ",
        stem = "raw value|numerator|denominator|CI low|CI high|\\(White\\)|\\(Black\\)|\\(Hispanic\\)"
      ),
      too_few = "align_start"
    ) %>%
    mutate(variable = str_replace_all(tolower(variable), " ", "_"),
           stem = str_replace_all(tolower(stem), " ", "_"),
           stem = str_replace_all(stem, "\\(|\\)", ""),
           values = as.numeric(values))
})

#### Separate national, state, and county level data (focused mostly on county)
nation_level <- health_data_dfs %>% filter(full_fips == "00000")

state_level <-
  health_data_dfs %>%
  filter(full_fips != "00000" & str_sub(full_fips, 3, 5) == "000")

county_level <- health_data_dfs %>% filter(str_sub(full_fips, 3, 5) != "000")

################################################### Harmonize data across years
# Change chlamydia rate in 2010 to sexually transmitted infections.
# Change income inequality to gini coefficient in 2010 to differentiate it from
#           later versions of income inequality which are measured differently.
# Change primary_care_provider_rate stems and variable names for 2010.
# Change primary_care_physician stems and variable names for multiple years.
# Change mental_health_provider stems and variable names for multiple years.
# Change other_primary_care_providers stems and variable names for multiple years.
# Change dentists stems and variable names for multiple years.
# Change lead poisoned children from 2010-2012 to match later years.
# Change water violations variable name to reflect its new meaning from 2016 on.
county_level_harmonize <-
  county_level %>%
  mutate(
    variable =
      if_else(
        release_year == 2010 & variable == "chlamydia_rate",
        "sexually_transmitted_infections",
        variable
      ),
    variable = 
      if_else(
        release_year == 2010 & variable == "income_inequality",
        "gini_coefficient",
        variable
      ),
    variable =
      if_else(
        release_year %in% c(2010, 2011, 2012) & variable == "lead_poisoned_children_(wi)",
        "lead_poisoned_children",
        variable
      ),
    variable = 
      if_else(
        release_year %in% c(2016, 2017, 2018) & variable == "drinking_water_violations",
        "drinking_water_violations_bin",
        variable
      ),
    stem =
      if_else(
        release_year == 2010 & variable == "primary_care_provider_rate_per_100000",
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year == 2010 & variable == "primary_care_provider_rate_per_100000",
        "primary_care_provider_rate",
        variable
      ),
    stem =
      if_else(
        release_year == 2010 & variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year == 2010 & variable == "ratio_of_population_to_primary_care",
        "primary_care_provider_rate",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
    ),
    variable = 
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_primary_care",
        "primary_care_physicians",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_mental_health",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_mental_health",
        "mental_health_providers",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to",
        "dentists",
        variable
        ),
    stem = 
      if_else(
        release_year %in% c(2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2014, 2015, 2016, 2017, 2018) & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "other_primary_care_providers",
        variable
      )
  )

# Need to turn liquor store density into per 100k people for both 2010 and 2011.
liquor_store_harmonize_2010 <-
  county_level_harmonize %>%
  filter(release_year == 2010, variable == "liquor_store_density") %>%
  pivot_wider(names_from = c("variable", "stem"), values_from = c("values")) %>%
  mutate(liquor_store_density_raw_value =
           if_else(!is.na(liquor_store_density_numerator),
                   liquor_store_density_numerator * 100000 / liquor_store_density_denominator,
                   0)) %>%
  pivot_longer(
    cols = -matches("state|county|fips|year"),
    names_to = "variable",
    values_to = "values"
  )

liquor_store_harmonize_2011 <-
  county_level_harmonize %>%
  filter(release_year == 2011, variable == "liquor_store_density") %>%
  pivot_wider(names_from = c("variable", "stem"), values_from = c("values")) %>%
  mutate(liquor_store_density_raw_value =
           liquor_store_density_numerator * 100000 / liquor_store_density_denominator) %>%
  pivot_longer(
    cols = -matches("state|county|fips|year"),
    names_to = "variable",
    values_to = "values"
  )

county_level_harmonize <-
  county_level_harmonize %>%
  filter(release_year != 2010 | variable != "liquor_store_density") %>%
  filter(release_year != 2011 | variable != "liquor_store_density") %>%
  bind_rows(list(liquor_store_harmonize_2010, liquor_store_harmonize_2011))

# Remove duplicate entries for access to healthy foods variable in 2012.
health_foods_harmonize_2012 <-
  county_level_harmonize %>%
  filter(release_year == 2012) %>%
  filter(str_detect(variable, "^access_to_healthy_foods")) %>%
  group_by(full_fips, variable, stem) %>%
  distinct(.keep_all = T) %>%
  group_by(full_fips, variable, stem) %>%
  filter(!is.na(values) | n() == 1) %>%
  ungroup()

county_level_harmonize <-
  county_level_harmonize %>%
  filter(release_year != 2012 | variable != "access_to_healthy_foods") %>%
  bind_rows(health_foods_harmonize_2012)















a2010 <-
  county_level_harmonize %>%
  filter(release_year == 2010, full_fips == "01001")

a2011 <-
  county_level_harmonize %>%
  filter(release_year == 2011, full_fips == "01001")

a2012 <-
  county_level_harmonize %>%
  filter(release_year == 2012, full_fips == "01001")

a2013 <-
  county_level_harmonize %>%
  filter(release_year == 2013, full_fips == "01001")

a2014 <-
  county_level_harmonize %>%
  filter(release_year == 2014, full_fips == "01001")

a2015 <-
  county_level_harmonize %>%
  filter(release_year == 2015, full_fips == "01001")

a2016 <-
  county_level_harmonize %>%
  filter(release_year == 2016, full_fips == "01001")

a2017 <-
  county_level_harmonize %>%
  filter(release_year == 2017, full_fips == "01001")

a2018 <-
  county_level_harmonize %>%
  filter(release_year == 2018, full_fips == "01001")
