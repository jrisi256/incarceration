################################################################## Load packages
library(here)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

################################## Read in state and county health rankings data
years <- 2010:2023
data_dir <- here("health_data", "src", "data")
files <- here(data_dir, paste0("health-data_", years, ".csv"))
names(files) <- years
health_data_list <- map(files, read_csv)

############################ Concatenate all the data into one large data frame.
health_data_dfs <-
  pmap_dfr(
    list(health_data_list, names(health_data_list)),
    function(df, yr) {
      # Filter out junk rows, make the data long, and rename identifying columns
      df <-
        df %>%
        select(-"County Ranked (Yes=1/No=0)") %>%
        filter(`5-digit FIPS Code` != "fipscode") %>%
        pivot_longer(
          cols = -matches("FIPS|State|Name|Release Year"),
          names_to = "variables",
          values_to = "values"
        ) %>%
        rename(
          state_fips = "State FIPS Code",
          county_fips = "County FIPS Code",
          full_fips = "5-digit FIPS Code",
          state_abb = "State Abbreviation",
          county_name = "Name",
          release_year = "Release Year"
        )

      race_str <- "\\(AIAN\\)|\\(White\\)|\\(Black\\)|\\(Hispanic\\)|\\(Asian/Pacific Islander\\)|\\(Asian\\)"
      stem_str <- "raw value|numerator|denominator|CI high|CI low|flag|unreliable"
      locate_race <- str_locate(df$variables, race_str)

      # separate out variable, stem, and race
      df %>%
        mutate(
          start = locate_race[, "start"],
          end = locate_race[, "end"],
          start = if_else(str_detect(variables, stem_str), NA_integer_, start),
          variables = if_else(
            !is.na(start),
            paste0(
              str_sub(variables, 1, start - 1),
              "raw value ",
              str_sub(variables, start, end)
            ),
            variables
          )
        ) %>%
        separate_wider_regex(
          variables,
          c(variable = ".+", " ", stem = stem_str, " ", race = race_str),
          too_few = "align_start"
        ) %>%
        select(-start, -end) %>%
        mutate(
          variable = str_replace_all(tolower(variable), " ", "_"),
          stem = str_replace_all(tolower(stem), " ", "_"),
          race = str_replace_all(tolower(race), "\\(|\\)", ""),
          values = as.numeric(values),
          release_year = yr
        )
    }
  )

###################### Separate out county level data (focused mostly on county)
county_level <- health_data_dfs %>% filter(str_sub(full_fips, 3, 5) != "000")

#################################################### Harmonize data across years
############## Change chlamydia rate in 2010 to sexually transmitted infections.
########################## Change income inequality to gini coefficient in 2010.
############# Change lead poisoned children from 2010-2012 to match later years.
########### Change primary_care_provider_rate stems and variable names for 2010.
#### Rename mammography_screening to mammography_screening_67_69 from 2011-2018.
# Change %_non-hispanic_african_american to %_non-hispanic_black from 2011-2019.
####### Change stem values for severe housing problems variables from 2019-2023.
### Recalculate preventable hospital stays to be per 1000 people from 2019-2023.
######## Change water violations name to reflect its new meaning from 2016-2023.
######### Change primary_care_physician stems and variable names from 2011-2023.
######### Change mental_health_provider stems and variable names from 2011-2023.
### Change other_primary_care_providers stems and variable names from 2014-2023.
####################### Change dentists stems and variable names from 2012-2023.
################################## Rename lbw to low_birthweight from 2020-2023.
################# Change the stem to raw_value for crude_suicide from 2020-2023.
###### Change the stems/names for formal/informal juvenile cases from 2020-2023.
#### Rename mammography_screening to mammography_screening_65_74 From 2019-2023.
######### Harmonize American Indian/Alaskan Native from 2011-2022 to match 2023.
###################### Harmonize females from 2011-2021 to match newer releases.
### Fix Hawaiian/Other Pacific Islanders from 2011-2022 to match newer releases.
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
        release_year %in% 2010:2012 & variable == "lead_poisoned_children_(wi)",
        "lead_poisoned_children",
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
        release_year %in% 2019:2023 &
          variable %in% c(
            "percentage_of_households_with_high_housing",
            "percentage_of_households_with",
            "percentage_of_households_with_lack_of_kitchen_or_plumbing"
          ),
        "raw_value",
        stem
      ),
    variable =
      case_when(
        release_year %in% 2011:2018 & variable == "mammography_screening" ~ "mammography_screening_67_69",
        release_year %in% 2019:2023 & variable == "mammography_screening" ~ "mammography_screening_65_74",
        T ~ variable
      ),
    variable = 
      case_when(
        release_year %in% 2011:2019 & variable == "%_american_indian_and_alaskan_native" ~ "%_american_indian_or_alaska_native",
        release_year %in% 2020:2022 & variable == "%_american_indian_&_alaska_native" ~ "%_american_indian_or_alaska_native",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2021 & variable == "%_females" ~ "%_female",
        T ~ variable
      ),
    variable = 
      case_when(
        release_year %in% 2011:2022 & variable == "%_native_hawaiian/other_pacific_islander" ~ "%_native_hawaiian_or_other_pacific_islander",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2019 & variable == "%_non-hispanic_african_american" ~ "%_non-hispanic_black",
        T ~ variable
      ),
    variable =
      if_else(
        release_year %in% 2019:2023 & variable == "percentage_of_households_with_high_housing",
        "percentage_of_households_with_high_housing_costs",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2019:2023 & variable == "percentage_of_households_with",
        "percentage_of_households_with_overcrowding",
        variable
      ),
    values =
      if_else(
        release_year %in% 2019:2023 & variable == "preventable_hospital_stays" & stem == "raw_value",
        values / 100,
        values
      ),
    variable =
      if_else(
        release_year %in% 2016:2023 & variable == "drinking_water_violations",
        "drinking_water_violations_bin",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2011:2023 & variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2011:2023 & variable == "ratio_of_population_to_primary_care",
        "primary_care_physicians",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2011:2023 & variable == "ratio_of_population_to_mental_health",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2011:2023 & variable == "ratio_of_population_to_mental_health",
        "mental_health_providers",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2012:2023 & variable == "ratio_of_population_to",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2012:2023 & variable == "ratio_of_population_to",
        "dentists",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2014:2023 & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2014:2023 & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "other_primary_care_providers",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2020:2023 & variable == "lbw",
        "low_birthweight",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2020:2023 & variable == "crude_suicide",
        "raw_value",
        stem
      ),
    stem =
      if_else(
        release_year %in% 2020:2023 & variable == "number_of_juvenile_delinquency_cases_formally_processed_by_a_juvenile",
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2020:2023 & variable == "number_of_juvenile_delinquency_cases_formally_processed_by_a_juvenile",
        "number_of_juvenile_delinquency_cases_formally_processed_by_a_juvenile_court",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2020:2023 & variable == "number_of_informally_handled_juvenile_delinquency",
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2020:2023 & variable == "number_of_informally_handled_juvenile_delinquency",
        "number_of_informally_handled_juvenile_delinquency_cases",
        variable
      )
  )

# Need to turn liquor store density into per 100k people for both 2010 and 2011.
liquor_store_harmonize_2010 <-
  county_level_harmonize %>%
  filter(release_year == 2010, variable == "liquor_store_density") %>%
  select(-race) %>%
  pivot_wider(names_from = c("variable", "stem"), values_from = c("values")) %>%
  mutate(
    liquor_store_density_raw_value =
      if_else(
        !is.na(liquor_store_density_numerator),
        liquor_store_density_numerator * 100000 / liquor_store_density_denominator,
        0
      )
  ) %>%
  pivot_longer(
    cols = -matches("state|county|fips|year"),
    names_to = "variable",
    values_to = "values"
  ) %>%
  separate_wider_regex(
    variable,
    c(
      variable = ".+",
      "_",
      stem = "raw_value|numerator|denominator|ci_low|ci_high"
    ),
    too_few = "align_start"
  ) %>%
  mutate(race = NA)

liquor_store_harmonize_2011 <-
  county_level_harmonize %>%
  select(-race) %>%
  filter(release_year == 2011, variable == "liquor_store_density") %>%
  pivot_wider(names_from = c("variable", "stem"), values_from = c("values")) %>%
  mutate(
    liquor_store_density_raw_value =
      liquor_store_density_numerator * 100000 / liquor_store_density_denominator
  ) %>%
  pivot_longer(
    cols = -matches("state|county|fips|year"),
    names_to = "variable",
    values_to = "values"
  ) %>%
  separate_wider_regex(
    variable,
    c(
      variable = ".+",
      "_",
      stem = "raw_value|numerator|denominator|ci_low|ci_high"
    ),
    too_few = "align_start"
  ) %>%
  mutate(race = NA)

county_level_harmonize <-
  county_level_harmonize %>%
  filter(release_year != 2010 | variable != "liquor_store_density") %>%
  filter(release_year != 2011 | variable != "liquor_store_density") %>%
  bind_rows(list(liquor_store_harmonize_2010, liquor_store_harmonize_2011))

######### Remove duplicate entries for access to healthy foods variable in 2012.
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

write_csv(county_level_harmonize,
          file = here("health_data", "src", "county_level_data_harmonized.csv"))
