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

############# Concatenate all the data from 2010-2019 into one large data frame.
###################################### Data from 2020-2023 on are too different.
health_data_list_2010_2019 <- health_data_list[as.character(c(2010:2019))]

health_data_dfs_2010_2019 <-
  pmap_dfr(
    list(health_data_list_2010_2019, names(health_data_list_2010_2019)),
    function(df, yr) {
      df %>%
        select(-"County Ranked (Yes=1/No=0)") %>%
        filter(`5-digit FIPS Code` != "fipscode") %>%
        pivot_longer(
          cols = -matches("FIPS|State|Name|Release Year"),
          names_to = "variables",
          values_to = "values"
        ) %>%
        rename(state_fips = "State FIPS Code",
               county_fips = "County FIPS Code",
               full_fips = "5-digit FIPS Code",
               state_abb = "State Abbreviation",
               county_name = "Name",
               release_year = "Release Year") %>%
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
               values = as.numeric(values),
               release_year = yr)
    })

###################### Separate out county level data (focused mostly on county)
county_level_2010_2019 <-
  health_data_dfs_2010_2019 %>%
  filter(str_sub(full_fips, 3, 5) != "000")

#################################################### Harmonize data across years
############## Change chlamydia rate in 2010 to sexually transmitted infections.
########################## Change income inequality to gini coefficient in 2010.
############# Change lead poisoned children from 2010-2012 to match later years.
########### Change primary_care_provider_rate stems and variable names for 2010.
####### Change stem values for severe housing problems variables from 2019-2019.
### Recalculate preventable hospital stays to be per 1000 people from 2019-2019.
######## Change water violations name to reflect its new meaning from 2016-2019.
######### Change primary_care_physician stems and variable names from 2011-2019.
######### Change mental_health_provider stems and variable names from 2011-2019.
### Change other_primary_care_providers stems and variable names from 2014-2019.
####################### Change dentists stems and variable names from 2012-2019.
county_level_harmonize_2010_2019 <-
  county_level_2010_2019 %>%
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
        release_year %in% c(2019) &
          variable %in% c(
            "percentage_of_households_with_high_housing",
            "percentage_of_households_with",
            "percentage_of_households_with_lack_of_kitchen_or_plumbing"
          ),
        "raw_value",
        stem
      ),
    variable = 
      if_else(
        release_year %in% c(2019) & variable == "percentage_of_households_with_high_housing",
        "percentage_of_households_with_high_housing_costs",
        variable
      ),
    variable =
      if_else(
        release_year %in% c(2019) & variable == "percentage_of_households_with",
        "percentage_of_households_with_overcrowding",
        variable
      ),
    values =
      if_else(
        release_year %in% c(2019) & variable == "preventable_hospital_stays" & stem %in% c("raw_value", "white", "black", "hispanic"),
        values / 100,
        values
      ),
    variable = 
      if_else(
        release_year %in% c(2016, 2017, 2018, 2019) & variable == "drinking_water_violations",
        "drinking_water_violations_bin",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
    ),
    variable = 
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_primary_care",
        "primary_care_physicians",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_mental_health",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_mental_health",
        "mental_health_providers",
        variable
      ),
    stem =
      if_else(
        release_year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to",
        "dentists",
        variable
        ),
    stem = 
      if_else(
        release_year %in% c(2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% c(2014, 2015, 2016, 2017, 2018, 2019) & variable == "ratio_of_population_to_primary_care_providers_other_than",
        "other_primary_care_providers",
        variable
      )
  )

# Need to turn liquor store density into per 100k people for both 2010 and 2011.
liquor_store_harmonize_2010 <-
  county_level_harmonize_2010_2019 %>%
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
  ) %>%
  separate_wider_regex(
    variable,
    c(
      variable = ".+",
      "_",
      stem = "raw_value|numerator|denominator|ci_low|ci_high"
    ),
    too_few = "align_start"
  )

liquor_store_harmonize_2011 <-
  county_level_harmonize_2010_2019 %>%
  filter(release_year == 2011, variable == "liquor_store_density") %>%
  pivot_wider(names_from = c("variable", "stem"), values_from = c("values")) %>%
  mutate(liquor_store_density_raw_value =
           liquor_store_density_numerator * 100000 / liquor_store_density_denominator) %>%
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
  )

county_level_harmonize_2010_2019 <-
  county_level_harmonize_2010_2019 %>%
  filter(release_year != 2010 | variable != "liquor_store_density") %>%
  filter(release_year != 2011 | variable != "liquor_store_density") %>%
  bind_rows(list(liquor_store_harmonize_2010, liquor_store_harmonize_2011))

######### Remove duplicate entries for access to healthy foods variable in 2012.
health_foods_harmonize_2012 <-
  county_level_harmonize_2010_2019 %>%
  filter(release_year == 2012) %>%
  filter(str_detect(variable, "^access_to_healthy_foods")) %>%
  group_by(full_fips, variable, stem) %>%
  distinct(.keep_all = T) %>%
  group_by(full_fips, variable, stem) %>%
  filter(!is.na(values) | n() == 1) %>%
  ungroup()

county_level_final_2010_2019 <-
  county_level_harmonize_2010_2019 %>%
  filter(release_year != 2012 | variable != "access_to_healthy_foods") %>%
  bind_rows(health_foods_harmonize_2012)

############# Concatenate all the data from 2020-2023 into one large data frame.
health_data_list_2020_2023 <- health_data_list[as.character(c(2020:2023))]

health_data_dfs_2020_2023 <-
  pmap_dfr(
    list(health_data_list_2020_2023, names(health_data_list_2020_2023)),
    function(df, yr) {
      df %>%
        select(-"County Ranked (Yes=1/No=0)") %>%
        filter(`5-digit FIPS Code` != "fipscode") %>%
        pivot_longer(
          cols = -matches("FIPS|State|Name|Release Year"),
          names_to = "variables",
          values_to = "values"
        ) %>%
        rename(state_fips = "State FIPS Code",
               county_fips = "County FIPS Code",
               full_fips = "5-digit FIPS Code",
               state_abb = "State Abbreviation",
               county_name = "Name",
               release_year = "Release Year") %>%
        mutate(original = variables) %>%
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
               values = as.numeric(values),
               release_year = yr)
    })

county_level_2020_2023 <-
  health_data_dfs_2020_2023 %>%
  filter(str_sub(full_fips, 3, 5) != "000")

race <- "\\(AIAN\\)|\\(White\\)|\\(Black\\)|\\(Hispanic\\)|\\(Asian/Pacific Islander\\)"
locate <- str_locate(a2020$original, race)

stem_str <- "numerator|denominator|CI high|CI low|flag|unreliable"

a2020 <-
  county_level_2020_2023 %>%
  filter(release_year == 2020, full_fips == "01001") %>%
  mutate(start = locate[, "start"],
         end = locate[, "end"]) %>%
  mutate(start = if_else(str_detect(original, stem_str), NA_integer_, start)) %>%
  mutate(why = str_detect(original, stem_str))

# pre mature death, low birth weight, teen births, preventable hospital stays, mammography screening, flu vaccinations, children in poverty, injury deaths, driving alone to work (available by race)
# life expectancy, premature age adjusted mortality, child mortality, infant mortality, drug overdose deaths, motor vehicle crash deaths, reading scores, math scores, median household income, homicides, suicides, firearm fatalities (available by race)
#
# low birth weight (unreliable)
# primary care physicians, dentists, mental health providers, preventable hospital stays, drinking water violations, other doctors (harmonize)
#
# expand racial categories!
# severe housing problems could be a problem.
# pre mature death, low birth weight, teen births, injury deaths, driving alone to work have a million categories!
# children in poverty racial categories come from acs (main value comes from saipe)!
