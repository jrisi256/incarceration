library(here)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

#################################################################
##            Read in (cross-sectional) health data            ##
#################################################################
years <- 2010:2023
data_dir <- here("clean_chrr-wphi", "input", "data")
files <- here(data_dir, paste0("health-data_", years, ".csv"))
names(files) <- years
health_data_list <- map(files, read_csv)

########################################################################
##  Turn liquor store density into per 100k people for 2010 and 2011  ##
########################################################################
health_data_list$`2010` <-
  health_data_list$`2010` %>%
  filter(`Liquor store density raw value` != "v041_rawvalue") %>%
  mutate(
    `Liquor store density raw value` =
      as.numeric(`Liquor store density raw value`) * 10
  ) %>%
  mutate(across(everything(), as.character))

# Do the full calculation because in the original release they round.
health_data_list$`2011` <-
  health_data_list$`2011` %>%
  filter(`Liquor store density raw value` != "v041_rawvalue") %>%
  mutate(
    `Liquor store density raw value` =
      as.numeric(`Liquor store density raw value`),
    `Liquor store density numerator` =
      as.numeric(`Liquor store density numerator`),
    `Liquor store density denominator` =
      as.numeric(`Liquor store density denominator`),
    `Liquor store density raw value` =
      `Liquor store density numerator` *
        100000 /
        `Liquor store density denominator`
  ) %>%
  mutate(across(everything(), as.character))

##############################################################################
##  Remove duplicate entries for access to healthy foods variable in 2012.  ##
##############################################################################
health_data_list$`2012` <-
  health_data_list$`2012` %>%
  select(
    -"Access to healthy foods raw value...149",
    -"Access to healthy foods numerator...150",
    -"Access to healthy foods denominator...151",
    -"Access to healthy foods CI low...152",
    -"Access to healthy foods CI high...153"
  )

#########################################################################
##                  Concatenate into one data frame.                   ##
##  Transform the data into a long data frame from a wide data frame.  ##
#########################################################################
health_data_dfs <-
  pmap_dfr(
    list(health_data_list, names(health_data_list)), function(df, yr) {
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

      # Identify race, stem, and variable names
      race_str <-
        paste0(
          "\\(AIAN\\)|\\(White\\)|\\(Black\\)|\\(Hispanic\\)|",
          "\\(Asian/Pacific Islander\\)|\\(Asian\\)"
        )

      stem_str <-
        "raw value|numerator|denominator|CI high|CI low|flag|unreliable"

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

##################################################################
##                 Keep only county-level data                  ##
##################################################################
county_level <- health_data_dfs %>% filter(str_sub(full_fips, 3, 5) != "000")

##################################################################
##                 Harmonize data across years.                 ##
##################################################################
county_harmonize <-
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
        release_year == 2010 &
          variable == "primary_care_provider_rate_per_100000",
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year == 2010 &
          variable == "primary_care_provider_rate_per_100000",
        "primary_care_provider_rate",
        variable
      ),
    stem =
      if_else(
        release_year == 2010 &
          variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year == 2010 &
          variable == "ratio_of_population_to_primary_care",
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
        release_year %in% 2011:2018 &
          variable == "mammography_screening" ~ "mammography_screening_67_69",
        release_year %in% 2019:2023 &
          variable == "mammography_screening" ~ "mammography_screening_65_74",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2010:2018 & variable == "preventable_hospital_stays" ~
          "preventable_hospital_stays_10_18",
        release_year %in% 2019:2023 & variable == "preventable_hospital_stays" ~
          "preventable_hospital_stays_19_23",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2019 &
          variable == "%_american_indian_and_alaskan_native" ~
          "%_american_indian_or_alaska_native",
        release_year %in% 2020:2022 &
          variable == "%_american_indian_&_alaska_native" ~
          "%_american_indian_or_alaska_native",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2021 & variable == "%_females" ~ "%_female",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2022 &
          variable == "%_native_hawaiian/other_pacific_islander" ~
          "%_native_hawaiian_or_other_pacific_islander",
        T ~ variable
      ),
    variable =
      case_when(
        release_year %in% 2011:2019 &
          variable == "%_non-hispanic_african_american" ~
          "%_non-hispanic_black",
        T ~ variable
      ),
    variable =
      if_else(
        release_year %in% 2019:2023 &
          variable == "percentage_of_households_with_high_housing",
        "percentage_of_households_with_high_housing_costs",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2019:2023 &
          variable == "percentage_of_households_with",
        "percentage_of_households_with_overcrowding",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2019:2023 & variable == paste0(
          "percentage_of_households_with_lack_of_kitchen_or_plumbing_",
          "facilities"
        ),
        "percentage_of_households_with_lack_of_kitchen_or_plumbing",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2016:2023 & variable == "drinking_water_violations",
        "drinking_water_violations_bin",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2011:2023 &
          variable == "ratio_of_population_to_primary_care",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2011:2023 &
          variable == "ratio_of_population_to_primary_care",
        "primary_care_physicians",
        variable
      ),
    stem =
      if_else(
        release_year %in% 2011:2023 &
          variable == "ratio_of_population_to_mental_health",
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2011:2023 &
          variable == "ratio_of_population_to_mental_health",
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
        release_year %in% 2014:2023 &
          variable == paste0(
            "ratio_of_population_to_primary_care_providers_other_",
            "than"
          ),
        "ratio",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2014:2023 &
          variable == paste0(
            "ratio_of_population_to_primary_care_providers_other_",
            "than"
          ),
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
    values =
      if_else(
        release_year %in% 2020:2023 & values < 0 &
          variable %in%
            c(
              paste0(
                "number_of_juvenile_delinquency_cases_formally_processed_by_a",
                "_juvenile"
              ),
              "number_of_informally_handled_juvenile_delinquency"
            ),
        NA_real_,
        values
      ),
    stem =
      if_else(
        release_year %in% 2020:2023 &
          variable == paste0(
            "number_of_juvenile_delinquency_cases_formally_processed_by_a_",
            "juvenile"
          ),
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2020:2023 &
          variable == paste0(
            "number_of_juvenile_delinquency_cases_formally_processed_by_a_",
            "juvenile"
          ),
        paste0(
          "number_of_juvenile_delinquency_cases_formally_processed_by_a_",
          "juvenile_court"
        ),
        variable
      ),
    stem =
      if_else(
        release_year %in% 2020:2023 &
          variable == "number_of_informally_handled_juvenile_delinquency",
        "raw_value",
        stem
      ),
    variable =
      if_else(
        release_year %in% 2020:2023 &
          variable == "number_of_informally_handled_juvenile_delinquency",
        "number_of_informally_handled_juvenile_delinquency_cases",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2022 & variable == "childcare_cost_burden",
        "child_care_cost_burden",
        variable
      ),
    variable =
      if_else(
        release_year %in% 2022 & variable == "childcare_centers",
        "child_care_centers",
        variable
      ),
    values =
      if_else(
        release_year == 2013 & variable == "infant_mortality",
        values / 100,
        values
      )
  )

county_harmonize <-
  county_harmonize %>%
  mutate(race = if_else(is.na(race), "all", race))

#################################################################
##            Dichotomize drinking water violations            ##
#################################################################
drinking_water_bin <-
  county_harmonize %>%
  filter(variable == "drinking_water_violations", stem == "raw_value") %>%
  mutate(drinking_water_violations_bin = if_else(values > 0, 1, 0)) %>%
  select(-variable, -values) %>%
  pivot_longer(
    drinking_water_violations_bin,
    names_to = "variable",
    values_to = "values"
  )

county_harmonize <- bind_rows(county_harmonize, drinking_water_bin)

##################################################################
##                        Write out data                        ##
##################################################################
if (!dir.exists(here("clean_chrr-wphi", "output", "harmonize"))) {
  dir.create(here("clean_chrr-wphi", "output", "harmonize"))
}

write_csv(
  county_harmonize,
  file =
    here(
      "clean_chrr-wphi",
      "output",
      "harmonize",
      "harmonize_cross-section.csv"
    )
)
