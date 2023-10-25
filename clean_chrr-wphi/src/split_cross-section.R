library(here)
library(dplyr)
library(readr)
library(purrr)

latest <-
  here("clean_chrr-wphi", "output", "county_repeated-cross-section_latest.csv")

earliest <-
  here(
    "clean_chrr-wphi",
    "output",
    "county_repeated-cross-section_earliest.csv"
  )

repeated_cross_section_list <-
  map(list(latest = latest, earliest = earliest), read_csv)

split_data <- function(df, var) {
  
  df <- df %>% filter(variable == var)
  
  if(var == "access_to_exercise_opportunities") {
    df %>%
      mutate(
        variable =
          case_when(
            release_year %in% 2014:2017 ~ paste0(var, "_14_17"),
          release_year %in% 2018:2022 ~ paste0(var, "_18_22"),
          release_year %in% 2023 ~ paste0(var, "_23"),
          T ~ variable
        )
      )
  } else if(var == "access_to_healthy_foods") {
    df %>%
      mutate(
        variable = 
        case_when(
          release_year %in% 2010 ~ paste0(var, "_10"),
          release_year %in% 2011:2012 ~ paste0(var, "_11_12"),
          T ~ variable
        )
      )
  } else if(var == "adult_obesity") {
    df %>%
      mutate(
        variable = 
        case_when(
          release_year %in% 2010:2021 ~ paste0(var, "_10_21"),
          release_year %in% 2022:2023 ~ paste0(var, "_22_23"),
          T ~ variable
        )
      )
  } else if(var == "adult_smoking") {
    df %>%
      mutate(
        variable = 
        case_when(
          release_year %in% 2010:2015 ~ paste0(var, "_10_15"),
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "children_in_single-parent_households") {
    df %>%
      mutate(
        variable = 
          case_when(
            release_year %in% 2011:2015 ~ paste0(var, "_11_15"),
            release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
            release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
            T ~ variable
        )
      )
  } else if(var == "diabetes_prevalence") {
    df %>%
      mutate(
        variable =
          case_when(
            release_year %in% 2011:2021 ~ paste0(var, "_11_21"),
            release_year %in% 2022:2023 ~ paste0(var, "_22_23"),
            T ~ variable
          )
      )
  } else if(var == "disconnected_youth") {
    df %>%
      mutate(
        variable =
          case_when(
            release_year %in% 2017:2018 ~ paste0(var, "_17_18"),
            release_year %in% 2019:2023 ~ paste0(var, "_19_23"),
            T ~ variable
          )
      )
    else if(var == "drinking_water_violations_bin") {
      df %>%
        mutate(
          variable =
            case_when(
              release_year %in% 2013:2017 ~ paste0(var, "_13_17"),
              release_year %in% 2019:2023 ~ paste0(var, "_19_23")
            )
        )
    }
  } else if(var == "execessive_drinking") {
    df %>%
      mutate(
        variable =
          case_when(
        release_year %in% 2011:2015 ~ paste0(var, "_11_15"),
        release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
        release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
        T ~ variable
        )
      )
  } else if(var == "food_insecurity") {
    df %>%
      mutate(
        variable =
          case_when(
        release_year %in% 2014:2020 ~ paste0(var, "_14_20"),
        releaes_year %in% 2021:2023 ~ paste0(var, "_21_23"),
        T ~ variable
      )
    )
  } else if(var == "frequent_mental_distress") {
    df %>%
      mutate(
        variable =
          case_when(
            release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
            release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
            T ~ variable
          )
      )
  } else if(var == "frequent_physical_distress") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "high_school_graduation") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010 ~ paste0(var, "_10"),
          release_year %in% 2011 ~ paste0(var, "_11"),
          release_year %in% 2012:2013 ~ paste0(var, "_12_13"),
          release_year %in% 2014:2018 ~ paste0(var, "_14_18"),
          release_year %in% 2019:2020 ~ paste0(var, "_19_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "insufficient_sleep") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "mental_health_providers") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2011:2013 ~ paste0(var, "_11_13"),
          release_year %in% 2015:2023 ~ paste0(var, "_15_23"),
          T ~ variable
        )
      )
  } else if(var == "motor_vehicle_crash_deaths") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010:2012 ~ paste0(var, "_10_12"),
          release_year %in% 2013:2023 ~ paste0(var, "_13_23"),
          T ~ variable
        )
      )
  } else if(var == "physical_inactivity") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2012:2021 ~ paste0(var, "_12_21"),
          release_year %in% 2022:2023 ~ paste0(var, "_22_23"),
          T ~ variable
        )
      )
  } else if(var == "poor_mental_health_days") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010:2015 ~ paste0(var, "_10_15"),
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "poor_or_fair_health") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010:2015 ~ paste0(var, "_10_15"),
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "poor_physical_health_days") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010:2015 ~ paste0(var, "_10_15"),
          release_year %in% 2016:2020 ~ paste0(var, "_16_20"),
          release_year %in% 2021:2023 ~ paste0(var, "_21_23"),
          T ~ variable
        )
      )
  } else if(var == "primary_care_physicians") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2011:2012 ~ paste0(var, "_11_12"),
          release_year %in% 2013:2023 ~ paste0(var, "_13_23"),
          T ~ variable
        )
      )
  } else if(var == "violent_crime") {
    df %>%
      mutate(
        case_when(
          release_year %in% 2010:2011 ~ paste0(var, "_10_11"),
          release_year %in% 2012:2022 ~ paste0(var, "_12_22"),
          T ~ variable
        )
      )
  }
}

# premature death
# violent crime
# check longitudinal
