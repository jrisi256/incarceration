library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

##################################################################
##     Read in cross-sectional and longitudinal health data     ##
##################################################################
years <- 2021:2023
data_dir <- here("clean_chrr-wphi", "input", "trend_data")
files <- here(data_dir, paste0("trend-data_", years, ".csv"))
names(files) <- years
longitudinal_data_list <- map(files, read_csv)

##################################################################
##          Test longitudinal data sets for consistency         ##
##################################################################
compare_longitudinal_dfs <- function(joined_df) {
  joined_df %>%
    mutate(
      numerator_match =
        case_when(
          numerator.x == numerator.y ~ T,
          is.na(numerator.x) & is.na(numerator.y) ~ T,
          T ~ F
        ),
      denominator_match =
        case_when(
          denominator.x == denominator.y ~ T,
          is.na(denominator.x) & is.na(denominator.y) ~ T,
          T ~ F
        ),
      rawvale_match =
        case_when(
          rawvalue.x == rawvalue.y ~ T,
          is.na(rawvalue.x) & is.na(rawvalue.y) ~ T,
          T ~ F
        ),
      cilow_match =
        case_when(
          cilow.x == cilow.y ~ T,
          is.na(cilow.x) & is.na(cilow.y) ~ T,
          T ~ F
        ),
      cihigh_match =
        case_when(
          cihigh.x == cihigh.y ~ T,
          is.na(cihigh.x) & is.na(cihigh.y) ~ T,
          T ~ F
        ),
      differflag_match =
        case_when(
          differflag.x == differflag.y ~ T,
          is.na(differflag.x) & is.na(differflag.y) ~ T,
          T ~ F
        ),
      trendbreak_match =
        case_when(
          trendbreak.x == trendbreak.y ~ T,
          is.na(trendbreak.x) & is.na(trendbreak.y) ~ T,
          T ~ F
        )
    )
}

compare_2021_2022 <-
  inner_join(
    longitudinal_data_list$`2021`,
    longitudinal_data_list$`2022`,
    by = c("statecode", "countycode", "measurename", "yearspan")
  ) %>%
  compare_longitudinal_dfs() %>%
  select(matches("match")) %>%
  all()

compare_2022_2023 <-
  inner_join(
    longitudinal_data_list$`2022`,
    longitudinal_data_list$`2023`,
    by = c("statecode", "countycode", "measurename", "yearspan")
  ) %>%
  compare_longitudinal_dfs() %>%
  select(matches("match")) %>%
  all()

#################################################################
##                Combine longitudinal data sets               ##
#################################################################

# Keep variables unique to 2021
longitudinal_data_list$`2021` <-
  longitudinal_data_list$`2021` %>%
  filter(measurename %in% c("Adult obesity", "Physical inactivity"))

# Keep variables unique to 2022
longitudinal_data_list$`2022` <-
  longitudinal_data_list$`2022` %>%
  filter(measurename == "Violent crime rate")

# Add raw values (not just ratios) for primary care doctors + dentists in 2023
longitudinal_data_list$`2023` <-
  longitudinal_data_list$`2023` %>%
  mutate(
    ratio =
      if_else(
        measurename %in% c("Primary care physicians", "Dentists"),
        rawvalue,
        NA_real_
      ),
    rawvalue =
      if_else(
        measurename %in% c("Primary care physicians", "Dentists"),
        numerator / denominator,
        rawvalue
      )
  )

# Transform longitudinal data so it can be merged with cross sectional data
longitudinal_data <-
  bind_rows(longitudinal_data_list) %>%
  filter(countycode != "000") %>%
  rename(
    state_fips = statecode,
    county_fips = countycode,
    county_name = county,
    state_abb = state,
    variable = measurename,
    release_year = chrreleaseyear,
    raw_value = rawvalue,
    ci_low = cilow,
    ci_high = cihigh
  ) %>%
  mutate(
    full_fips = paste0(state_fips, county_fips),
    variable = str_replace_all(tolower(variable), " ", "_"),
    race = "all"
  ) %>%
  select(
    -measureid,
    -differflag,
    -trendbreak
  ) %>%
  pivot_longer(
    cols =
      c(
        "numerator",
        "denominator",
        "raw_value",
        "ci_low",
        "ci_high",
        "ratio"
      ),
    names_to = "stem",
    values_to = "values"
  ) %>%
  mutate(
    variable =
      case_when(
        variable == "mammography_screening" ~ "mammography_screening_65_74",
        T ~ variable
      ),
    variable =
      case_when(
        variable == "unemployment_rate" ~ "unemployment",
        T ~ variable
      ),
    variable =
      case_when(
        variable == "violent_crime_rate" ~ "violent_crime",
        T ~ variable
      ),
    variable =
      case_when(
        variable == "school_funding" ~ "school_funding_adequacy",
        T ~ variable
      ),
    variable =
      case_when(
        variable == "preventable_hospital_stays" ~
          "preventable_hospital_stays_19_23",
        T ~ variable
      )
  ) %>%
  separate_wider_delim(
    yearspan,
    delim = "-",
    names = c("start_year", "end_year"),
    too_few = "align_start"
  ) %>%
  mutate(
    end_year = if_else(is.na(end_year), start_year, end_year),
    start_year = as.numeric(start_year),
    end_year = as.numeric(end_year)
  ) %>%
  filter(stem != "ratio" | !is.na(values))

#################################################################
##                      Write out results                      ##
#################################################################
if (!dir.exists(here("clean_chrr-wphi", "output", "harmonize"))) {
  dir.create(here("clean_chrr-wphi", "output", "harmonize"))
}

write_csv(
  longitudinal_data,
  here("clean_chrr-wphi", "output", "harmonize", "harmonize_longitudinal.csv")
)
