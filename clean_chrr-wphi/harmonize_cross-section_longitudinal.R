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
trend_data_list <- map(files, read_csv)

county_repeated_cross_section <-
  read_csv(
    here(
      "clean_chrr-wphi", "output", "county_repeated-cross-section.csv"
    )
  )

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
    trend_data_list$`2021`,
    trend_data_list$`2022`,
    by = c("statecode", "countycode", "measurename", "yearspan")
  ) %>%
  compare_longitudinal_dfs() %>%
  select(matches("match")) %>%
  all()

compare_2022_2023 <-
  inner_join(
    trend_data_list$`2022`,
    trend_data_list$`2023`,
    by = c("statecode", "countycode", "measurename", "yearspan")
  ) %>%
  compare_longitudinal_dfs() %>%
  select(matches("match")) %>%
  all()

#################################################################
##                Combine longitudinal datasets                ##
#################################################################

# Keep variables unique to 2021
trend_data_list$`2021` <-
  trend_data_list$`2021` %>%
  filter(measurename %in% c("Adult obesity", "Physical inactivity"))

# Keep variables unique to 2022
trend_data_list$`2022` <-
  trend_data_list$`2022` %>%
  filter(measurename == "Violent crime rate")

#################################################################
##        Join longitudinal and cross-sectional data           ##
#################################################################

# Transform longitudinal data so it can be merged with cross sectional data
trend_data <-
  bind_rows(trend_data_list) %>%
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
    race = NA_character_
  ) %>%
  select(-measureid, -differflag) %>%
  pivot_longer(
    cols =
      c(
        "numerator",
        "denominator",
        "raw_value",
        "ci_low",
        "ci_high"
      ),
    names_to = "stem",
    values_to = "values"
  ) %>%
  mutate(
    stem =
      case_when(
        variable == "primary_care_physicians" & stem == "raw_value" ~ "ratio",
        variable == "dentists" & stem == "raw_value" ~ "ratio",
        T ~ stem
      )
  )

# Merge longitudinal and cross-sectional data. I have found the differflag
# variable to be unreliable in matching. Sometimes it will says the values are
# different when they are not and vice versa.
cross_sectional_merged <-
  county_repeated_cross_section %>%
  select(-state_fips, -county_fips, -county_name, -state_abb) %>%
  full_join(
    select(trend_data, -state_fips, -county_fips, -county_name, -state_abb),
    by = c("full_fips", "release_year", "variable", "stem", "race")
  ) %>%
  mutate(
    cross_or_long =
      case_when(
        is.na(values.x) & is.na(values.y) ~ "always_use",
        !is.na(values.x) & is.na(values.y) ~ "always_use",
        is.na(values.x) & !is.na(values.y) ~ "missing_c",
        values.x == values.y ~ "always_use",
        values.x != values.y ~ "use_c"
      )
  )

# Keep entries where longitudinal and cross-sectional data do not match.
longitudinal_merged <-
  cross_sectional_merged %>%
  filter(cross_or_long != "always_use") %>%
  select(-values.x) %>%
  rename(values = values.y) %>%
  mutate(
    cross_or_long =
      case_when(
        cross_or_long == "missing_c" ~ "replace_missing_c_with_l",
        cross_or_long == "use_c" ~ "use_l"
      )
  )

# What happens when I join on year of data release and values are different (as compared to release year)?
# Keep only full fips and create separate table for county/state partial FIPs and names
# combine longitudinal and cross-sectional
combined_cross_long  <-
  cross_sectional_merged %>%
  select(-values.y) %>%
  rename(values = values.x) %>%
  bind_rows(longitudinal_merged)

# go through documentation and see what hidden gems emerge
# finally, set the break points in terms of variable comparison









year_matching <-
  read_csv(here("clean_chrr-wphi", "output", "year_matching.csv"),
           col_types = list(nj_year = "c"))

discard_vars <-
  c(
    "Only available for the state of Wisconsin. No documentation available.",
    "Only available for the state of New York. No documentation available.",
    "Only available for the state of Florida. No documentation available."
  )

wi_fl_ny_only <-
  year_matching %>%
  filter(notes %in% discard_vars) %>%
  distinct(variable, .keep_all = T)

clean <-
  county_harmonize %>%
  filter(stem == "raw_value", is.na(race)) %>%
  select(-race) %>%
  filter(!(variable %in% wi_fl_ny_only$variable))

# Nation level
nation_level_longitudinal <-
  clean %>%
  group_by(variable, release_year) %>%
  summarise(mean_value = mean(values, na.rm = T)) %>%
  mutate(standardized_mean_values = scale(mean_value)[, 1]) %>%
  ungroup()

ggplot(
  nation_level_longitudinal,
  aes(
    x = release_year,
    y = standardized_mean_values
  )
) +
  geom_point(aes(color = variable)) +
  geom_line(aes(group = variable, color = variable)) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~variable)

nation_level_longitudinal %>%
  filter(variable == "excessive_drinking") %>%
  ggplot(aes(x = release_year, y = standardized_mean_values)) +
  geom_point() +
  geom_line() +
  theme_bw()

# State level
state_level_trends <-
  map(unique(clean$variable),
      function(col_name, df) {
        df %>%
          filter(variable == col_name) %>%
          group_by(state_fips, variable, release_year) %>%
          summarise(mean_value = mean(values, na.rm = T)) %>%
          mutate(standardized_mean_values = scale(mean_value)[, 1]) %>%
          ungroup()
      },
      df = clean)

names(state_level_trends) <- unique(clean$variable)

ggplot(state_level_trends$motor_vehicle_crash_deaths, aes(x = release_year, y = standardized_mean_values)) +
  geom_point(aes(color = state_fips)) +
  geom_line(aes(color = state_fips, group = state_fips)) +
  theme_bw()

# county level
county_level_trends <-
  map(unique(clean$variable),
      function(col_name, df) {
        df %>%
          filter(variable == col_name) %>%
          group_by(full_fips, variable, release_year) %>%
          summarise(mean_value = mean(values, na.rm = T)) %>%
          mutate(standardized_mean_values = scale(mean_value)[, 1]) %>%
          ungroup()
      },
      df = clean)

names(county_level_trends) <- unique(clean$variable)

ggplot(county_level_trends$premature_death, aes(x = release_year, y = standardized_mean_values)) +
  geom_point(aes(color = full_fips)) +
  geom_line(aes(color = full_fips, group = full_fips)) +
  theme_bw() +
  theme(legend.position = "none")
