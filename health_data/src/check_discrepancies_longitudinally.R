library(here)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)

county_harmonize <-
  read_csv(here("health_data", "src", "county_level_data_harmonized.csv"))

year_matching <- read_csv(here("health_data", "src", "year_matching.csv"))

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

says # county level
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
