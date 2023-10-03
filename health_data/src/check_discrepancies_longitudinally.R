library(here)
library(dplyr)
library(readr)
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

# Nation level
nation_level_longitudinal <-
  county_harmonize %>%
  filter(stem == "raw_value", is.na(race)) %>%
  select(-race) %>%
  filter(!(variable %in% wi_fl_ny_only$variable)) %>%
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

a <- county_level %>% filter(str_detect(variable, "african|c_black")) %>% distinct(variable, release_year)
