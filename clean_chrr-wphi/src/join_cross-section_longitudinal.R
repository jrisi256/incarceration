library(here)
library(dplyr)
library(readr)
library(tidyr)

##################################################################
##                         Read in data                         ##
##################################################################
cross_section <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "harmonize",
      "harmonize_cross-section.csv"
    )
  )

longitudinal <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "harmonize",
      "harmonize_longitudinal.csv"
    )
  ) %>%
  select(-release_year)

state_names <-
  read_csv(here("clean_chrr-wphi", "output", "county_state_names.csv"))

year_matching <-
  read_csv(
    here("clean_chrr-wphi", "docs", "year_matching.csv"),
    col_types =
      cols_only(
        release_year = "d",
        start_year = "d",
        end_year = "d",
        variable = "c"
      )
  ) %>%
  filter(!is.na(start_year))

year_state_matching <-
  read_csv(
    here("clean_chrr-wphi", "docs", "year_state_matching.csv"),
    col_types = cols(release_year = "d", start_year = "d", end_year = "d")
  )

#################################################################
##             Separate out county and state names             ##
#################################################################
state_county_names <-
  cross_section %>%
  distinct(state_fips, county_fips, full_fips, state_abb, county_name)

one_name <-
  state_county_names %>%
  group_by(full_fips) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  ungroup()

two_name <-
  state_county_names %>%
  group_by(full_fips) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  filter(str_length(county_name) == max(str_length(county_name))) %>%
  mutate(county_name = case_when(
    county_name == "La Salle Parish" ~ "LaSalle Parish",
    county_name == "La Salle County" ~ "LaSalle County",
    T ~ county_name
  )) %>%
  ungroup()

state_county_names_clean <- bind_rows(one_name, two_name) %>% select(-n)

state_county_names_clean_join <-
  longitudinal %>%
  distinct(state_fips, county_fips, full_fips, state_abb, county_name) %>%
  filter(!(full_fips %in% state_county_names_clean)) %>%
  bind_rows(state_county_names_clean)

write_csv(
  state_county_names_clean_join,
  file = here("clean_chrr-wphi", "output", "county_state_names.csv")
)

cross_section <-
  cross_section %>%
  select(-state_abb, -state_fips, -county_name, -county_fips)

longitudinal <-
  longitudinal %>%
  select(-state_fips, -county_fips, -county_name, -state_abb,)

##################################################################
##         Merge release year with years of measurement         ##
##################################################################
cross_section <-
  full_join(
    cross_section,
    select(state_names, full_fips, state_abb),
    by = "full_fips"
  )

cross_section_year_match <-
  inner_join(cross_section, year_matching, by = c("variable", "release_year"))

cross_section_year_state_match <-
  inner_join(
    cross_section,
    year_state_matching,
    by = c("variable", "release_year", "state_abb")
  )

cross_section_year <-
  bind_rows(cross_section_year_match, cross_section_year_state_match) %>%
  select(-state_abb)

#################################################################
##        Join longitudinal and cross-sectional data           ##
#################################################################

# Merge longitudinal and cross-sectional data.
joined <-
  cross_section_year %>%
  full_join(
    longitudinal,
    by = c("full_fips", "start_year", "end_year", "variable", "stem", "race")
  ) %>%
  mutate(
    cross_or_longitudinal =
      case_when(
        is.na(values.x) & is.na(values.y) ~ "both missing",
        !is.na(values.x) & is.na(values.y) ~ "longitudinal missing",
        is.na(values.x) & !is.na(values.y) ~ "cross missing",
        near(values.x, values.y) ~ "match",
        !near(values.x, values.y) ~ "no match"
      )
  )

# Keep cross-sectional estimates in cases where they do not match.
# Keep cross-sectional estimates when longitudinal estimates are missing.
# Keep cross-sectional estimates when they do match.
# Keep cross-sectional estimates when they are both missing.
# Keep longitudinal estimates when cross-sectional estimates are missing.
cross_joined <-
  joined %>%
  filter(cross_or_longitudinal == "cross missing" & stem == "raw_value") %>%
  mutate(replace = T) %>%
  select(-values.x, -values.y, -cross_or_longitudinal, -stem) %>%
  full_join(joined, multiple = "all") %>%
  mutate(
    replace = if_else(is.na(replace), F, replace),
    values.x = if_else(replace, values.y, values.x)
  ) %>%
  select(-values.y, -replace) %>%
  rename(
    values = values.x,
    cross_longitudinal_match = cross_or_longitudinal
  ) %>%
  mutate(
    cross_longitudinal_match =
      if_else(cross_longitudinal_match != "no match", "match", "no match")
  )

# Keep longitudinal estimates in cases where they do not match.
# Keep longitudinal estimates when cross-sectional estimates are missing.
# Keep longitudinal estimates when they do match.
# Keep longitudinal estimates when they are both missing.
# Keep cross-sectional estimates when longitudinal estimates are missing.
longitudinal_joined <-
  joined %>%
  filter(
    cross_or_longitudinal == "longitudinal missing" &
      stem == "raw_value"
  ) %>%
  mutate(replace = T) %>%
  select(-values.x, -values.y, -cross_or_longitudinal, -stem) %>%
  full_join(joined, multiple = "all") %>%
  mutate(
    replace = if_else(is.na(replace), F, replace),
    values.y = if_else(replace, values.x, values.y)
  ) %>%
  select(-values.x, -replace) %>%
  rename(
    values = values.y,
    cross_longitudinal_match = cross_or_longitudinal
  ) %>%
  mutate(
    cross_longitudinal_match =
      if_else(cross_longitudinal_match != "no match", "match", "no match")
  )

#################################################################
##                      Write out results                      ##
#################################################################
if (!dir.exists(here("clean_chrr-wphi", "output", "joined"))) {
  dir.create(here("clean_chrr-wphi", "output", "joined"))
}

write_csv(
  cross_joined,
  here("clean_chrr-wphi", "output", "joined", "cross_joined.csv")
)

write_csv(
  longitudinal_joined,
  here("clean_chrr-wphi", "output", "joined", "longitudinal_joined.csv")
)
