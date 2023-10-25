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
  select(-values.y, -replace, -cross_or_longitudinal) %>%
  rename(values = values.x)

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
  select(-values.x, -replace, -cross_or_longitudinal) %>%
  rename(values = values.y)

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
