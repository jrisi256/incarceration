##################################################################
##                     Fix duplicate values                     ##
##################################################################

# Connects release year to actual years of data coverage
year_matching <-
  read_csv(
    here("clean_chrr-wphi", "output", "year_matching.csv"),
    col_types =
      cols_only(
        release_year = "c",
        start_year = "c",
        end_year = "c",
        variable = "c"
      )
  ) %>%
  filter(!is.na(start_year))

county_level_harmonize_year_match <-
  county_level_harmonize %>%
  inner_join(year_matching, by = c("variable", "release_year"))

# Connects release year to actual years of data coverage by state
year_state_matching <-
  read_csv(
    here("clean_chrr-wphi", "output", "year_state_matching.csv"),
    col_types = cols(release_year = "c", start_year = "c", end_year = "c")
  )

county_level_harmonize_year_state_match <-
  county_level_harmonize %>%
  inner_join(
    year_state_matching,
    by = c("variable", "release_year", "state_abb")
  )

county_level_harmonize_distinct <-
  bind_rows(
    county_level_harmonize_year_match,
    county_level_harmonize_year_state_match
  ) %>%
  select(-state_abb, -state_fips, -county_name, -county_fips) %>%
  group_by(full_fips, start_year, end_year, variable, stem, race) # %>%
# mutate(n_distinct = n_distinct(values),
#        n = n())

# When there is a duplicate entry, keep the one from the latest release.
county_level_harmonize_latest <-
  county_level_harmonize_distinct %>%
  # select(-n, -n_distinct) %>%
  filter(release_year == max(release_year)) %>%
  ungroup()

# When there is a duplicate entry, keep the one from the earliest release.
county_level_harmonize_earliest <-
  county_level_harmonize_distinct %>%
  # select(-n, -n_distinct) %>%
  filter(release_year == min(release_year)) %>%
  ungroup()

compare <-
  county_level_harmonize_latest %>%
  filter(stem == "raw_value") %>%
  full_join(
    filter(county_level_harmonize_earliest, stem == "raw_value"),
    by = c("full_fips", "variable", "stem", "race", "start_year", "end_year")
  ) %>%
  mutate(
    match =
      case_when(
        is.na(values.x) & is.na(values.y) ~ "both missing",
        is.na(values.x) & !is.na(values.y) ~ "latest missing",
        !is.na(values.x) & is.na(values.y) ~ "earliest missing",
        near(values.x, values.y) ~ "match",
        !near(values.x, values.y) ~ "no match"
      )
  )

compare_tbl <- table(compare$match)

write_csv(
  county_level_harmonize_latest,
  file =
    here(
      "clean_chrr-wphi",
      "output",
      "county_repeated-cross-section_latest.csv"
    )
)

write_csv(
  county_level_harmonize_earliest,
  file =
    here(
      "clean_chrr-wphi",
      "output",
      "county_repeated-cross-section_earliest.csv"
    )
)