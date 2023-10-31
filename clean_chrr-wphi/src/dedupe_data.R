library(here)
library(readr)
library(dplyr)
library(purrr)
library(dtplyr)
library(data.table)

##################################################################
##                         Read in data                         ##
##################################################################
cross_non_split <-
  read_csv(here("clean_chrr-wphi", "output", "joined", "cross_joined.csv"))

# cross_split <-
#   read_csv(here("clean_chrr-wphi", "output", "split", "cross_split.csv"))

longitudinal_non_split <-
  read_csv(
    here("clean_chrr-wphi", "output", "joined", "longitudinal_joined.csv")
  )

# longitudinal_split <-
#   read_csv(
#     here("clean_chrr-wphi", "output", "split", "longitudinal_split.csv")
#   )

##################################################################
##                     Fix duplicate values                     ##
##################################################################
dedupe <- function(df) {
  check_dupes <-
    df %>%
    lazy_dt() %>%
    group_by(full_fips, variable, start_year, end_year, stem, race) %>%
    mutate(n_distinct = n_distinct(values),
           n = n()) %>%
    ungroup() %>%
    filter(!is.na(values) | n_distinct <= 1) %>%
    as_tibble()

  # Only one observation for the given year or year-range.
  one <-
    check_dupes %>%
    filter(n == 1) %>%
    mutate(dedupe = "no duplicate") %>%
    select(-n_distinct, -n)
  
  # Two observations for a given year, but both values are the same. Keep one.
  two_same <-
    check_dupes %>%
    filter(n > 1 & n_distinct == 1) %>%
    distinct(
      full_fips,
      variable,
      stem,
      race,
      start_year,
      end_year,
      .keep_all = T
    ) %>%
    mutate(dedupe = "same value") %>%
    select(-n_distinct, -n)

  # Two observations for a given year, and the values are different.
  # Keep earlier or later value.
  two_different_early <-
    check_dupes %>%
    lazy_dt() %>%
    group_by(full_fips, variable, start_year, end_year, stem, race) %>%
    filter(n_distinct == 2, stem == "raw_value", !any(is.na(values))) %>%
    filter(release_year == min(release_year)) %>%
    ungroup() %>%
    select(-cross_longitudinal_match, -n_distinct, -values, -stem, -n) %>%
    left_join(df, multiple = "all") %>%
    mutate(dedupe = "early") %>%
    as_tibble()
  
  two_different_late <-
    check_dupes %>%
    lazy_dt() %>%
    group_by(full_fips, variable, start_year, end_year, stem, race) %>%
    filter(n_distinct == 2, stem == "raw_value", !any(is.na(values))) %>%
    filter(release_year == max(release_year)) %>%
    ungroup() %>%
    select(-cross_longitudinal_match, -n_distinct, -values, -stem, -n) %>%
    left_join(df, multiple = "all") %>%
    mutate(dedupe = "late") %>%
    as_tibble()
  
  early <- bind_rows(one, two_same, two_missing, two_different_early)
  late <- bind_rows(one, two_same, two_missing, two_different_late)
  return(list(early, late))
}

deduped_data <-
  map(
    list(
      #cross_non_split,
      longitudinal_non_split
    ),
    dedupe
  ) %>%
  list_flatten()

names(deduped_data) <-
  c(
    #"cross_nonsplit_early",
    #"cross_nonsplit_late",
    "longitudinal_nonsplit_early",
    "longitudinal_nonsplit_late"
  )

##################################################################
##                        Write out data                        ##
##################################################################
if (!dir.exists(here("clean_chrr-wphi", "output", "dedupe"))) {
  dir.create(here("clean_chrr-wphi", "output", "dedupe"))
}

pwalk(
  list(deduped_data, as.list(names(deduped_data))),
  function(df, name) {
    write_csv(
      df,
      here("clean_chrr-wphi", "output", "dedupe", paste0(name, ".csv"))
    )
  }
)
