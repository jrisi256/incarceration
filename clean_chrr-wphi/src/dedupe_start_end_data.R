library(here)
library(readr)
library(purrr)
library(dplyr)
library(stringr)

##################################################################
##                         Read in data                         ##
##################################################################
names <- list.files(here("clean_chrr-wphi", "output", "dedupe"))
names <- names[c(2)]

files <- list.files(here("clean_chrr-wphi", "output", "dedupe"), full.names = T)
files <- files[c(2)]

deduped_data <- map(as.list(files), read_csv)
names(deduped_data) <- str_replace(names, ".csv", "")

##################################################################
##               Remove duplicate start/end years               ##
##################################################################
dedupe_start_end_year <- function(df, year_select, min_max, name) {
  print(year_select)
  print(min_max)
  print(name)
  
  check_duplicates <-
    df %>%
    distinct(full_fips, variable, start_year, end_year) %>%
    group_by(full_fips, variable, .data[[year_select]]) %>%
    mutate(n = n()) %>%
    ungroup()
  
  one <-
    check_duplicates %>%
    filter(n == 1) %>%
    select(-n) %>%
    left_join(df, multiple = "all") %>%
    mutate(dedupe_start_end_year = "no duplicate")
  
  two_diff <-
    check_duplicates %>%
    filter(n > 1) %>%
    mutate(diff = end_year - start_year) %>%
    group_by(full_fips, variable, .data[[year_select]])
  
  if(min_max == "min") {
    two_diff <-
      two_diff %>%
      filter(diff == min(diff)) %>%
      ungroup() %>%
      select(-n, -diff) %>%
      left_join(df, multiple = "all") %>%
      mutate(dedupe_start_end_year = paste0(min_max, " ", year_select))
  } else if(min_max == "max") {
    two_diff <-
      two_diff %>%
      filter(diff == max(diff)) %>%
      ungroup() %>%
      select(-n, -diff) %>%
      left_join(df, multiple = "all") %>%
      mutate(dedupe_start_end_year = paste0(min_max, " ", year_select))
  }
  
  no_dupes <- bind_rows(one, two_diff)
  return(no_dupes)
}

start_end <- c("start_year", "end_year")
min_max <- c("min", "max")

deduped_start_end <-
  pmap(
    list(
      rep(deduped_data, each = 4),
      as.list(rep(start_end, times = 1 * length(deduped_data), each = 2)),
      as.list(rep(min_max, 2 * length(deduped_data))),
      as.list(rep(names(deduped_data), each = 4))
    ),
    dedupe_start_end_year
  ) %>%
  list_flatten()

#################################################################
##                Generate names of data frames                ##
#################################################################
final_names <- c()

for(name in names(deduped_data)) {
  start_end_names <- paste0(name, "_", start_end)
  min_max_names <- c()
  
  for(new_name in start_end_names) {
    min_max_names <-
      append(min_max_names, paste0(new_name, "-", min_max))
  }
  
  final_names <- append(final_names, min_max_names)
}

rm(start_end_names)
rm(name)
rm(new_name)
rm(min_max_names)

names(deduped_start_end) <- final_names

#################################################################
##                      Write our results                      ##
#################################################################
if (!dir.exists(here("clean_chrr-wphi", "output", "dedupe_start_end"))) {
  dir.create(here("clean_chrr-wphi", "output", "dedupe_start_end"))
}

pwalk(
  list(deduped_start_end, as.list(names(deduped_start_end))),
  function(df, name) {
    write_csv(
      df,
      here(
        "clean_chrr-wphi", "output", "dedupe_start_end", paste0(name, ".csv")
      )
    )
  }
)
