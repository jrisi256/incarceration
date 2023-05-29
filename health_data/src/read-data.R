################################################################# Load packages
library(here)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

################################# Read in state and county health rankings data
years <- 2010:2023
data_dir <- here("health_data", "src", "data")
files <- here(data_dir, paste0("health-data_", years, ".csv"))
names(files) <- years
health_data_list <- map(files, read_csv)

############################ Concatenate all the data into one large data frame
health_data_dfs <- map_dfr(health_data_list, function(df) {
  df %>%
    select(-"County Ranked (Yes=1/No=0)") %>%
    filter(`5-digit FIPS Code` != "fipscode") %>%
    pivot_longer(
      cols = -matches("FIPS|State|Name|Release Year"),
      names_to = "variables",
      values_to = "values"
    ) %>%
    rename(state_fips = "State FIPS Code", county_fips = "County FIPS Code",
           full_fips = "5-digit FIPS Code", state_abb = "State Abbreviation",
           county_name = "Name", release_year = "Release Year") %>%
    separate_wider_regex(
      variables,
      c(
        variable = ".+",
        " ",
        stem = "raw value|numerator|denominator|CI low|CI high"
      ),
      too_few = "align_start"
    ) %>%
    mutate(variable = str_replace_all(tolower(variable), " ", "_"),
           stem = str_replace_all(tolower(stem), " ", "_"))
})

#### Separate national, state, and county level data (focused mostly on county)
nation_level <- health_data_dfs %>% filter(full_fips == "00000")

state_level <-
  health_data_dfs %>%
  filter(full_fips != "00000" & str_sub(full_fips, 3, 5) == "000")

county_level <- health_data_dfs %>% filter(str_sub(full_fips, 3, 5) != "000")

################################################### Harmonize data across years
county_level_harmonize <-
  county_level %>%
  mutate(
    variable =
      if_else(
        release_year == 2010 & variable == "chlamydia_rate",
        "sexually_transmitted_infections",
        variable
      ),
    variable = 
      if_else(
        release_year == 2010 & variable == "single-parent_households",
        "children_in_single-parent_households",
        variable
      )
  )

# In 2011, they drop the primary_care_provider_rate_per_100000 variable. The raw rate for 
# primary_care_physicians also gets reported (strangely as the number of physicians per person).
# Need to harmonize.

# Have to harmonize liquor store values. 2010 is per 10k people, 2011 is per 100k people.
