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

#################################################################
##        Join longitudinal and cross-sectional data           ##
#################################################################

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
    race = NA_character_
  ) %>%
  select(
    -measureid,
    -differflag,
    -state_fips,
    -county_fips,
    -county_name,
    -state_abb
  ) %>%
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
      ),
    values =
      case_when(
        variable == "preventable_hospital_stays" & stem == "raw_value" ~
          values / 100,
        T ~ values
      ),
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
      )
  )

# Merge longitudinal and cross-sectional data.
cross_sectional_merged <-
  county_repeated_cross_section %>%
  select(-state_fips, -county_fips, -county_name, -state_abb) %>%
  full_join(
    longitudinal_data,
    by = c("full_fips", "release_year", "variable", "stem", "race")
  ) %>%
  mutate(
    cross_or_longitudinal =
      case_when(
        is.na(values.x) & is.na(values.y) ~ "always_use",
        !is.na(values.x) & is.na(values.y) ~ "always_use",
        is.na(values.x) & !is.na(values.y) ~ "missing_c",
        near(values.x, values.y) ~ "always_use",
        !near(values.x, values.y) ~ "use_c"
      )
  )

# Keep entries where longitudinal and cross-sectional data do not match.
longitudinal_merged <-
  cross_sectional_merged %>%
  filter(cross_or_longitudinal != "always_use") %>%
  select(-values.x) %>%
  rename(values = values.y) %>%
  mutate(
    cross_or_longitudinal =
      case_when(
        cross_or_longitudinal == "missing_c" ~ "replace_missing_c_with_l",
        cross_or_longitudinal == "use_c" ~ "use_l"
      )
  )

# Combine all cases (values match and values do not match)
combined_cross_longitudinal  <-
  cross_sectional_merged %>%
  select(-values.y) %>%
  rename(values = values.x) %>%
  bind_rows(longitudinal_merged)

# What happens when I join on year of data release and values are different (as compared to release year)?
# need to disambiguate between which release is correct and when

# Keep only full fips and create separate table for county/state partial FIPs and names

# finally, set the break points in terms of variable comparison
# separate out preventable hospital stays (measuring two different things)
# violent crime has no release year?
# mammography screening is indeed different (release from longiutindal data which goes back further does not match earlier releases)
# not all longitudinal data has a release year (even when it seems like it should)
# clean out duplicate entries from cross-sectional (since some releases use the same year(s))
# check out drug poisoning vs. drug overdoses








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
  filter
  distinct(variable, .keep_all = T)

clean <-
  county_repeated_cross_section %>%
  filter(stem == "raw_value", is.na(race)) %>%
  select(-race) %>%
  filter(!(variable %in% wi_fl_ny_only$variable))

variables <- unique(clean$variable)

# Nation level
nation_level_longitudinal <-
  clean %>%
  group_by(variable, release_year) %>%
  summarise(mean_value = mean(values, na.rm = T)) %>%
  arrange(factor(variable, levels = variables)) %>%
  #mutate(standardized_mean_values = scale(mean_value)[, 1]) %>%
  ungroup()

pdf(here("nation_trends.pdf"), onefile = T)
map(
  variables,
  function(var, df) {
    df %>%
      filter(variable == var) %>%
      ggplot(aes(x = release_year, y = mean_value)) +
      geom_point() +
      geom_line(aes(group = variable)) +
      theme_bw() +
      labs(y = var)
  },
  df = nation_level_longitudinal
)
dev.off()

clean <- clean %>% arrange(variable)

# State level
state_level_trends <-
  map(unique(clean$variable),
      function(col_name, df) {
        df %>%
          filter(variable == col_name) %>%
          group_by(state_fips, variable, release_year) %>%
          summarise(mean_value = mean(values, na.rm = T)) %>%
          #mutate(standardized_mean_values = scale(mean_value)[, 1]) %>%
          ungroup()
      },
      df = clean)

names(state_level_trends) <- unique(clean$variable)

pdf(here("state_trends.pdf"), onefile = T)
map(
  state_level_trends,
  function(df) {
    var <- unique(df$variable)
    
    df %>%
      ggplot(aes(x = release_year, y = mean_value)) +
      geom_point(aes(color = state_fips)) +
      geom_line(aes(group = state_fips, color = state_fips)) +
      theme_bw() +
      labs(y = var) +
      theme(legend.position = "none")
  }
)
dev.off()
