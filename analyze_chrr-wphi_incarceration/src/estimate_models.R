library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(panelr)
library(fixest)
library(stringr)
library(ggplot2)

##################################################################
##                         Read in data                         ##
##################################################################
# Look into jail discharges and male/female juvenile population.
incarceration <-
  read_csv(here("incarceration_trends.csv")) %>%
  select(total_jail_adm_rate, total_prison_adm_rate, year, fips, state) %>%
  mutate(
    fips =
      if_else(str_length(fips) == 4, paste0("0", fips), as.character(fips))
  ) %>%
  pivot_longer(
    cols = c("total_jail_adm_rate", "total_prison_adm_rate"),
    names_to = "variable",
    values_to = "values"
  ) %>%
  filter(!is.na(values)) %>%
  rename(full_fips = fips, state_abb = state, start_year = year) %>%
  mutate(end_year = start_year, range = F)

state_names <-
  read_csv(here("clean_chrr-wphi", "output", "county_state_names.csv")) %>%
  select(full_fips, state_abb)

health_start_year <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "dedupe_start_end",
      "cross_nonsplit_late_start_year-min.csv"
    )
  ) %>%
  filter(stem == "raw_value", !is.na(values), race == "all") %>%
  select(
    -stem,
    -race,
    -cross_longitudinal_match,
    -dedupe,
    -dedupe_start_end_year,
    -release_year
  ) %>%
  full_join(state_names, by = c("full_fips"), multiple = "all") %>%
  mutate(range = if_else(end_year - start_year != 0, T, F))

health_end_year <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "dedupe_start_end",
      "cross_nonsplit_late_end_year-min.csv"
    )
  ) %>%
  filter(stem == "raw_value", !is.na(values), race == "all") %>%
  select(
    -stem,
    -race,
    -cross_longitudinal_match,
    -dedupe,
    -dedupe_start_end_year,
    -release_year
  ) %>%
  full_join(state_names, by = c("full_fips"), multiple = "all") %>%
  mutate(range = if_else(end_year - start_year != 0, T, F))

#################################################################
##                          Join data                          ##
#################################################################
join_start_year <-
  bind_rows(health_start_year, incarceration) %>%
  select(-end_year, -range) %>%
  pivot_wider(names_from = variable, values_from = values) %>%
  rename(children_in_single_parent_households = `children_in_single-parent_households`,
         year = start_year)

join_end_year <-
  bind_rows(health_end_year, incarceration) %>%
  select(-start_year, -range) %>%
  pivot_wider(names_from = variable, values_from = values) %>%
  rename(children_in_single_parent_households = `children_in_single-parent_households`,
         year = end_year)

#################################################################
##                       Estimate models                       ##
#################################################################
a <-
  join_end_year %>%
  select(
    year,
    full_fips,
    state_abb,
    total_jail_adm_rate,
    total_prison_adm_rate,
    homicides,
    children_in_poverty,
    teen_births,
    children_in_single_parent_households,
    child_mortality,
    infant_mortality,
    low_birthweight,
    unemployment
  ) %>%
  mutate(
    teen_births = teen_births * 100,
    children_in_poverty = children_in_poverty * 100000,
    children_in_single_parent_households = children_in_single_parent_households * 100000,
    infant_mortality = infant_mortality * 100,
    low_birthweight = low_birthweight * 100000,
    total_prison_adm_rate = total_prison_adm_rate / 100,
    total_jail_adm_rate = total_jail_adm_rate / 100,
    unemployment = unemployment * 10000
  ) %>%
  #filter(if_all(everything(), ~ !is.na(.x))) %>%
  arrange(full_fips, year) %>%
  group_by(full_fips) %>%
  mutate(
    across(
      total_jail_adm_rate:unemployment, list(lag1 = ~ lag(.x, n = 1))
    ),
    across(
      total_jail_adm_rate:homicides,
      list(
        lag2 = ~ lag(.x, n = 2),
        lag3 = ~ lag(.x, n = 3),
        lag5 = ~ lag(.x, n = 5),
        lag10 = ~ lag(.x, n = 10)
      )
    )
  )

model_unemployment_both <-
  feols(
    unemployment ~
      total_prison_adm_rate_lag1 + I(total_prison_adm_rate_lag1 ^ 2) +
      total_jail_adm_rate_lag1 + I(total_jail_adm_rate_lag1 ^ 2) | full_fips + year,
    data = a
  )

model_single_parent_both <-
  feols(
    children_in_single_parent_households ~
      total_prison_adm_rate_lag1 + I(total_prison_adm_rate_lag1 ^ 2) +
      total_jail_adm_rate_lag1 + I(total_jail_adm_rate_lag1 ^ 2)  | full_fips + year,
    data = a
  )

models <-
  map(
    list(
      teen_births = "teen_births",
      children_in_poverty = "children_in_poverty",
      single_parents = "children_in_single_parent_households",
      child_mortality = "child_mortality",
      infant_mortality = "infant_mortality",
      low_birthweight = "low_birthweight",
      unemployment = "unemployment",
      homicides = "homicides"
    ),
    function(df, dep_var) {
      lag <- paste0(dep_var, "_lag1")
      
      f <- feols(
        as.formula(
          paste(dep_var, " ~ total_prison_adm_rate + I(total_prison_adm_rate ^ 2) | full_fips + year")
        ),
        data = df
      )
      
      f1 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag1 + I(total_prison_adm_rate_lag1 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f2 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag2 + I(total_prison_adm_rate_lag2 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f3 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag3 + I(total_prison_adm_rate_lag3 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f5 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag5 + I(total_prison_adm_rate_lag5 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f10 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag10 + I(total_prison_adm_rate_lag10 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      return(list(f = f, f1 = f1, f2 = f2, f3 = f3, f5 = f5, f10 = f10))
    },
    df = a
  ) %>%
  list_flatten()

graph_coefficients <-
  pmap_dfr(
    list(models, names(models)),
    function(model, name) {
      coefficient <- model$coefficients
      names(coefficient) <- str_replace(names(coefficient), "_lag[0-9]*", "")
      coefficient <- coefficient[["total_prison_adm_rate"]]
      
      ci_df <- confint(model, level = 0.9)
      rownames(ci_df) <- str_replace(rownames(ci_df), "_lag[0-9]*", "")
      ci_df <-
        ci_df %>%
        mutate(variable = rownames(ci_df)) %>%
        filter(variable == "total_prison_adm_rate")

      ci_lower <- ci_df[["5 %"]]
      ci_upper <- ci_df[["95 %"]]
      
      split_name <- str_split_1(name, "_")
      dependent_variable <- paste0(split_name[1:length(split_name) - 1], collapse = "_")
      lag <- split_name[length(split_name)]
      
      tibble(coefficient = coefficient,
             ci_lower = ci_lower,
             ci_upper = ci_upper,
             dependent_variable = dependent_variable,
             lag = lag)
    }
  )

graph_coefficients <-
  graph_coefficients %>%
  mutate(
    lag =
      case_when(
        lag == "f" ~ 0,
        lag == "f1" ~ 1,
        lag == "f2" ~ 2,
        lag == "f3" ~ 3,
        lag == "f5" ~ 5,
        lag == "f10" ~ 10
      )
  )

ggplot(graph_coefficients, aes(y = coefficient, x = lag)) +
  geom_point(aes(color = dependent_variable)) +
  geom_line(aes(color = dependent_variable, group = dependent_variable)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper, color = dependent_variable)) +
  theme_bw() +
  facet_wrap(~dependent_variable, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 5, 10)) +
  theme(legend.position = "none")

models_homicide <-
  map(
    list(
      teen_births = "teen_births",
      children_in_poverty = "children_in_poverty",
      single_parents = "children_in_single_parent_households",
      child_mortality = "child_mortality",
      infant_mortality = "infant_mortality",
      low_birthweight = "low_birthweight",
      unemployment = "unemployment"
    ),
    function(df, dep_var) {
      lag <- paste0(dep_var, "_lag1")
      
      # f <- feols(
      #   as.formula(
      #     paste(dep_var, " ~ homicides + total_prison_adm_rate | full_fips + year")
      #   ),
      #   data = df
      # )
      
      f1 <- feols(
        as.formula(
          paste(
            dep_var, " ~ homicides + total_prison_adm_rate_lag1 + I(total_prison_adm_rate_lag1 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f2 <- feols(
        as.formula(
          paste(
            dep_var, " ~ homicides + total_prison_adm_rate_lag2 + I(total_prison_adm_rate_lag2 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f3 <- feols(
        as.formula(
          paste(
            dep_var, " ~ homicides + total_prison_adm_rate_lag3 + I(total_prison_adm_rate_lag3 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f5 <- feols(
        as.formula(
          paste(
            dep_var, " ~ homicides + total_prison_adm_rate_lag5 + I(total_prison_adm_rate_lag5 ^ 2) | full_fips + year"
          )
        ),
        data = df
      )
      
      f10 <- feols(
        as.formula(
          paste(
            dep_var, " ~ total_prison_adm_rate_lag10 | full_fips + year"
          )
        ),
        data = df
      )
      
      return(list(#f = f, 
                  f1 = f1, f2 = f2, f3 = f3, f5 = f5, f10 = f10))
    },
    df = a
  ) %>%
  list_flatten()

graph_coefficients_homicide <-
  pmap_dfr(
    list(models_homicide, names(models_homicide)),
    function(model, name) {
      coefficient <- model$coefficients
      names(coefficient) <- str_replace(names(coefficient), "_lag[0-9]*", "")
      coefficient <- coefficient[["total_prison_adm_rate"]]
      
      ci_df <- confint(model, level = 0.9)
      rownames(ci_df) <- str_replace(rownames(ci_df), "_lag[0-9]*", "")
      ci_df <-
        ci_df %>%
        mutate(variable = rownames(ci_df)) %>%
        filter(variable == "total_prison_adm_rate")
      
      ci_lower <- ci_df[["5 %"]]
      ci_upper <- ci_df[["95 %"]]
      
      split_name <- str_split_1(name, "_")
      dependent_variable <- paste0(split_name[1:length(split_name) - 1], collapse = "_")
      lag <- split_name[length(split_name)]
      
      tibble(coefficient = coefficient,
             ci_lower = ci_lower,
             ci_upper = ci_upper,
             dependent_variable = dependent_variable,
             lag = lag)
    }
  )

graph_coefficients_homicide <-
  graph_coefficients_homicide %>%
  mutate(
    lag =
      case_when(
        lag == "f" ~ 0,
        lag == "f1" ~ 1,
        lag == "f2" ~ 2,
        lag == "f3" ~ 3,
        lag == "f5" ~ 5,
        lag == "f10" ~ 10
      )
  ) %>%
  filter(lag != 10)

ggplot(graph_coefficients_homicide, aes(y = coefficient, x = lag)) +
  geom_point(aes(color = dependent_variable)) +
  geom_line(aes(color = dependent_variable, group = dependent_variable)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper, color = dependent_variable)) +
  theme_bw() +
  facet_wrap(~dependent_variable, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 5)) +
  theme(legend.position = "none")
