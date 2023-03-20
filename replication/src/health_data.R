library(plm)
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(Hmisc)
library(GGally)
library(ggplot2)
library(stargazer)
library(ggcorrplot)

# Load data---------------------------------------------------------------------
county_data_nosrati <-
  read_csv(here("nosrati_replication", "input", "county_data_nosrati.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips)
  )

incarceration_trends <-
  read_csv(here("replication", "input", "incarceration_trends.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips)
  ) %>%
  select(total_jail_adm_rate, total_pop_15to64, total_jail_adm, total_jail_dis,
         year, fips) %>%
  arrange(fips, year) %>%
  mutate(across(matches("jail"), list(lag = ~ lag(.)))) %>%
  filter(year == 2017 | year == 2018) %>%
  mutate(year = as.character(year))

# Missing New York City which is really weird-----------------------------------
#
# Also important, the numbers from Nosrati et al. 2019 and the most recent data
# from Vera also do not match, sometimes the discrepancies are quite large.
# Could be worth investigating.
#
# check <-
#   select(county_data, year, fips, state, county, county_jail_adm, county_prison_adm) %>%
#   inner_join(select(incarceration_trends, year, fips, total_jail_adm_rate, total_prison_adm_rate))
# 
# a <- anti_join(check, county_data)
# a2 <- anti_join(county_data, check)

# Read in data dictionary-------------------------------------------------------
data_dictionary <-
  read_csv(here("replication", "input", "PLACES_and_500_Cities__Data_Dictionary.csv")) %>%
  mutate(variable = paste0(MeasureID, "_AdjPrev")) %>%
  arrange(variable)

# Read in health data-----------------------------------------------------------
health_data <- read_csv(here("replication", "input", "health_data_2020.csv"))

# Descriptive statistics--------------------------------------------------------
health_data_des <-
  health_data %>%
  select(CountyFIPS, ACCESS2_AdjPrev, ARTHRITIS_AdjPrev, BINGE_AdjPrev,
         BPHIGH_AdjPrev, BPMED_AdjPrev, CANCER_AdjPrev, CASTHMA_AdjPrev,
         CERVICAL_AdjPrev, CHD_AdjPrev, CHECKUP_AdjPrev, CHOLSCREEN_AdjPrev,
         COLON_SCREEN_AdjPrev, COPD_AdjPrev, COREM_AdjPrev, COREW_AdjPrev,
         CSMOKING_AdjPrev, DENTAL_AdjPrev, DIABETES_AdjPrev, HIGHCHOL_AdjPrev,
         KIDNEY_AdjPrev, LPA_AdjPrev, MAMMOUSE_AdjPrev, MHLTH_AdjPrev,
         OBESITY_AdjPrev, PHLTH_AdjPrev, SLEEP_AdjPrev, STROKE_AdjPrev,
         TEETHLOST_AdjPrev)

labels <- data_dictionary$measure_short_name

stargazer(data.frame(health_data_des),
          title = "Descriptive Statistics",
          digits = 2,
          median = T,
          iqr = T,
          covariate.labels = labels,
          out = here("replication", "output", "health_variables", "descript_table_health.txt"))

health_data_reshape <-
  health_data_des %>%
  pivot_longer(-CountyFIPS, names_to = "variable", values_to = "value") %>%
  left_join(select(data_dictionary, variable, year), by = "variable") %>%
  rename(fips = CountyFIPS) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>%
  full_join(incarceration_trends, by = c("fips", "year")) %>%
  select(-total_jail_adm, -total_jail_adm_lag, -total_jail_dis, -total_jail_dis_lag,
         -total_pop_15to64, -total_jail_adm_rate_lag)

# Plot distribution of values---------------------------------------------------
des <- health_data_des %>% select(-CountyFIPS)

densitychart <- function(df, col, col_name) {
  boxplot_coord <- max(density(df[[col_name]], na.rm = T)$y) / 2
  
  ggplot(df, aes(x = {{ col }})) +
    geom_boxplot(aes(y = -boxplot_coord), width = boxplot_coord) +
    geom_density() +
    labs(x = col_name, y = "density") +
    theme_bw()
}

pdf(here("replication", "output", "health_variables", "density_plots_health.pdf"), onefile = T)
pmap(list(as.list(des), as.list(colnames(des))), densitychart, df = des)
dev.off()

# Correlations------------------------------------------------------------------
pdf(here("replication", "output", "health_variables", "correlation_plots_health.pdf"), onefile = T)
map(colnames(select(health_data_reshape, -year, -fips, -total_jail_adm_rate)), function(col_str, df) {
  
  corr <- round(cor(df[["total_jail_adm_rate"]], df[[col_str]], use = "complete.obs"), 3)
  
  ggplot(df, aes(x = total_jail_adm_rate, y = .data[[col_str]])) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x) +
    theme_bw() +
    annotate(geom = "text", label = paste0("Correlation Coefficient: ", corr),
             x = 250000, y = max(df[[col_str]], na.rm = T))
  
}, df = health_data_reshape)
dev.off()

