# Paper and authors ------------------------------------------------------------

# Title TBD,
# Joseph Risi and Corina Graif

# File--------------------------------------------------------------------------
# Extending the regressions from Nosrati et al. 2019

# Load packages-----------------------------------------------------------------
library(plm)
library(here)
library(dplyr)
library(readr)
library(lmtest)
library(ggplot2)
library(stargazer)

# Load data---------------------------------------------------------------------
data <-
  read_csv(here("nosrati_replication", "input", "county_data_nosrati.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips)
  )

out_dir <- file.path("nosrati_replication", "output", "regressions")

# Standardize data--------------------------------------------------------------
z.data <-
  data %>%
  select(-county) %>%
  mutate(log_education = log(education)) %>%
  mutate(across(
    c(
      "income", "county_jail_adm", "county_prison_adm", "px", "county_crime",
      "education", "blacks", "hispanics", "race_other", "log_education", "drugs"
    ),
    ~ scale(.x)[, 1],
    .names = "{.col}_std"
  )) %>%
  arrange(fips, year) %>%
  group_by(fips) %>%
  mutate(across(matches("std"), ~ dplyr::lag(.x), .names = "{.col}_lag"))

# data frame with no missing incarceration data---------------------------------
no_incarceration <-
  z.data %>%
  filter(!is.na(county_jail_adm) & !is.na(county_prison_adm))

# Fixed effects models ---------------------------------------------------------
FE_drugs <-
  plm(
    log(drugs) ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std +
      hispanics_std + log_education_std + blacks_std + race_other_std +
      county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_drugs_no_i <-
  plm(
    log(drugs) ~ I(-income_std) + hispanics_std + log_education_std +
      blacks_std + race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = no_incarceration
  )
  
FE_education <-
  plm(
    log_education ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_edu_no_i <-
  plm(
    log_education ~ I(-income_std) + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = no_incarceration
  )

FE_income <-
  plm(
    log(income) ~ log_education_std + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_no_i <-
  plm(
    log(income) ~ log_education_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = no_incarceration
  )

stargazer(
  FE_drugs,
  FE_education,
  FE_income,
  column.labels = c(
    "Drug ODs (CDC)",
    "% HS Graduates (Census)",
    "Median Income (Census)"
  ),
  se = list(
    coeftest(FE_drugs, vcov = vcovHC(FE_drugs, "arellano"))[, 2],
    coeftest(FE_education, vcov = vcovHC(FE_education, "arellano"))[, 2],
    coeftest(FE_income, vcov = vcovHC(FE_income, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "ods_edu_income_incarceration.txt"),
  type = "text"
)

stargazer(
  FE_drugs_no_i,
  FE_edu_no_i,
  FE_income_no_i,
  column.labels = c(
    "Drug ODs (CDC) - No incarceration data",
    "% HS Graduates (Census) - No incarceration data",
    "Median Income (Census) - No incarceration data"
  ),
  se = list(
    coeftest(FE_drugs_no_i, vcov = vcovHC(FE_drugs_no_i, "arellano"))[, 2],
    coeftest(FE_edu_no_i, vcov = vcovHC(FE_edu_no_i, "arellano"))[, 2],
    coeftest(FE_income_no_i, vcov = vcovHC(FE_income_no_i, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "ods_edu_income_no_incarceration.txt"),
  type = "text"
)

# Estimate models using the opioid prescription rate (different sample)---------
FE_drugs_px <-
  plm(
    log(drugs) ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std +
      hispanics_std + log_education_std + blacks_std + race_other_std +
      county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_drugs_px_lag <-
  plm(
    log(drugs) ~ I(-income_std_lag) + county_jail_adm_std_lag +
      county_prison_adm_std_lag + hispanics_std_lag + log_education_std_lag +
      blacks_std_lag + race_other_std_lag + county_crime_std_lag + px_std_lag +
      year,
    index = c("fips", "year"),
    model = "within",
    data = px_lag_data
  )

FE_education_px <-
  plm(
    log_education ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_px <-
  plm(
    log(income) ~ log_education_std + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(
  FE_drugs_px,
  FE_drugs_px_lag,
  FE_education_px,
  FE_income_px,
  column.labels = c(
    "Drug Overdose Deaths",
    "Drug Overdose Deaths (Lagged)",
    "Fraction High School Graduates",
    "Median Income"
  ),
  se = list(
    coeftest(FE_drugs_px, vcov = vcovHC(FE_drugs_px, "arellano"))[, 2],
    coeftest(FE_drugs_px_lag, vcov = vcovHC(FE_drugs_px_lag, "arellano"))[, 2],
    coeftest(FE_education_px, vcov = vcovHC(FE_education_px, "arellano"))[, 2],
    coeftest(FE_income_px, vcov = vcovHC(FE_income_px, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "opioid_prescriptions.txt"),
  type = "text"
)

# Estimate models using 1 year lags---------------------------------------------
FE_drugs_lag <-
  plm(
    log(drugs) ~ I(-income_std_lag) + county_jail_adm_std_lag +
      county_prison_adm_std_lag + hispanics_std_lag + log_education_std_lag +
      blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_edu_lag <-
  plm(
    log_education ~ I(-income_std_lag) + county_jail_adm_std_lag +
      county_prison_adm_std_lag + hispanics_std_lag + drugs_std_lag +
      blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_lag <-
  plm(
    log(income) ~ log_education_std_lag + county_jail_adm_std_lag +
      county_prison_adm_std_lag + hispanics_std_lag + drugs_std_lag +
      blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(
  FE_drugs_lag,
  FE_edu_lag,
  FE_income_lag,
  column.labels = c(
    "Drug Overdose Deaths",
    "Fraction High School Graduates",
    "Median Income"
  ),
  se = list(
    coeftest(FE_drugs_lag, vcov = vcovHC(FE_drugs_lag, "arellano"))[, 2],
    coeftest(FE_edu_lag, vcov = vcovHC(FE_edu_lag, "arellano"))[, 2],
    coeftest(FE_income_lag, vcov = vcovHC(FE_income_lag, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "lagged_models.txt"),
  type = "text"
)

# Estimate models using education instead of ln(education)----------------------
FE_drugs_edu <-
  plm(
    log(drugs) ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std +
      hispanics_std + education_std + blacks_std + race_other_std +
      county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_edu_edu <-
  plm(
    log_education ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + drugs_std + blacks_std +
      race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_edu <-
  plm(
    log(income) ~ education_std + county_jail_adm_std + county_prison_adm_std +
      hispanics_std + drugs_std + blacks_std + race_other_std +
      county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(
  FE_drugs_edu,
  FE_edu_edu,
  FE_income_edu,
  column.labels = c(
    "Drug Overdose Deaths - No ln(education)",
    "Fraction High School Graduates - No ln(education)",
    "Median Income - No ln(education)"
  ),
  se = list(
    coeftest(FE_drugs_edu, vcov = vcovHC(FE_drugs_edu, "arellano"))[, 2],
    coeftest(FE_edu_edu, vcov = vcovHC(FE_edu_edu, "arellano"))[, 2],
    coeftest(FE_income_edu, vcov = vcovHC(FE_income_edu, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "education_models.txt"),
  type = "text"
)

# Fixed effects models where we are estimating crime----------------------------
# Compare crime when it is logged vs. not logged using a variety of techniques.
non_zero_min <-
  z.data %>%
  filter(county_crime != 0) %>%
  pull(county_crime) %>%
  min(na.rm = T)

z.data_crime <-
  z.data %>%
  mutate(
    county_crime_const = log(county_crime + 1),
    county_crime_min = log(county_crime + non_zero_min / 2)
  )

# Very few county/year observations have a homicide rate of 0, only 2.26%
prcnt_zero <- prop.table(table(z.data_crime$county_crime == 0)) * 100

FE_crime <-
  plm(
    county_crime ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + log_education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_const <-
  plm(
    county_crime_const ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + log_education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_min <-
  plm(
    county_crime_min ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + log_education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

stargazer(
  FE_crime,
  FE_crime_const,
  FE_crime_min,
  column.labels = c(
    "Homicide Rate (CDC)",
    "Homicide Rate (CDC) - ln(homicide + 1)",
    "Homicide Rate (CDC) - ln(homicide + non-zero minimum / 2)"
  ),
  se = list(
    coeftest(FE_crime, vcov = vcovHC(FE_crime, "arellano"))[, 2],
    coeftest(FE_crime_const, vcov = vcovHC(FE_crime_const, "arellano"))[, 2],
    coeftest(FE_crime_min, vcov = vcovHC(FE_crime_min, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "crime_models.txt"),
  type = "text"
)

# Examine if effects change when we use education rather than ln(education)
FE_crime_edu <-
  plm(
    county_crime ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_const_edu <-
  plm(
    county_crime_const ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_min_edu <-
  plm(
    county_crime_min ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

stargazer(
  FE_crime_edu,
  FE_crime_const_edu,
  FE_crime_min_edu,
  column.labels = c(
    "Homicide Rate (CDC)",
    "Homicide Rate (CDC) - ln(homicide + 1)",
    "Homicide Rate (CDC) - ln(homicide + non-zero minimum / 2)"
  ),
  se = list(
    coeftest(
      FE_crime_edu,
      vcov = vcovHC(FE_crime_edu, "arellano")
    )[, 2],
    coeftest(
      FE_crime_const_edu,
      vcov = vcovHC(FE_crime_const_edu, "arellano")
    )[, 2],
    coeftest(
      FE_crime_min_edu,
      vcov = vcovHC(FE_crime_min_edu, "arellano")
    )[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "crime_models_education.txt"),
  type = "text"
)

# What happens to estimates when we remove education?
FE_crime_no_edu <-
  plm(
    county_crime ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + blacks_std + race_other_std +
      drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_const_no_edu <-
  plm(
    county_crime_const ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + blacks_std + race_other_std +
      drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

FE_crime_min_no_edu <-
  plm(
    county_crime_min ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + blacks_std + race_other_std +
      drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data_crime
  )

stargazer(
  FE_crime_no_edu,
  FE_crime_const_no_edu,
  FE_crime_min_no_edu,
  column.labels = c(
    "Homicide Rate (CDC)",
    "Homicide Rate (CDC) - ln(homicide + 1)",
    "Homicide Rate (CDC) - ln(homicide + non-zero minimum / 2)"
  ),
  se = list(
    coeftest(
      FE_crime_no_edu,
      vcov = vcovHC(FE_crime_no_edu, "arellano")
    )[, 2],
    coeftest(
      FE_crime_const_no_edu,
      vcov = vcovHC(FE_crime_const_no_edu, "arellano")
    )[, 2],
    coeftest(
      FE_crime_min_no_edu,
      vcov = vcovHC(FE_crime_min_no_edu, "arellano")
    )[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 4,
  out = here(out_dir, "crime_models_no_education.txt"),
  type = "text"
)

# Why does education have such a large association with the homicide rate?
corr_edu_crime <- cor(data$county_crime, data$education, use = "complete.obs")

ggplot(z.data, aes(x = education, y = county_crime)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "% with at least a HS education",
       y = "Homicide Rate per 100k")

edu_crime <-
  z.data %>%
  group_by(fips) %>%
  summarise(education = mean(education, na.rm = T),
            county_crime = mean(county_crime, na.rm = T))

ggplot(edu_crime, aes(x = education, y = county_crime)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "% with at least a HS education",
       y = "Homicide Rate per 100k",
       title = "Average Education vs. Average Homicide Rate Per County")

# Examining different ways of estimating the standard error---------------------
coeftest(
  FE,
  vcov = vcovHC(x = FE, method = "arellano", type = "HC0", cluster = "group")
)
