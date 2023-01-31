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

# Fixed effects models where we are estimating crime----------------------------
FE_crime <-
  plm(
    county_crime ~ I(-income_std) + county_jail_adm_std +
      county_prison_adm_std + hispanics_std + log_education_std + blacks_std +
      race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

# Examining different ways of estimating the standard error---------------------
coeftest(
  FE,
  vcov = vcovHC(x = FE, method = "arellano", type = "HC0", cluster = "group")
)
