library(plm)
library(here)
library(dplyr)
library(readr)
library(lmtest)
library(stargazer)

# Load data---------------------------------------------------------------------
data <-
  read_csv(here("replication", "input", "county_data.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips)
  )

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
  mutate(across(matches("std"), ~ lag(.x), .names = "{.col}_lag"))

# Fixed effects model with clustered standard errors----------------------------
FE <-
  plm(
    log(drugs) ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      log_education_std + blacks_std + race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_crime <-
  plm(
    county_crime ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      log_education_std + blacks_std + race_other_std + drugs_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

# Joe Work
FE_education <-
  plm(
    log_education ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      drugs_std + blacks_std + race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income <-
  plm(
    log(income) ~ log_education_std + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      drugs_std + blacks_std + race_other_std + county_crime_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(FE, FE_education, FE_income, FE_crime,
          column.labels = c("Drug Overdose Deaths", "Fraction High School Graduates", "Median Income", "Homicide Rate (CDC)"),
          se = list(coeftest(FE, vcov = vcovHC(FE, "arellano"))[, 2],
                    coeftest(FE_education, vcov = vcovHC(FE_education, "arellano"))[, 2],
                    coeftest(FE_income, vcov = vcovHC(FE_income, "arellano"))[, 2],
                    coeftest(FE_crime, vcov = vcovHC(FE_crime, "arellano"))[, 2]),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          out = "main_regressions.txt",
          type = "text")

# Let's estimate models using the opioid prescription rate (different sample)
##################################################################################
FE_px <-
  plm(
    log(drugs) ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      log_education_std + blacks_std + race_other_std + county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

# Joe Work
FE_education_px <-
  plm(
    log_education ~ I(-income_std) + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      drugs_std + blacks_std + race_other_std + county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_px <-
  plm(
    log(income) ~ log_education_std + county_jail_adm_std + county_prison_adm_std + hispanics_std +
      drugs_std + blacks_std + race_other_std + county_crime_std + px_std + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(FE_px, FE_education_px, FE_income_px,
          column.labels = c("Drug Overdose Deaths", "Fraction High School Graduates", "Median Income"),
          se = list(coeftest(FE_px, vcov = vcovHC(FE_px, "arellano"))[, 2],
                    coeftest(FE_education_px, vcov = vcovHC(FE_education_px, "arellano"))[, 2],
                    coeftest(FE_income_px, vcov = vcovHC(FE_income_px, "arellano"))[, 2]),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          out = "opioid_prescription.txt",
          type = "text")

# Let's estimate models using 1 year lags
##################################################################################
FE_lag <-
  plm(
    log(drugs) ~ I(-income_std_lag) + county_jail_adm_std_lag + county_prison_adm_std_lag + hispanics_std_lag +
      log_education_std_lag + blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

# Joe Work
FE_education_lag <-
  plm(
    log_education ~ I(-income_std_lag) + county_jail_adm_std_lag + county_prison_adm_std_lag + hispanics_std_lag +
      drugs_std_lag + blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

FE_income_lag <-
  plm(
    log(income) ~ log_education_std_lag + county_jail_adm_std_lag + county_prison_adm_std_lag + hispanics_std_lag +
      drugs_std_lag + blacks_std_lag + race_other_std_lag + county_crime_std_lag + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(FE_lag, FE_education_lag, FE_income_lag,
          column.labels = c("Drug Overdose Deaths", "Fraction High School Graduates", "Median Income"),
          se = list(coeftest(FE_lag, vcov = vcovHC(FE_lag, "arellano"))[, 2],
                    coeftest(FE_education_lag, vcov = vcovHC(FE_education_lag, "arellano"))[, 2],
                    coeftest(FE_income_lag, vcov = vcovHC(FE_income_lag, "arellano"))[, 2]),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 4,
          out = "lagged_models.txt",
          type = "text")
