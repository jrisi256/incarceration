# Paper and authors-------------------------------------------------------------

# Economic decline, incarceration, and mortality from drug use disorders
# Elias Nosrati, Jacob Kang-Brown, Michael Ash, Martin McKee, Michael Marmot,
# and Lawrence King

# File--------------------------------------------------------------------------
# Panel regressions

# Load packages-----------------------------------------------------------------
library(plm)
library(lme4)
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
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips),
    year = as.factor(year)
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
      "education", "blacks", "hispanics", "race_other", "log_education"
    ),
    ~ scale(.x)[, 1]
  ))

# Main fixed effects model------------------------------------------------------
FE <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + hispanics +
      log_education + blacks + race_other + county_crime + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

# Display results
P <- FE$coefficients[1:8]
SE <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:8, 2]
p <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:8, 4]
R <- cbind(100 * P, 100 * SE, p)
colnames(R) <- c("Coefficient", "Standard error", "P-value")
rownames(R) <-
  c(
    "Household income decline", "County jail admissions rate",
    "County prison admissions rate", "Fraction Hispanics",
    "Log(Fraction high school graduates)", "Fraction African Americans",
    "Fraction other ethnicity", "Violent crime rate"
  )

stargazer(
  R,
  digits = 3,
  out = here(out_dir, "main_fe_nosrati.txt"),
  type = "text"
)

# Confidence intervals
(R <- cbind(100 * P, 100 * P - 196 * SE, 100 * P + 196 * SE))

# Random effects model----------------------------------------------------------
RE <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education +
      blacks + hispanics + race_other + county_crime + year,
    index = c("fips", "year"),
    model = "random",
    data = z.data
  )

# Multilevel random intercept model---------------------------------------------
ML <-
  lmer(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education +
      blacks + hispanics + race_other + county_crime + year + (1 | fips),
    data = z.data
  )

# Pooled OLS--------------------------------------------------------------------
OLS <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education +
      blacks + hispanics + race_other + county_crime + year,
    index = c("fips", "year"),
    model = "pooling",
    data = z.data
  )

# Compare models----------------------------------------------------------------
stargazer(
  FE,
  RE,
  ML,
  OLS,
  column.labels = c(
    "Fixed Effects",
    "Random Effects",
    "Random Intercept",
    "OLS"
  ),
  se = list(
    coeftest(FE, vcov = vcovHC(FE, "arellano"))[, 2],
    coeftest(RE, vcov = vcovHC(RE, "arellano"))[, 2],
    summary(ML)$coefficients[, 2],
    coeftest(OLS, vcov = vcovHC(OLS, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 3,
  out = here(out_dir, "fe_re_ri_pols_nosrati.txt"),
  type = "text"
)

# Panel regressions with opioid prescription rates------------------------------
m1 <-
  plm(
    log(drugs) ~ px + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

m2 <-
  plm(
    log(drugs) ~ px + I(-income) + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

m3 <-
  plm(
    log(drugs) ~ px + I(-income) + county_crime + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

m4 <-
  plm(
    log(drugs) ~ px + I(-income) + county_crime + county_jail_adm +
      county_prison_adm + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

m5 <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education +
      blacks + hispanics + race_other + county_crime + px + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(
  m1,
  m2,
  m3,
  m4,
  m5,
  se = list(
    coeftest(m1, vcov = vcovHC(m1, "arellano"))[, 2],
    coeftest(m2, vcov = vcovHC(m2, "arellano"))[, 2],
    coeftest(m3, vcov = vcovHC(m3, "arellano"))[, 2],
    coeftest(m4, vcov = vcovHC(m4, "arellano"))[, 2],
    coeftest(m5, vcov = vcovHC(m5, "arellano"))[, 2]
  ),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 2,
  out = here(out_dir, "opioid_prescriptions_nosrati.txt"),
  type = "text"
)

# Adjusting for all-cause mortality
FE_all <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education +
      blacks + hispanics + race_other + county_crime + all_cause + year,
    index = c("fips", "year"),
    model = "within",
    data = z.data
  )

stargazer(
  FE_all,
  se = list(coeftest(FE_all, vcov = vcovHC(FE_all, "arellano"))[, 2]),
  star.cutoffs = c(0.05, 0.01, 0.001),
  digits = 2,
  out = here(out_dir, "fe_all_deaths_nosrati.txt"),
  type = "text"
)
