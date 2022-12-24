
###################################################################################################
###################################################################################################

# Economic decline, incarceration, and mortality from drug use disorders
# Elias Nosrati, Jacob Kang-Brown, Michael Ash, Martin McKee, Michael Marmot, Lawrence King

# Panel regressions

###################################################################################################
###################################################################################################

# Load packages
library(stargazer)
library(Hmisc)
library(plm)
library(lme4)
library(lmtest)
library(scales)
library(MatchIt)
library(Zelig)
library(gridExtra)

library(here)
library(dplyr)
library(readr)

# Load data
data <- read_csv(here("clean-and-process", "src", "Replication", "county_data.csv"))

###################################################################################################
###################################################################################################

# Fixed effects model with robust standard errors
z.data <- data %>%
	select(fips, state, year, drugs, all_cause, income, county_jail_adm,
	       county_prison_adm, px, education, county_crime, blacks,
	       hispanics, race_other) %>%
    mutate(education = log(education)) %>%
	mutate(across(c("income", "county_jail_adm", "county_prison_adm", "px",
	                "education", "county_crime", "blacks", "hispanics",
	                "race_other"),
	              ~scale(.x)[,1]))

FE <-
    plm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + 
            education + blacks + hispanics + race_other + county_crime + year,
        index = c("fips", "year"),
        model = "within",
        data = z.data)

summary(FE)
coeftest(FE, vcov = vcovHC(FE, "arellano"))

# Display results
P <- FE$coefficients[1:8]
SE <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:8, 2]
p <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:8, 4]
R <- cbind(100*P, 100*SE, p)
colnames(R) <- c("Coefficient", "Standard error", "P-value")
rownames(R) <- c("Household income decline", "County jail admissions rate",
                 "County prison admissions rate", "Fraction high school graduates",
                 "Fraction African Americans", "Fraction Hispanics",
                 "Fraction other ethnicity", "Violent crime rate")
stargazer(R, digits = 3, out = "e1.txt", type = "text")

# Confidence intervals
(R <- cbind(100*P, 100*P - 196*SE, 100*P + 196*SE))

###################################################################################################

# Random effects model

RE <-
    plm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm +
            education + blacks + hispanics + race_other + county_crime + year,
        index = c("fips", "year"), model = "random", data = z.data)
summary(RE)
coeftest(RE, vcov = vcovHC(RE, "arellano"))

###################################################################################################

# Multilevel random intercept model
z.data <- z.data %>% mutate(year = as.factor(year))

ML <-
    lmer(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm +
             education + blacks + hispanics + race_other + county_crime +
             year + (1|fips), data = z.data)
summary(ML)

###################################################################################################

# Pooled OLS

OLS <-
    plm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm +
            education + blacks + hispanics + race_other + county_crime + year,
        index = c("fips", "year"),
        model = "pooling",
        data = z.data)
summary(OLS)
coeftest(OLS, vcov = vcovHC(OLS, "arellano"))

###################################################################################################

# Display results
stargazer(FE, RE, ML, OLS,
          column.labels = c("Fixed Effects", "Random Effects", "Random Intercept",
                            "OLS"),
          se = list(coeftest(FE, vcov = vcovHC(FE, "arellano"))[, 2],
                    coeftest(RE, vcov = vcovHC(RE, "arellano"))[, 2],
                    summary(ML)$coefficients[, 2],
                    coeftest(OLS, vcov = vcovHC(OLS, "arellano"))[, 2]),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          out = "mods.txt",
          type = "text")

###################################################################################################
###################################################################################################

# Panel regressions with opioid prescription rates
m1 <- plm(log(drugs) ~ px + year, index = c("fips", "year"), model = "within", data = z.data)

m2 <- plm(log(drugs) ~ px + I(-income) + year, index = c("fips", "year"), model = "within", data = z.data)

m3 <- plm(log(drugs) ~ px + I(-income) + county_crime + year, index = c("fips", "year"), model = "within", data = z.data)

m4 <- plm(log(drugs) ~ px + I(-income) + county_crime + county_jail_adm + county_prison_adm + year, index = c("fips", "year"), model = "within", data = z.data)

stargazer(m1, m2, m3, m4, se = list(coeftest(m1, vcov = vcovHC(m1, "arellano"))[, 2], coeftest(m2, vcov = vcovHC(m2, "arellano"))[, 2], coeftest(m3, vcov = vcovHC(m3, "arellano"))[, 2], coeftest(m4, vcov = vcovHC(m4, "arellano"))[, 2]), star.cutoffs = c(0.05, 0.01, 0.001), digits = 2)

FE <- plm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education + blacks + hispanics + race_other + county_crime + px + year, index = c("fips", "year"), model = "within", data = z.data)
summary(FE)
coeftest(FE, vcov = vcovHC(FE, "arellano"))

# Adjusting for all-cause mortality
FE <- plm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + education + blacks + hispanics + race_other + county_crime + all_cause + year, index = c("fips", "year"), model = "within", data = z.data)
summary(FE)
coeftest(FE, vcov = vcovHC(FE, "arellano"))

# Display results
P <- FE$coefficients[1:9]
SE <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:9, 2]
p <- coeftest(FE, vcov = vcovHC(FE, "arellano"))[1:9, 4]
(R <- cbind(100*P, 100*P - 196*SE, 100*P + 196*SE))

###################################################################################################
###################################################################################################

z.df <-
    z.data %>%
	group_by(fips) %>%
    mutate(across(c("income", "county_jail_adm", "county_prison_adm", "px",
                    "education", "county_crime", "blacks", "hispanics",
                    "race_other"),
           lag)) %>%
    ungroup() %>%
    mutate(lag_drugs = lag(drugs))

FE <- plm(log(drugs) ~ log(lag_drugs) + I(-income) + county_jail_adm +
              county_prison_adm + education + blacks + hispanics + race_other +
              county_crime + year,
          index = c("fips", "year"),
          model = "within",
          data = z.df)
summary(FE)
coeftest(FE, vcov = vcovHC(FE, "arellano"))
	
###################################################################################################
###################################################################################################
	