
###################################################################################################
###################################################################################################

# Economic decline, incarceration, and mortality from drug use disorders
# Elias Nosrati, Jacob Kang-Brown, Michael Ash, Martin McKee, Michael Marmot, Lawrence King

# Descriptive statistics and figures

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
# library(Zelig)
library(gridExtra)

library(ggplot2)
library(readr)
library(dplyr)
library(here)

# Load data
data <- read_csv(here("clean-and-process", "src", "Replication", "county_data.csv"))

###################################################################################################
###################################################################################################

# Descriptive statistics
des <- data %>%
	select(drugs, all_cause, county_jail_adm, county_prison_adm, income, education, blacks, hispanics, race_other, county_crime, px)

summary(des)
stargazer(data.frame(des), title = "Descriptive statistics", digits = 1, omit.summary.stat = c("p25", "p75"), out = "des.txt")

# Correlation matrix
r <- rcorr(as.matrix(des), type = "spearman")$r
stargazer(r, digits = 2, out = "corr.txt")

# Look at distribution of data
ggplot(data) + 
	geom_freqpoly(aes(x = drugs, y = ..density..))

data %>%
	group_by(fips) %>%
	count() %>%
	ungroup()

###################################################################################################
###################################################################################################

# Mortality over time
data <- read_csv("despair_tidy.csv")
data <- data %>%
	mutate(
		t = case_when(
			year < 1985 ~ "1980-1984",
			year > 1984 & year < 1990 ~ "1985-1989",
			year > 1989 & year < 1995 ~ "1990-1994",
			year > 1994 & year < 2000 ~ "1995-1999",
			year > 1999 & year < 2005 ~ "2000-2004",
			year > 2004 & year < 2010 ~ "2005-2009",
			year > 2009 & year < 2015 ~ "2010-2014"),
		t = factor(t, levels = c("1980-1984", "1985-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014"))
		)
time_drugs <- ggplot(data = data) + 
	geom_boxplot(aes(x = t, y = county_prison_adm), fill = "deepskyblue") +
	labs(x = "", y = "Drug deaths per 100,000 county residents") +
	scale_y_continuous(breaks = seq(0, 62, 4)) +
	theme_bw()
ggsave("time_drugs.jpg")
ggsave(time_drugs, file = "Fig1.eps", device = "eps")

###################################################################################################
###################################################################################################

# Average
data <- read_csv("county_data_final.csv")
data <- data %>%
	group_by(fips) %>%
	summarise(across(c("drugs", "income", "education", "blacks", "hispanics",
	                   "race_other", "county_crime", "px", "county_jail_adm",
	                   "county_prison_adm"),
	          mean,
	          na.rm = TRUE))

# Heterogeneity in mortality rates by income
income_drugs <- ggplot(data = data, aes(x = income, y = drugs)) + 
	geom_point(size = 2) +
	labs(x = "Median county household income ($)", y = "Drug deaths per 100,000 county residents") +
	scale_x_continuous(
		breaks = c(20000, 40000, 60000, 80000, 100000),
		label = c("20k", "40k", "60k", "80k", "100k")) +
	scale_y_continuous(limits = c(0, 18), breaks = seq(0, 20, 2)) +
	theme_minimal()
ggsave("income_drugs.jpg")
ggsave(income_drugs, file = "Fig2.eps", device = "eps")

# Jail incarceration by income
jail_income <- ggplot(data = data, aes(x = income, y = county_jail_adm)) +
	geom_point(alpha = 0.45, size = 2) +
	geom_smooth(method = "lm", colour = "dark green") +
	labs(x = "Median household income ($)", y = "Jail admissions rate per 100,000 county residents") +
	scale_x_continuous(
		breaks = c(20000, 40000, 60000, 80000, 100000),
		label = c("20k", "40k", "60k", "80k", "100k")) +
	scale_y_continuous(breaks = seq(0, 25000, 2000)) +
	theme_minimal()
ggsave("jail_income.jpg")

# Prison incarceration by income
prison <- ggplot(data = data, aes(x = income, y = county_prison_adm)) +
	geom_point(alpha = 0.45, size = 2) +
	geom_smooth(method = "lm", colour = "dark turquoise") +
	labs(x = "Median household income ($)", y = "Prison admissions rate per 100,000 county residents") +
	scale_x_continuous(
		breaks = c(20000, 40000, 60000, 80000, 100000),
		label = c("20k", "40k", "60k", "80k", "100k")) +
	scale_y_continuous(breaks = seq(0, 800, 100)) +
	theme_minimal()
ggsave("prison_income.jpg")

# Drug deaths by jail incarceration
drugs_jail <- ggplot(data = data, aes(x = county_jail_adm, y = drugs)) +
	geom_point(alpha = 0.45, size = 2) +
	geom_smooth(method = "lm", colour = "brown") +
	labs(x = "Jail admissions rate per 100,000 county residents", y = "Drug deaths per 100,000 county residents") +
	scale_x_continuous(breaks = seq(0, 25000, 2000)) +
	scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 4)) +
	theme_minimal()
ggsave("drugs_jail.jpg")

# Drug deaths by prison incarceration
drugs_prison <- ggplot(data = data, aes(x = county_prison_adm, y = drugs)) +
	geom_point(alpha = 0.45, size = 2) +
	geom_smooth(method = "lm", colour = "pink3") +
	labs(x = "Prison admissions rate per 100,000 county residents", y = "Drug deaths per 100,000 county residents") +
	scale_x_continuous(breaks = seq(0, 800, 100)) +
	scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 4)) +
	theme_minimal()
ggsave("drugs_prison.jpg")

###################################################################################################
###################################################################################################

# Plot prescription rates
layout(matrix(1:4, 2, 2))

plot(data$px, data$drugs, xlab = "Local prescription rate", ylab = "Overdose mortality rate")
plot(data$px, data$income, xlab = "Local prescription rate", ylab = "Median household income")
plot(data$px, data$education, xlab = "Local prescription rate", ylab = "HS graduation rate")
plot(data$px, data$county_jail_adm, xlab = "Local prescription rate", ylab = "Jail admissions rate")

layout(matrix(1:4, 2, 2))

plot(data$px, data$county_prison_adm, xlab = "Local prescription rate", ylab = "Prison admissions rate")
plot(data$px, data$blacks, xlab = "Local prescription rate", ylab = "Fraction African Americans")
plot(data$px, data$hispanics, xlab = "Local prescription rate", ylab = "Fraction Hispanics")
plot(data$px, data$county_crime, xlab = "Local prescription rate", ylab = "Crime rate")

###################################################################################################
###################################################################################################
