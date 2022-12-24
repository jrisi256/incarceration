
###################################################################################################
###################################################################################################

# Economic decline, incarceration, and mortality from drug use disorders
# Elias Nosrati, Jacob Kang-Brown, Michael Ash, Martin McKee, Michael Marmot, Lawrence King

# Matching and "between" regression

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
library(tidyverse)

# Load data
data <- read_csv("county_data.csv")
data <- data %>%
	group_by(state, fips) %>%
	summarise_at(c("drugs", "income", "education", "blacks", "hispanics", "race_other", "county_crime", "px", "county_jail_adm", "county_prison_adm"), mean, na.rm = TRUE) %>%
	na.omit(data)

# Define treatment
quantile(data$county_jail_adm, probs = seq(0, 1, 0.1))
quantile(data$county_prison_adm, probs = seq(0, 1, 0.1))
data$t <- with(data, ifelse(county_jail_adm > 6999.0571 | county_prison_adm > 249.33159, 1, 0))
table(data$t == 1)

# CEM
m <- matchit(t ~ income + education + blacks + hispanics + race_other + county_crime + px, data = data.frame(data), method = "cem")
summary(m)
plot(m)

# Extract data and run model
mdata <- match.data(m)

z <- zelig(drugs ~ county_jail_adm + county_prison_adm, model = "ls", data = mdata)
summary(z)

# Set X
mean(mdata$county_jail_adm) - sd(mdata$county_jail_adm)
mean(mdata$county_prison_adm) - sd(mdata$county_prison_adm)
x <- setx(z, county_jail_adm = 3326.767, county_prison_adm = 101.7502)

mean(mdata$county_jail_adm) + sd(mdata$county_jail_adm)
mean(mdata$county_prison_adm) + sd(mdata$county_prison_adm)
x1 <- setx(z, county_jail_adm = 8540.586, county_prison_adm = 306.0522)

# Simulate and plot
s <- sim(z, x = x, x1 = x1, num = 100000)
summary(s)
plot(s)

# Extract quantities of interest and plot manually
d <- zelig_qi_to_df(s) %>%
	mutate(
		setx_value = as.factor(setx_value),
		setx_value = fct_recode(setx_value,
		"T = 0" = "x",
		"T = 1" = "x1"))

p1 <- ggplot(d, aes(x = expected_value, colour = setx_value, fill = setx_value)) +
	geom_density(adjust = 8) + 
	facet_grid(setx_value ~., scales = "free") +	
	scale_fill_manual(values = c("darkcyan", "grey0")) +
	scale_colour_manual(values = c("darkcyan", "grey0")) +
	labs(title = "Expected overdose mortality rate by incarceration rate", x = "Expected value of overdose mortality rate", y = "Density") +
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), legend.position = "none")

# First difference	
fd <- data.frame(get_qi(s, qi = "fd", xvalue = "x1"))
names(fd) <- "fd"
p2 <- ggplot(fd) +
	geom_density(aes(x = fd), colour = "deeppink4", fill = "deeppink4", adjust = 2) +
	coord_fixed(0.18) +
	labs(title = "Expected difference in overdose mortality rate \n between high- and low-incarceration counties", x = "Expected difference in overdose mortality rate", y = "Density") +
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5, face = "bold"))

sim <- grid.arrange(p1, p2)
ggsave("sim_drugs.jpg", sim)
ggsave(sim, file = "Fig3.eps", device = "eps")

###################################################################################################
###################################################################################################

# Redefine treatment
quantile(data$county_jail_adm, probs = seq(0, 1, 0.2))
quantile(data$county_prison_adm, probs = seq(0, 1, 0.2))
data$t <- with(data, ifelse(county_jail_adm > 7845.7687 | county_prison_adm > 287.93141, 1, 0))
table(data$t == 1)

# CEM
m <- matchit(t ~ income + education + blacks + hispanics + race_other + county_crime + px, data = data.frame(data), method = "cem")
summary(m)
plot(m)

# Extract data and run model
mdata <- match.data(m)

z <- zelig(drugs ~ t, model = "ls", data = mdata)
summary(z)

# Simulate and plot
x <- setx(z, t = 0)
x1 <- setx(z, t = 1)
s <- sim(z, x = x, x1 = x1, num = 100000)
summary(s)
plot(s)

# Extract quantities of interest and plot manually
# T = 0
t0 <- data.frame(get_qi(s, qi = "ev", xvalue = "x"))
names(t0) <- "ev"
p0 <- ggplot(t0) +
	geom_density(aes(x = ev), colour = "dark red", fill = "dark red", alpha = 0.8, adjust = 14) +
	coord_fixed(0.5) +
	scale_y_continuous(limits = c(0, 1.6)) +
	labs(x = "Expected values: E(Y|T = 0)", y = "Density") +
	theme_minimal()

# T = 1
t1 <- data.frame(get_qi(s, qi = "ev", xvalue = "x1"))
names(t1) <- "ev"
p1 <- ggplot(t1) +
	geom_density(aes(x = ev), colour = "dark blue", fill = "dark blue", alpha = 0.8, adjust = 8) +
	coord_fixed(0.5) +
	scale_y_continuous(limits = c(0, 1.3)) +
	labs(title = "Expected overdose mortality rate by incarceration rate", x = "Expected value of overdose mortality rate", y = "Density") +
	theme_minimal()	
	
# Compare on same axes	
d <- zelig_qi_to_df(s) %>%
	mutate(
		setx_value = as.factor(setx_value),
		setx_value = fct_recode(setx_value,
		"T = 0" = "x",
		"T = 1" = "x1"))
		
p0 <- ggplot(d, aes(x = expected_value, colour = factor(setx_value), fill = factor(setx_value))) +
	coord_fixed(0.5) +
	geom_density(alpha = 0.8, adjust = 5) + 
	labs(x = "Comparison of expected values", y = "Density") +
	scale_fill_manual(values = c("dark blue", "dark red")) +
	scale_colour_manual(values = c("dark blue", "dark red")) +
	theme_minimal() +
	theme(legend.position = "top", legend.title = element_blank())

p1 <- ggplot(d, aes(x = expected_value, colour = setx_value, fill = setx_value)) +
	geom_density(alpha = 0.8, adjust = 8) + 
	facet_grid(setx_value ~., scales = "free") +	
	scale_fill_manual(values = c("dark blue", "dark red")) +
	scale_colour_manual(values = c("dark blue", "dark red")) +
	labs(title = "Expected overdose mortality rate by incarceration rate", x = "Expected value of overdose mortality rate", y = "Density") +
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), legend.position = "none")

# First difference	
fd <- data.frame(get_qi(s, qi = "fd", xvalue = "x1"))
names(fd) <- "fd"
p2 <- ggplot(fd) +
	geom_density(aes(x = fd), colour = "darkorchid4", fill = "darkorchid4", alpha = 0.8, adjust = 2) +
	coord_fixed(0.13) +
	labs(title = "Expected difference in overdose mortality rate \n between high- and low-incarceration counties", x = "Expected difference in overdose mortality rate", y = "Density") +
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5, face = "bold"))

sim <- grid.arrange(p1, p2)
ggsave("sim_drugs2.jpg", sim)

###################################################################################################
###################################################################################################
