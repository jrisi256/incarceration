# Paper and authors ------------------------------------------------------------

# Economic decline, incarceration, and mortality from drug use disorders
# Elias Nosrati, Jacob Kang-Brown, Michael Ash, Martin McKee, Michael Marmot,
# and Lawrence King

# File -------------------------------------------------------------------------
# Descriptive statistics and figures

# Load packages ----------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(Hmisc)
library(GGally)
library(ggplot2)
library(stargazer)
library(ggcorrplot)

# Load data --------------------------------------------------------------------
data <-
  read_csv(here("replication", "input", "county_data.csv")) %>%
  mutate(
    fips = as.character(fips),
    fips = if_else(nchar(fips) == 4, paste0("0", fips), fips)
  )

# Descriptive statistics -------------------------------------------------------
des <-
  data %>%
  select(
    drugs, all_cause, county_jail_adm, county_prison_adm, income, px,
    education, blacks, hispanics, race_other, county_crime
  )

stargazer(data.frame(des),
  title = "Descriptive Statistics",
  digits = 2,
  median = T,
  iqr = T,
  out = here("replication", "output", "descriptive_table.txt")
)

# Correlation matrices----------------------------------------------------------
r_sp <- rcorr(as.matrix(des), type = "spearman")$r
r_pr <- rcorr(as.matrix(des), type = "pearson")$r
stargazer(r_sp, digits = 2, out = here("replication", "output", "corr_sp.txt"))
stargazer(r_pr, digits = 2, out = here("replication", "output", "corr_pr.txt"))

ggcorrplot(r_sp, type = "lower", lab = T, title = "Spearman Correlation Matrix")
ggsave(here("replication", "output", "corr_sp.pdf"))

ggcorrplot(r_pr, type = "lower", lab = T, title = "Pearson Correlation Matrix")
ggsave(here("replication", "output", "corr_pr.pdf"))

# Look at distribution of data--------------------------------------------------
densitychart <- function(df, col, col_name) {
  boxplot_coord <- max(density(df[[col_name]], na.rm = T)$y) / 2

  ggplot(df, aes(x = {{ col }})) +
    geom_boxplot(aes(y = -boxplot_coord), width = boxplot_coord) +
    geom_density() +
    labs(x = col_name, y = "density") +
    theme_bw()
}

pdf(here("replication", "output", "density_plots.pdf"), onefile = T)
pmap(list(as.list(des), as.list(colnames(des))), densitychart, df = des)
dev.off()

nr_obs_year <- data %>% count(year)
ggplot(nr_obs_year, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "Number of county observations each year")
ggsave(here("replication", "output", "nr_obs_year.pdf"))

nr_obs_county <- data %>% count(fips)
ggplot(nr_obs_county, aes(x = n)) +
  geom_boxplot(width = 0.01) +
  geom_density(aes(x = n)) +
  labs(title = "Number of (yearly) observations for each county") +
  theme_bw()
ggsave(here("replication", "output", "nr_obs_county.pdf"))

# Look at distribution of variables over time-----------------------------------
boxplottime <- function(df, col, col_name) {
  ggplot(df, aes(x = as.factor(year), y = {{ col }})) +
    geom_boxplot() +
    labs(x = "Year", y = col_name) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1))
}

pdf(here("replication", "output", "change_over_time.pdf"), onefile = T)
pmap(
  list(
    as.list(select(data, -fips, -county, -year, -state)),
    as.list(colnames(select(data, -fips, -county, -year, -state)))
  ),
  boxplottime,
  df = data
)
dev.off()

# Mortality over time by original research team---------------------------------
data <- data %>%
  mutate(
    t = case_when(
      year < 1985 ~ "1980-1984",
      year > 1984 & year < 1990 ~ "1985-1989",
      year > 1989 & year < 1995 ~ "1990-1994",
      year > 1994 & year < 2000 ~ "1995-1999",
      year > 1999 & year < 2005 ~ "2000-2004",
      year > 2004 & year < 2010 ~ "2005-2009",
      year > 2009 & year < 2015 ~ "2010-2014"
    ),
    t = factor(t, levels = c(
      "1980-1984", "1985-1989", "1990-1994", "1995-1999",
      "2000-2004", "2005-2009", "2010-2014"
    ))
  )

ggplot(data = data) +
  geom_boxplot(aes(x = t, y = drugs), fill = "deepskyblue") +
  labs(x = "Years", y = "Drug deaths per 100,000 county residents") +
  scale_y_continuous(breaks = seq(0, 62, 4)) +
  theme_bw()
ggsave(here("replication", "output", "time_drugs.jpg"))

# Scatter plot matrix-----------------------------------------------------------
density2dchart <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_density2d(aes(fill = stat(level)), geom = "polygon")
}

ggpairs(des,
  lower = list(continuous = density2dchart),
  upper = list(continuous = wrap("cor", size = 2, digits = 2))
) +
  theme_bw() +
  theme(
    text = element_text(size = 7),
    axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1)
  )

ggsave(here("replication", "output", "scatterplot_matrix.jpg"))

# Scatter plots created by original research team-------------------------------
data_mean_sd <-
  data %>%
  group_by(fips) %>%
  summarise(across(
    c(
      "drugs", "income", "education", "blacks", "hispanics", "race_other",
      "county_crime", "px", "county_jail_adm", "county_prison_adm", "all_cause"
    ),
    list(
      mean = ~ mean(., na.rm = T),
      sd = ~ sd(.x, na.rm = T)
    ),
    na.rm = TRUE,
    .names = "{.col}__{.fn}"
  )) %>%
  ungroup()

# Income by drug deaths
ggplot(data = data_mean_sd, aes(x = income__mean, y = drugs__mean)) +
  geom_point(size = 2) +
  labs(
    x = "Median county household income ($)",
    y = "Drug deaths per 100,000 county residents",
    title = "Mean income vs. mean drug deaths: Counties from 1983 to 2014"
  ) +
  scale_x_continuous(
    breaks = c(20000, 40000, 60000, 80000, 100000),
    label = c("20k", "40k", "60k", "80k", "100k")
  ) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 20, 2)) +
  theme_minimal()
ggsave(here("replication", "output", "income_drugs.pdf"))

# Income by jail rates
ggplot(data = data_mean_sd, aes(x = income__mean, y = county_jail_adm__mean)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", colour = "dark green") +
  labs(
    x = "Median household income ($)",
    y = "Jail admissions rate per 100,000 county residents",
    title = "Mean income vs. mean jail rates: Counties from 1983 to 2014"
  ) +
  scale_x_continuous(
    breaks = c(20000, 40000, 60000, 80000, 100000),
    label = c("20k", "40k", "60k", "80k", "100k")
  ) +
  scale_y_continuous(breaks = seq(0, 25000, 2000)) +
  theme_minimal()
ggsave(here("replication", "output", "income_jail.pdf"))

# Income by prison rates
ggplot(data = data_mean_sd, aes(x = income__mean, y = county_prison_adm__mean)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", colour = "dark turquoise") +
  labs(
    x = "Median household income ($)",
    y = "Prison admissions rate per 100,000 county residents",
    title = "Mean income vs. mean prison rates: Counties from 1983 to 2014"
  ) +
  scale_x_continuous(
    breaks = c(20000, 40000, 60000, 80000, 100000),
    label = c("20k", "40k", "60k", "80k", "100k")
  ) +
  scale_y_continuous(breaks = seq(0, 800, 100)) +
  theme_minimal()
ggsave(here("replication", "output", "income_prison.pdf"))

# jail vs. drug deaths
ggplot(data = data_mean_sd, aes(x = county_jail_adm__mean, y = drugs__mean)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", colour = "brown") +
  labs(
    x = "Jail admissions rate per 100,000 county residents",
    y = "Drug deaths per 100,000 county residents",
    title = "Mean jail rates vs. mean drug deaths: Counties from 1983 to 2014"
  ) +
  scale_x_continuous(breaks = seq(0, 25000, 2000)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 4)) +
  theme_minimal()
ggsave(here("replication", "output", "jail_drugs.pdf"))

# Drug deaths by prison incarceration
ggplot(data = data_mean_sd, aes(x = county_prison_adm__mean, y = drugs__mean)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", colour = "pink3") +
  labs(
    x = "Prison admissions rate per 100,000 county residents",
    y = "Drug deaths per 100,000 county residents",
    title = "Mean prison rates vs. mean drug deaths: Counties from 1983 to 2014"
  ) +
  scale_x_continuous(breaks = seq(0, 800, 100)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 4)) +
  theme_minimal()
ggsave(here("replication", "output", "prison_drugs.pdf"))

# Plot prescription drug rates
scatterplot_prescription_rates <- function(data, ycol, yname) {
  ggplot(data, aes(x = px__mean, y = {{ ycol }})) +
    geom_point() +
    theme_bw() +
    labs(
      x = "Drug prescription rate",
      y = yname,
      title = "Mean values: USA Counties from 2006 to 2014"
    )
}

ynames <- list(
  "Drug overdose mortality rate", "Overall mortality rate",
  "Jail admissions rate", "Median household income", "Prison admissions rate",
  "HS graduation rate", "% Black", "% Hispanic", "% Race other", "Homicide rate"
)

pdf(here("replication", "output", "prescription_scatterplots.pdf"), onefile = T)
pmap(
  list(
    as.list(select(
      data_mean_sd, drugs__mean, all_cause__mean, county_jail_adm__mean,
      income__mean, county_prison_adm__mean, education__mean, blacks__mean,
      hispanics__mean, race_other__mean, county_crime__mean,
    )),
    ynames
  ),
  scatterplot_prescription_rates,
  data = data_mean_sd
)
dev.off()

# Plot coefficient of variation for each county for each variable---------------
# How much does each variable vary over time for each county?-------------------
data_cov <-
  data_mean_sd %>%
  pivot_longer(
    cols = -fips,
    names_to = "variable",
    values_to = "value"
  ) %>%
  separate(variable, c("variable", "statistic"), sep = "__") %>%
  pivot_wider(names_from = "statistic", values_from = "value") %>%
  mutate(cov = sd / mean) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = "variable", values_from = "cov")

histogram_chart <- function(df, col, col_name) {
  ggplot(df, aes(x = {{ col }})) +
    geom_boxplot(aes(y = -50), width = 50) +
    geom_histogram(bins = 35, color = "green", fill = "blue") +
    labs(
      x = col_name, y = "n",
      title = "Coefficient of Variation: Counties in the USA"
    ) +
    theme_bw()
}

pdf(here("replication", "output", "histograms_cov.pdf"), onefile = T)
pmap(
  list(
    as.list(select(data_cov, -fips)),
    as.list(colnames(select(data_cov, -fips)))
  ),
  histogram_chart,
  df = data_cov
)
dev.off()
