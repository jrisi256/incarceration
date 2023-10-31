library(here)
library(readr)
library(dplyr)
library(purrr)
library(forcats)
library(ggplot2)
library(ggridges)
library(flextable)

##################################################################
##                         Read in data                         ##
##################################################################
data <-
  read_csv(
    here(
      "clean_chrr-wphi",
      "output",
      "dedupe_start_end",
      "cross_nonsplit_late_start_year-min.csv"
    )
  ) %>%
  filter(stem == "raw_value", race == "all")

##################################################################
##                  Generate descriptive table                  ##
##################################################################
descriptive_table <-
  data %>%
  group_by(variable) %>%
  filter(!(all(is.na(values)))) %>%
  summarise(
    #nr_rows = n(),
    mean = round(mean(values, na.rm = T), digits = 3),
    sd = round(sd(values, na.rm = T), digits = 3),
    min = round(min(values, na.rm = T), digits = 3),
    p25 = round(quantile(values, probs = 0.25, na.rm = T)[["25%"]], digits = 3),
    median = round(median(values, na.rm = T), digits = 3),
    p75 = round(quantile(values, probs = 0.75, na.rm = T)[["75%"]], digits = 3),
    max = round(max(values, na.rm = T), digits = 3),
    #iqr = round(IQR(values, na.rm = T), digits = 3),
    #mad = round(mad(values, na.rm = T), digits = 3),
    #nr_missing = sum(is.na(values)),
    prcnt_missing = round(sum(is.na(values)) / n(), digits = 3),
    earliest = min(start_year),
    latest = max(start_year)
  ) %>%
  ungroup()

descriptive_ftable <-
  flextable(descriptive_table) %>%
  fit_to_width(max_width = 8)

save_as_docx(
  descriptive_ftable,
  path = here("visualize_chrr-wphi", "output", "descriptive_table.docx")
)

#################################################################
##               Graph distributions of the data               ##
#################################################################
graph_distribution_data <-
  data %>%
  group_by(variable) %>%
  filter(!all(is.na(values))) %>%
  ungroup()

densitychart <- function(df, col) {
  if(col != "all") {
    df <- df %>% filter(variable == col)
  }
  
  raw_values <- df %>% pull(values)
  boxplot_coord <- max(density(raw_values, na.rm = T)$y) / 2
  
  ggplot(df, aes(x = values)) +
    geom_boxplot(aes(y = -boxplot_coord), width = boxplot_coord) +
    geom_density() +
    labs(x = col, y = "density") +
    theme_bw()
}

vars <- unique(graph_distribution_data$variable)

pdf(here("visualize_chrr-wphi", "output", "density_plots.pdf"), onefile = T)
map(as.list(vars), densitychart, df = graph_distribution_data)
dev.off()

##################################################################
##           Percent of the time counties report data           ##
##################################################################
no_missing_data <- data %>% filter(!is.na(values))

nr_total_unique_vars <-
  no_missing_data %>%
  group_by(start_year) %>%
  summarise(total_nr_vars_per_year = length(unique(variable))) %>%
  ungroup() %>%
  summarise(total_nr_vars = sum(total_nr_vars_per_year)) %>%
  pull(total_nr_vars)

nr_county_unique_vars <-
  no_missing_data %>%
  group_by(full_fips, start_year) %>%
  summarise(county_nr_vars_per_year = length(unique(variable))) %>%
  group_by(full_fips) %>%
  summarise(county_nr_vars = sum(county_nr_vars_per_year)) %>%
  ungroup() %>%
  mutate(
    total_nr_vars = nr_total_unique_vars,
    values = county_nr_vars / total_nr_vars
  )

title <-
  paste0(
    "Share of counties reporting 'x' proportion of all variable-years\n",
    "Total # of variables: ", length(unique(no_missing_data$variable)), "\n",
    "Total number of years: ", length(unique(no_missing_data$start_year)), "\n",
    "Total # of variable-years: ", nr_total_unique_vars
  )

pdf(here("visualize_chrr-wphi", "output", "county_reporting.pdf"), onefile = T)
densitychart(nr_county_unique_vars, "all") +
  xlab("Proportion of time reporting") +
  ylab("Number of counties reporting 'x' proportion of the time") +
  ggtitle(title)
dev.off()

#################################################################
##        Percent of time counties report data per year        ##
#################################################################
nr_total_unique_vars_per_year <-
  no_missing_data %>%
  group_by(start_year) %>%
  summarise(total_nr_vars = length(unique(variable))) %>%
  ungroup()

pdf(here("visualize_chrr-wphi", "output", "nr_vars_per_year.pdf"), onefile = T)
ggplot(nr_total_unique_vars_per_year, aes(x = start_year, y = total_nr_vars)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Year") +
  ylab("Number of unique variables reported in a given year")
dev.off()

nr_county_unique_vars_per_year <-
  no_missing_data %>%
  group_by(full_fips, start_year) %>%
  summarise(county_nr_vars = length(unique(variable))) %>%
  ungroup() %>%
  full_join(
    nr_total_unique_vars_per_year, by = "start_year", multiple = "all"
  ) %>%
  mutate(values = county_nr_vars / total_nr_vars)

years <- unique(nr_county_unique_vars_per_year$start_year)

pdf(
  here("visualize_chrr-wphi", "output", "county_year_reporting.pdf"),
  onefile = T
)

map(
  as.list(years),
  function(df, year) {
    df <- df %>% filter(start_year == year)
    
    title <-
      paste0(
        "Share of counties reporting 'x' proportion of all variable-years\n",
        "Total # of variables: ",  df$total_nr_vars, "\n",
        "Year: ", year
      )
    
    densitychart(df, "all") +
      xlab("Proportion of time reporting") +
      ylab("Number of counties reporting 'x' proportion of the time") +
      ggtitle(title)
  },
  df = nr_county_unique_vars_per_year
)

dev.off()

#################################################################
##         Percent missing and percent missing by year         ##
#################################################################
prcnt_missing <-
  data %>%
  group_by(variable) %>%
  summarise(
    nr_missing = sum(is.na(values)),
    nr_non_missing = sum(!is.na(values))
  ) %>%
  ungroup() %>%
  mutate(prcnt_missing = nr_missing / (nr_missing + nr_non_missing))

pdf(here("visualize_chrr-wphi", "output", "prcnt_missing.pdf"), onefile = T)
ggplot(
  prcnt_missing,
  aes(x = fct_reorder(variable, prcnt_missing), y = prcnt_missing)
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  xlab("Variable") +
  ylab("Percent missing")
dev.off()

prcnt_missing_by_year <-
  data %>%
  group_by(variable, start_year) %>%
  summarise(
    nr_missing = sum(is.na(values)),
    nr_non_missing = sum(!is.na(values))
  ) %>%
  mutate(
    prcnt_missing = nr_missing / (nr_missing + nr_non_missing),
    diff = max(prcnt_missing) - min(prcnt_missing)
  ) %>%
  ungroup() %>%
  filter(diff > 0.01)

pdf(
  here("visualize_chrr-wphi", "output", "prcnt_missing_by_year.pdf"),
  onefile = T
)

map(
  as.list(unique(prcnt_missing_by_year$variable)),
  function(df, col) {
    df <- df %>% filter(variable == col)
    
    ggplot(df, aes(x = start_year, y = prcnt_missing)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      xlab("Year") +
      ylab("Percent missing") +
      ggtitle(col)
  },
  df = prcnt_missing_by_year
)

dev.off()

#################################################################
##      Plot changing distribution of variables over time      ##
#################################################################
pdf(
  here("visualize_chrr-wphi", "output", "violin_plots_over_time.pdf"),
  onefile = T
)

map(
  as.list(unique(no_missing_data$variable)),
  function(df, col) {
    df <- df %>% filter(variable == col)
    
    ggplot(df, aes(y = values, x = start_year)) +
      geom_violin(aes(group = start_year)) +
      geom_boxplot(aes(group = start_year), width = 0.1, alpha = 0) +
      theme_bw() +
      xlab(col)
  },
  df = no_missing_data
)

dev.off()

pdf(
  here("visualize_chrr-wphi", "output", "ridge_plots_over_time.pdf"),
  onefile = T
)
map(
  as.list(unique(no_missing_data$variable)),
  function(df, col) {
    df <- df %>% filter(variable == col)
    
    ggplot(df, aes(x = values, y = start_year, group = start_year)) +
      geom_density_ridges(rel_min_height = 0.01, quantile_lines = T) +
      theme_ridges() +
      ylab("Year") +
      xlab(col)
  },
  df = no_missing_data
)
dev.off()

#################################################################
##                Plot coefficient of variation                ##
#################################################################
cov_within <-
  no_missing_data %>%
  group_by(full_fips, variable) %>%
  summarise(mean = mean(values), sd = sd(values)) %>%
  group_by(variable) %>%
  filter(!(all(is.na(sd)))) %>%
  ungroup() %>%
  mutate(cov = sd / mean)
  
cov_between <-
  no_missing_data %>%
  group_by(start_year, variable) %>%
  summarise(mean = mean(values), sd = sd(values)) %>%
  mutate(cov = sd / mean)

pdf(
  here("visualize_chrr-wphi", "output", "cov_within_between.pdf"), onefile = T
)
map(
  as.list(unique(cov_within$variable)),
  function(col, within, between) {
    within <- within %>% filter(variable == col)
    between <- between %>% filter(variable == col)
      
    raw_values <- within %>% pull(cov)
    boxplot_coord <- max(density(raw_values, na.rm = T)$y) / 2
      
    ggplot(within, aes(x = cov)) +
      geom_boxplot(aes(y = -50), width = 50) +
      geom_histogram(bins = 50) +
      labs(
        x = "Coefficient of Variation",
        y = "Counties",
        title = paste0("CoV within and between counties for ", col)
      ) +
      geom_vline(xintercept = between[["cov"]]) +
      theme_bw()
  },
  within = cov_within,
  between = cov_between
)
dev.off()

#################################################################
##                  National and state trends                  ##
#################################################################
nation_level <-
  no_missing_data %>%
  group_by(start_year, variable) %>%
  summarise(mean = mean(values), sd = sd(values)) %>%
  ungroup()

pdf(here("visualize_chrr-wphi", "output", "national_trends.pdf"), onefile = T)
map(
  as.list(unique(nation_level$variable)),
  function(df, col) {
    df <- df %>% filter(variable == col)
    
    ggplot(df, aes(x = start_year, y = mean)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      labs(
        x = "Year",
        y = paste0("Unweighted mean value each year for ", col)
      ) +
      geom_errorbar(
        aes(
          ymin = mean - sd,
          ymax = mean + sd),
        width = 0.2,
        position = position_dodge(0.9)
      )
  },
  df = nation_level
)
dev.off()

state_names <-
  read_csv(here("clean_chrr-wphi", "output", "county_state_names.csv")) %>%
  select(full_fips, state_abb)

state_level <-
  no_missing_data %>%
  full_join(state_names, by = "full_fips", multiple = "all") %>%
  group_by(start_year, variable, state_abb) %>%
  summarise(mean = mean(values)) %>%
  ungroup()

pdf(here("visualize_chrr-wphi", "output", "state_trends.pdf"), onefile = T)
map(
  as.list(unique(state_level$variable)),
  function(df, col) {
    df <- df %>% filter(variable == col)
    
    ggplot(df, aes(x = start_year, y = mean)) +
      geom_point(aes(color = state_abb)) +
      geom_line(aes(color = state_abb, group = state_abb)) +
      theme_bw() +
      labs(
        x = "Year",
        y = paste0("Unweighted mean value each year for ", col)
      ) +
      theme(legend.position = "none")
  },
  df = state_level
)
dev.off()
