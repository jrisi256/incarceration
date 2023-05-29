test.data <-
  data %>%
  select(-county) %>%
  mutate(log_education = log(education)) %>%
  filter(if_all(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education"
    ),
    ~ !is.na(.x)
  )) %>%
  group_by(fips) %>%
  filter(n() >= 31) %>%
  ungroup() %>%
  mutate(across(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education"
    ),
    mean, .names = "{col}_mean"
  )) %>%
  group_by(fips) %>%
  mutate(across(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education"
    ),
    mean, .names = "{col}_county"
  )) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(across(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education"
    ),
    mean, .names = "{col}_time"
  )) %>%
  ungroup() %>%
  mutate(income = income - income_time - income_county + income_mean,
         county_jail_adm = county_jail_adm - county_jail_adm_time - county_jail_adm_county + county_jail_adm_mean,
         county_prison_adm = county_prison_adm - county_prison_adm_time - county_prison_adm_county + county_prison_adm_mean,
         county_crime = county_crime - county_crime_time - county_crime_county + county_crime_mean,
         log_education = log_education - log_education_time - log_education_county + log_education_mean,
         blacks = blacks - blacks_time - blacks_county + blacks_mean,
         hispanics = hispanics - hispanics_time - hispanics_county + hispanics_mean,
         race_other = race_other - race_other_time - race_other_county + race_other_mean)

test <- lm(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + hispanics +
             log_education + blacks + race_other + county_crime, data = test.data)

data1 <- data %>% mutate(log_education = log(education)) %>%
  filter(if_all(
    c(
      "income", "county_jail_adm", "county_prison_adm", "county_crime",
      "blacks", "hispanics", "race_other", "log_education"
    ),
    ~ !is.na(.x)
  )) %>%
  group_by(fips) %>%
  filter(n() >= 31) %>%
  ungroup()

library(fixest)
FE_fixest <-
  feols(log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + hispanics +
          log_education + blacks + race_other + county_crime | fips + year,
        data = data1,
        demeaned = T)

a<-as.data.frame(FE_fixest$X_demeaned)
a2 <-
  test.data %>%
  select(county_jail_adm, county_jail_adm_time, county_jail_adm_county, county_jail_adm_mean) %>%
  rename(county_jail1 = county_jail_adm) %>%
  bind_cols(select(a, county_jail_adm)) %>%
  bind_cols(select(data, county_jail_adm, fips)) %>%
  mutate(test = county_jail1 / county_jail_adm...5)

a3 <- count(data, fips)
a2 <- a2 %>% full_join(a3, by = "fips")

FE <-
  plm(
    log(drugs) ~ I(-income) + county_jail_adm + county_prison_adm + hispanics +
      log_education + blacks + race_other + county_crime + year,
    index = c("fips", "year"),
    model = "within",
    data = data
  )
