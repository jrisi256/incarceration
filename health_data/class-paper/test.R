library(readr)
library(stringr)
library(dplyr)
library(here)

a <- read_tsv(here("class-paper", "input", "Multiple Cause of Death, 1999-2020.txt"))
a2 <- read_tsv(here("class-paper", "input", "Multiple Cause of Death, - Large Metro Areas - 1999-2020.txt"))

a3 <-
    read_tsv(here("class-paper", "input", "Multiple Cause of Death, 1999-2020 - state all years.txt")) %>%
    filter(!is.na(`Age Adjusted Rate`)) %>%
    filter(str_detect(`Age Adjusted Rate`, "[0-9]+"))

