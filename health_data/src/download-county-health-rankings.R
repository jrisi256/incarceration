library(here)
library(purrr)

dir <- here("health_data", "src")

dir.create(here(dir, "dictionary"))
dir.create(here(dir, "documentation"))
dir.create(here(dir, "data"))
dir.create(here(dir, "data_source"))
dir.create(here(dir, "state_compare"))
dir.create(here(dir, "variable_change"))

years <- 2010:2023
dictionaries <- here(dir, "dictionary", paste0("dictionary_", years, ".pdf"))
doc <- here(dir, "documentation", paste0("documentation_", years, ".pdf"))
data <- here(dir, "data", paste0("health-data_", years, ".csv"))
data_source <- here(dir, "data_source", paste0("source_", years, ".xlsx"))
state_comp <- here(dir, "state_compare", paste0("state-comp_", years, ".pdf"))
var_change <- here(dir, "variable_change", paste0("var-change_", years, ".pdf"))

download_var_change <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"
  
  if(yr == 2021 | yr == 2022) {
    download.file(
      paste0(s, "media/document/", yr, "%20Measure%20Changes.pdf"),
      dest
    )
  } else if(yr == 2019) {
    download.file(paste0(s, "Measure%20changes%202018-2019.pdf"), dest)
  } else if(yr == 2020) {
    download.file(
      paste0(s, "media/document/Measure%20changes%202019-2020.pdf"),
      dest
    )
  } else if(yr == 2023) {
    download.file(
      paste0(s, "media/document/Measure%20changes%202022-2023_0.pdf"),
      dest
    )
  }
}

download_state_comp <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"
  
  if(yr == 2021 | yr == 2022 | yr == 2023) {
    download.file(
      paste0(
        s,
        "media/document/",
        yr,
        "%20Comparability%20Across%20States.pdf"),
      dest
    )
  } else if(yr == 2019) {
    download.file(
      paste0(s, yr, "%20Comparability%20across%20states.pdf"),
      dest
    )
  } else if(yr == 2020) {
    download.file(
      paste0(
        s,
        "media/document/",
        yr,
        "%20Comparability%20across%20states.pdf"),
      dest
    )
  }
}

download_dictionary <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"

  if (yr != 2020 & yr != 2021 & yr != 2022 & yr != 2023) {
    download.file(paste0(s, "DataDictionary_", yr, ".pdf"), dest)
  } else if (yr == 2020) {
    download.file(
      paste0(s, "media/document/DataDictionary_", yr, "_2.pdf"),
      dest
    )
  } else if(yr == 2021 | yr == 2022) {
    download.file(
      paste0(s, "media/document/DataDictionary_", yr, ".pdf"),
      dest
    )
  } else if (yr == 2023) {
    download.file(
      paste0(s, "media/document/", yr, "%20Data%20Dictionary%20%28PDF%29.pdf"),
      dest
    )
  }
}

download_documentation <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"

  if (yr == 2020) {
    download.file(
      paste0(s, "media/document/", yr, "%20Analytic%20Documentation_0.pdf"),
      dest
    )
  } else if (yr == 2021 | yr == 2022 | yr == 2023) {
    download.file(
      paste0(s, "media/document/", yr, "%20Analytic%20Documentation.pdf"), dest
    )
  } else if (yr == 2019) {
    download.file(paste0(s, yr, "%20Analytic%20Documentation_1.pdf"), dest)
  } else {
    download.file(paste0(s, yr, "%20Analytic%20Documentation.pdf"), dest)
  }
}

download_data <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"

  if (yr == 2018) {
    download.file(paste0(s, "analytic_data", yr, "_0.csv"), dest)
  } else if (yr == 2019 | yr == 2021 | yr == 2022) {
    download.file(paste0(s, "media/document/analytic_data", yr, ".csv"), dest)
  } else if (yr == 2020 | yr == 2023) {
    download.file(paste0(s, "media/document/analytic_data", yr, "_0.csv"), dest)
  } else {
    download.file(paste0(s, "analytic_data", yr, ".csv"), dest)
  }
}

download_source <- function(yr, dest) {
  s <- "https://www.countyhealthrankings.org/sites/default/files/"

  if (yr == 2015 | yr == 2016) {
    download.file(
      paste0(s, yr, "%20County%20Health%20Rankings%20Data%20-%20v3.xls"), dest
    )
  } else if (yr == 2011 | yr == 2012) {
    download.file(
      paste0(s, yr, "%20County%20Health%20Rankings%20National%20Data_v2_0.xls"),
      dest
    )
  } else if (yr == 2010) {
    download.file(
      paste0(s, yr, "%20County%20Health%20Rankings%20National%20Data_v2.xls"),
      dest
    )
  } else if (yr == 2013) {
    download.file(paste0(s, yr, "CountyHealthRankingsNationalData.xls"), dest)
  } else if (yr == 2017) {
    download.file(paste0(s, yr, "CountyHealthRankingsData.xls"), dest)
  } else if (yr == 2014) {
    download.file(
      paste0(s, yr, "%20County%20Health%20Rankings%20Data%20-%20v6.xls"), dest
    )
  } else if (yr == 2018) {
    download.file(
      paste0(s, yr, "%20County%20Health%20Rankings%20Data%20-%20v2.xls"), dest
    )
  } else if (yr == 2019) {
    download.file(
      paste0(
        s,
        "media/document/",
        yr,
        "%20County%20Health%20Rankings%20Data%20-%20v3.xls"
      ),
      dest
    )
  } else if (yr == 2020 | yr == 2023) {
    download.file(
      paste0(
        s,
        "media/document/",
        yr,
        "%20County%20Health%20Rankings%20Data%20-%20v2.xlsx"
      ),
      dest
    )
  } else if (yr == 2021 | yr == 2022) {
    download.file(
      paste0(
        s,
        "media/document/",
        yr,
        "%20County%20Health%20Rankings%20Data%20-%20v1.xlsx"
      ),
      dest
    )
  }
}

pmap(list(years, dictionaries), download_dictionary)
pmap(list(years, doc), download_documentation)
pmap(list(years, data), download_data)
pmap(list(years, data_source), download_source)
pmap(list(years, state_comp), download_state_comp)
pmap(list(years, var_change), download_var_change)
