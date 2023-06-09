# Things to check when harmonizing the State & County health rankings data across years

## Within year

* What does each variable mean? Record the meaning of each variable as well as its source in my **my_dictionary**. (Look at introduction in excel file).
* Which years of data measurement correspond to which years of the State & County Health Rankings data releases? Record that in **year_matching**. (Look at sources in excel file).
* Does all the documentation agree about the variable meaning, variable source, and years of availability? (compare sources in excel file to PDF).

## Across years

* Do the data releases have the same names for the same name for the same variables?
* Do the data releases have the same source for the same variable?
* Do the data releases introduce new variables?
* Are the variables on the same scale?

## Harmonization

* I changed *chlamydia_rate* in 2010 to *sexually_transmitted_infections* in order to match later years.
* I changed *income_inequality* in 2010 to *gini_coefficient* to differentiate it from future versions of *income_inequality*. Later versions of this variable calculate the ratio of income at the 80th percentile to income at the 20th percentile and **not** the Gini coefficient.
* In 2010, I change *primary_care_provider_rate_per_100000* to *primary_care_provider_rate* and change the stem value to *raw_value*. I change *ratio_of_population_to_primary_care* to *primary_care_provider_rate*, and I also change the stem value to *ratio*.
* I recalculated *liquor_store_density* in 2010 to be per 100,000 people rather than 10,000 people. Additionally, I recalculated it in 2011 as well because the creators of the data originally rounded their value to the nearest whole number.
* In 2012, there are duplicate columns for *access_to_healthy_foods*, and I remove those columns.
* From 2010-2012, lead poisoned children was named *lead_poisoned_children_(wi)*. Starting in 2013, the variable name changed to *lead_poisoned_children*. I update the values from 2010-2012 to reflect the change in the variable name.
* From 2016-2019, *drinking_water_violations* changes from a percentage variable (% of people exposed to a drinking water violation) to a binary variable (county was or was not exposed to a drinking water violation). I change the name of this variable to *drinking_water_violations_bin* to reflect the new meaning of the variable.
* From 2011-2019, I change *ratio_of_population_to_primary_care* to *primary_care_physicians*, and I change the stem value to *ratio*.
* From 2011-2019, I change *ratio_of_population_mental_health* to *mental_health_providers*, and I change the stem value to *ratio*.
* From 2012-2019, I change *ratio_of_population_to_dentists* to *dentists*, and I change the stem value to *ratio*.
* From 2014-2019, I change *ratio_of_population_to_primary_care_providers_other_than_physicians* to *other_primary_care_providers*, and I change the stem value to *ratio*.
* From 2019-2019, I change the stem value for *percentage_of_households_with_high_housing_costs* to *raw_value*.
* From 2019-2019, I change the stem value for *percentage_of_households_with_overcrowding* to *raw_value*.
* From 2019-2019, I change the stem value for *percentage_of_households_with_lack_of_kitchen_or_plumbing* to *raw_value*.
* From 2019-2019, I recalculate *preventable_hospital_stays* to be per 1,000 people rather than per 100,000 people to keep it consistent with how the variable was measured in previous years.
