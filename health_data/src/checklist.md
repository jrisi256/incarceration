# Things to check when harmonizing the State & County health rankings data across years

## Within year

* What does each variable mean? Record the meaning of each variable as well as its source in my **my_dictionary**. (Look at introduction in excel file).
* Which years of data measurement correspond to which years of the State & County Health Rankings data releases? Record that in **year_matching**. (Look at sources in exel file).
* Does all the documentation agree about the variable meaning, variable source, and years of availability? (compare sources in excel file to PDF).

## Across years

* Do the data releases have the same names for the same name for the same variables?
* Do the data releases have the same source for the same variable?
* Do the data releases introduce new variables?
* Are the variables on the same scale?

## Harmonization

* I changed *chlamydia_rate* in 2010 to *sexually_transmitted_infections* in order to match later years.
* In 2010, I change *primary_care_provider_rate_per_100000* to *primary_care_proivder_rate* and change the stem value to *raw_value*. I change *ratio_of_population_to_primary_care* to *primary_care_provider_rate*, and I also change the stem value to *ratio*.
* I recalculated *liquor_store_density* in 2010 to be per 100,000 people rather than 10,000 people. Additionally, I recalculated it in 2011 as well because the creators of the data originally rounded their value to the nearest whole number.
* In 2012, there are duplicate columns for *access_to_healthy_foods*, and I remove those columns.
* From 2010-2012, air pollution due to particulate matter is called *air_pollution-particulate_matter_days*. Starting in 2013, the variable name changed to *air_pollution_-_particulate_matter*. I update the values from 2010-2012 to reflect the change in the variable name.
* From 2010-2012, lead poisoned children was named *lead_poisoned_children_(wi)*. Starting in 2013, the variable name changed to *lead_poisoned_children*. I update the values from 2010-2012 to reflect the change in the variable name.
* In 2011, 2012, and 2013 I change *ratio_of_population_to_primary_care* to *primary_care_physicians*, and I change the stem value to *ratio*.
* In 2011, 2012, and 2013 I change *ratio_of_population_mental_health* to *mental_health_providers*, and I change the stem value to *ratio*.
* In 2012 and 2013 I change *ratio_of_population_to_dentists* to *dentists*, and I change the stem value to *ratio*.
