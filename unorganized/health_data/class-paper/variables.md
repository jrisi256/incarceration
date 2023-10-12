# Variables

* State-level laws around drug usage (Meara et al. 2016, Doleac & Mukherjee 2018, Packham 2022, and Reader et al. 2022).
  * Naloxone access. Law Atlas and Doleac & Mukherjee 2018.
  * Syringe exchange opening (see Packham 2022). Law Atlas has syringe services, generally.
  * See LawAtlas in general.
  * Meara et al. 2016 link does not work.
  * See Reader et al. 2022 for differentiation in Good Samaritan laws.
  * Packham 2022 has a nice list of laws they control for.
* Dependent Variable
  * Overdose deaths (see Doleac & Mukherjee 2018, Packham 2022, Hedegaard 2020, Efranian et al. 2019) for drug overdose definitions.
  * Reader et al. 2022 may have which specific overdose deaths I should be looking at.
* Controls
  * County Business Patterns - Number of drug treatment centers (also include number of hospitals?)
  * Rurality.
  * Population.
  * Percent poverty.
  * Unemployment.

## Efranian et al. 2019 - variables

* Opioid overdose rates, "We included overdose deaths coded as unintentional (X40-X44), homicide (X85), undetermined intent (Y10-Y14), and suicide cases (X60-X64)... Among deaths with opioid overdose as the underlying cause, the type of opioid involved is indicated by the following ICD-10 multiple cause-of-death codes: opioids (T40.0, T40.1, T40.2, T40.3, T40.4, or T40.6); heroin (T40.1); natural and semi-synthetic opioids (T40.2); methadone (T40.3); and synthetic opioids other than methadone (T40.4)." page 57 of document, 13 of pdf.
* They include multiple ways of coding Naloxone access: binary, number of provisions (a sequence of binary variables), and days after Naloxone access law was passed.
* Controls
  * Census
    * Per pupil spending on education
    * Poverty rate
    * Unemployment rate
    * Population density
    * Uninsured rate.
  * Non-Census
    * Income inequality.
    * High school attainment/college attainment.
    * Per capita income (FRED).
    * Median income (FRED).
    * **Employment in mining, construction, and manufacturing** (BLS) divided by the size of the total labor force.
  * **Drug Access**
    * Drug arrests (FBI UCR and NIBRS).
    * Availability of prescription opioids (ARCOS) as measured by the number of controlled substances transactions of prescriptions.
    * Nosrati et al. 2019 use opioid prescription rates from the CDC.
  * **Law Enforcement Investment**
    * Log of police officers per capita (LEOKA), only available at the jurisdiction level, see Doleac & Mukherjee 2018.
  * Fixed effects for area (state, county, jurisdiction).
  * Controls for time (month, quarter, year).
  * Interacted fixed effects for area and time.
  * **Marijuana access laws**
    * Efranian et al. 2019 make the interesting suggestion that marijuana can act as a substitute for opioids. So the degree to which marijuana is allowed, one should expect opioid overdoses to decline.
  * **From Doleac & Mukherjee 2018, Packham 2022**
    * Availability of drug treatment. Number of drug treatment facilities, number of hospitals from County Business Patterns (CBP).
    * Rurality (Packham 2022).
    * Packham 2022 used % Black and % Hispanic although I am not sure why. Segregation? Diversity index?