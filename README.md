# Covid Hospitalization and Vaccination
 A study of vaccination rates and hospital bed utilization in the United States

# Project Description

## About

This is a study of vaccination rate and hospital bed usage in the United States inspired by the Washington Post article [Mapping America’s hospitalization and vaccination divide](https://www.washingtonpost.com/health/2021/09/23/covid-vaccination-hospitalization-map/). In this project we will be recreating the USA map found in the article and adding interactivity so different dates and variables can be selected.

#### **Map By Zach Levitt and Dan Keating:**

![Washington Post Bivariate Choropleth Map](washington-post-map.png)

## Methods

Vaccination data is provided at the county level by the CDC and hospital bed usage data is provided by HealthData.gov. These variables are visualized with a bivariate choropleth map of Hospital Referral Regions in the United States.

Vaccination Rate is defined by county and HRRs are defined by zip code. We can't use county data to calculate the vaccination rate of an HRR because these regions overlap. Zip codes in one HRR can live in different counties, and Zip codes in different counties can live in the same HRR.

**We need to know both the population of each HRR and the vaccination rate of each part of that population.**

We determine the vaccination rate of the HHRs by averaging the vaccination rate (given by county) of the zip codes in that HRR. The individual zip code's vaccination rate needs to be weighted by that zip code's population. Population data is obtained from the United States Census Bureau, which is unfortunately not counted by zip code, but by blocks that make up the congressional districts. 

To find zip code population we will use a a Zip Code to Congressional District Crosswalk.
To find all zip codes in an HRR we will use a Zip Code to HRR crosswalk.

# Data Sources

## Summery

1. Hospital Bed Usage in USA - per hospital
2. Vaccination Rates in USA - per county
3. Population Census in USA - per district
4. HRR Geography in USA - per HRR number
5. County Geography in USA - per county
6. Crosswalk for Zip Code to Congressional District Tract
7. Crosswalk for Zip Code and HRR number

Anytime a new file is downloaded, that file is cached in the "cached-data" folder. Anytime a request is made 
for a dataset by date, this folder is checked first.

## Local Data Caching

All data sources are downloaded from the internet. Only the needed portions of the datasets are requested from the corresponding endpoints. 
Before downloading, this application first checks if the dataset has already been downloaded in a local cache 
file. Whenever a new portion of a dataset is downloaded, it is saved to the cache folder local to the application 
folder.

## Vaccination Rates Data per US County

Vaccination rates are obtained from the CDC: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh. 
This dataset is large, so instead of downloading the whole dataset, this application accesses it through the SODA API and retrieves only the 
relevant rows and columns.

The "COVID-19 Vaccinations in the United States,County" data provides counts and percentages of people who have been 
vaccinated in each county of the United States.

The variables retrieved are:

* **fips**
* **series_complete_pop_pct**: "Percent of people who have completed a primary series (have second dose of a two-dose vaccine or one dose of a single-dose vaccine) based on the jurisdiction and county where vaccine recipient lives."
* **administered_dose1_pop_pct**: "Percent of Total Pop with at least one Dose by State of Residence"
* **booster_doses_vax_pct**: "Percent of people who completed a primary series and have received a booster (or additional) dose."

## Hospital Capacity Data of USA per Hospital

Hospital bed usage counts is obtained from HealthData.gov: https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u. 
This dataset is large, so instead of downloading the whole dataset, this application accesses it through the SODA API and retrieves only the 
relevant rows and columns.

The "COVID-19 Reported Patient Impact and Hospital Capacity by Facility" data provides counts on hospital bed utilization that is aggregated weekly.

The variables retrieved are:

* **Hospital_name** and **fips_code**
* **inpatient_beds_7_day_avg**: "Average number of total number of staffed inpatient beds in your hospital including all overflow, observation, and active surge/expansion beds used for inpatients (including all ICU beds) reported in the 7-day period."
* **inpatient_beds_used_7_day_avg**: "Average of total number of staffed inpatient beds that are occupied reported during the 7-day period."
* **inpatient_beds_used_covid_7_day_avg**: "Average of reported patients currently hospitalized in an inpatient bed who have suspected or confirmed COVID-19 reported during the 7-day period."

*Inpatient bed counts are used instead of total bed counts because many hospitals that only have inpatient bed data do not include data for inpatient and outpatient totals.*


#### Calculate ratios for hospital bed usage

We also calculate: 

* **bed_usage_ratio**: The ratio of hospital beds used out of 100 beds (inpatient)
* **covid_bed_usage_ratio**: The ratio of hospital beds used by covid patients out of 100 beds (inpatient)
* **covid_bed_usage_total_bed_usage_ratio**: The ratio of hospital beds used by covid patients out of all used beds (inpatient)

#### Calculate average ratios per region

The hospital bed dataset contains records for each hospital. For mapping, each map region will need to represent the average 
of all hospitals present in that region.


## Population Census Data

Population data is obtained from the United States Census Bureau through their Decennial Census of Population and Housing APIs. The specific API dataset used is called Redistricting Data (PL 94-171): https://www.census.gov/data/developers/data-sets/decennial-census.html

API Call: `api.census.gov/data/2020/dec/pl`

We obtain population data per census tract:

Variable: `P1_001N`: Total Population

## Geographic Shape Data

#### *US County Shape Data*

County shape data is obtained from the R package `Albersusa`.

#### *Hospital Referral Region Shape Data*

Shape data for USA HRR regions are downloaded from arcgis.com using their "FeatureServer" REST api.

https://www.arcgis.com/home/item.html?id=46bf6790c4e0455e9379ee9769b1a5ab


## Crosswalk Data

#### *HRR number to zip code translation (2019)*

This crosswalk is obtained from Dartmouth Atlas as a zip file.

https://data.dartmouthatlas.org/supplemental/#crosswalks

#### *Congressional District to ZIP translation (2010 Census)*

This crosswalk is obtained from Dartmouth Atlas as a zip file.

https://www.huduser.gov/portal/datasets/usps_crosswalk.html