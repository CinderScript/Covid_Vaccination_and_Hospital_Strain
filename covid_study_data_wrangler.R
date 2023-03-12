
# Setup all vars and static data needed for data wrangling
source("covid_study_data_loader.R")


# get population totals of hrr

# Approximate zip code population using ZCTA populations
hrr_population_zip_slice <- zip_hrr_crosswalk_data %>% 
  left_join(
    zcta_population_data, 
    by = c("zipcode19" = "ZCTA5")) %>%
  group_by(hrrnum) %>% 
  summarise(population = sum(POPPT, na.rm = T), zip_count = n())


## check how well the join worked ##

total_zcta_pop = sum(zcta_population_data$POPPT, na.rm = T)
us_territories_pop_wiki = 3623895
usa_zcta_pop = total_zcta_pop - us_territories_pop_wiki

total_hrr_pop = sum(hrr_population_zip_slice
                    $population, na.rm = T)

percent_retained = format(total_hrr_pop / total_zcta_pop * 100, digits = 4) 
number_lost = format(total_zcta_pop - total_hrr_pop, big.mark=",")

print("Approximated zip code population using ZCTA populations: ")
print(paste(
  "--After joining US state zip codes with ZCTAs, We keep about", 
  percent_retained, 
  "% of the population, loosing", 
  number_lost, "people." ))

print(paste(
  "--zctas also account for the population in us territories, which have a total population of", 
  format(us_territories_pop_wiki, big.mark = ","), "according to wikipedia."))
print(paste(
  "--This pushes the retained popoulation of the states closer to",
  format(total_hrr_pop / usa_zcta_pop * 100, digits = 4), "percent" ))


### Calculate HHR vaccination rate using ZCTA population zip code slices
# Calculation of Vaccination Rates in HRRs for use with graphing functions
# combines fips vaccination statistics with zcta-fips population counts and
# zip code to HHR data to calculate the weighted percentages (by population) each
# county-based statistic contributes to the HRR.
#
# RETURNS:
#           HRR
#           vacc_complete_percent
#           single_dose_percent
#           covid_bed_usage_total_bed_usage_ratio
calculate_hrr_vaccination_rates <- function(date) {
  
  # get administered_dose1_pop_pct, series_complete_pop_pct, fips
  vaccination_data = get_vaccination_rates_data(date)
  
  zip_hrr_vaccination_population_join = vaccination_data %>% 
    inner_join(zcta_population_data, by = c("fips" = "GEOID")) %>% 
    inner_join(zip_hrr_crosswalk_data, by = c("ZCTA5" = "zipcode19")) %>% 
    select(ZCTA5, fips, POPPT, series_complete_pop_pct, administered_dose1_pop_pct, hrrnum) %>% 
    
    rename(
      zcta = ZCTA5, 
      zip_pop_slice_in_fips = POPPT, 
      fips_vaccination_percent = series_complete_pop_pct,
      fips_one_dose_percent = administered_dose1_pop_pct) %>% 
    
    inner_join(hrr_population_zip_slice, by = "hrrnum") %>% 
    rename(total_hrr_population = population)
  
  hrr_vacc_percent = zip_hrr_vaccination_population_join %>% 
    mutate(
      num_vacc_in_zip_slice = fips_vaccination_percent * zip_pop_slice_in_fips,           # calculate number in zip of fully vaccinated
      num_one_dose_in_zip_slice = fips_one_dose_percent * zip_pop_slice_in_fips) %>%       # calculate number in zip of one dose
    mutate(
      zip_slice_weighted_vacc_percent = num_vacc_in_zip_slice / total_hrr_population,                  # calculate weighted portion of fully vaccinated
      zip_slice_weighted_single_dose_percent = num_one_dose_in_zip_slice / total_hrr_population) %>%   # calculate weighted portion of single dose
    select(hrrnum, zip_slice_weighted_vacc_percent, zip_slice_weighted_single_dose_percent) %>% 
    group_by(hrrnum) %>% 
    summarise(
      vacc_complete_percent = sum(zip_slice_weighted_vacc_percent, na.rm = T),
      single_dose_percent = sum(zip_slice_weighted_single_dose_percent, na.rm = T))
}

### Calculation of Hospital Bed Usage Rates in HRRs for use with graphing functions
# groups all hospital's by hrr (using zip code to hrr crosswalk) and calculates bed 
# usage averages.
#
# RETURNS:
#           HRR
#           bed_usage_ratio
#           covid_bed_usage_ratio
#           covid_bed_usage_total_bed_usage_ratio
calculate_hrr_bed_usage_rates <- function(date) {
  
  hospital_bed_data_per_hospital = get_bed_utilization_data(date) %>%  # don't include hospitals with 0 beds
    filter(inpatient_beds_7_day_avg > 1, state != "TX")                    # TX doesn't have vaccination data
  
  # calculate bed usage ratios
  hospital_bed_data_per_hospital = hospital_bed_data_per_hospital %>% 
    mutate(
      bed_usage_ratio = inpatient_beds_used_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_total_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_used_7_day_avg
    )
  
  # Group Hospitals in HRR
  bed_ratios_per_hrr = hospital_bed_data_per_hospital %>% 
    left_join(zip_hrr_crosswalk_data, by = c("zip" = "zipcode19")) %>% 
    group_by(hrrnum) %>% 
    summarise(
      bed_usage_ratio = mean(bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_ratio = mean(covid_bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_total_bed_usage_ratio = mean(covid_bed_usage_total_bed_usage_ratio, na.rm = TRUE))
}


calculate_bivariate_covid_vaccination_hospital_bed_aggrigate <- function(dates){
  
  ## Get Vaccination Data
  vaccination_data_per_hrr = calculate_hrr_vaccination_rates(dates[1])
  
  ## Get Hospital Data
  bed_ratio_data_per_hrr = calculate_hrr_bed_usage_rates(dates[2])
  
  ## Combine Vaccination and Bed Data (and hrr/state crosswalk for tooltip)
  bivariate_covid_vaccination_hospital_bed_aggrigate = bed_ratio_data_per_hrr %>% 
    left_join(vaccination_data_per_hrr, by = "hrrnum") %>% 
    left_join(hrr_population_zip_slice, by = "hrrnum") %>% 
    left_join(hrr_to_state, by = c("hrrnum" = "HRRNUM"))
}