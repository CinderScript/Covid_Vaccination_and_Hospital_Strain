library(shiny)

library(tidyverse)
library(lubridate)
library(sf)

# for map data
library(albersusa)

# for SODA API calls
library(httr)
library(jsonlite)


###############################
######### GLOBAL VARS ######### 
###############################


# hospital and vaccination data endpoints to soda api
hospital_data_soda_endpoint <- "https://healthdata.gov/resource/anag-cw7u.json"
vaccination_data_soda_endpoint <- "https://data.cdc.gov/resource/8xkx-amqh.json"

# needed to avoid soda api throttling
soda_app_key <- "3tl81UiFuoaJ1DboeqMuy5K9U"

# api url for arcgis hrr boundary shape data
hrr_boundary_arcgis_endpoint <- "https://services.arcgis.com/nzS0F0zdNLvs7nc8/arcgis/rest/services/HRR_boundaries/FeatureServer"

# url for hrr zip crosswalk zip file from Dartmouth Atlas
crosswalk_hrr_zip_url <- "https://data.dartmouthatlas.org/downloads/geography/ZipHsaHrr19.csv.zip"

#cached data
cached_data_folder_base = "cached-data"
cached_bed_data_folder = paste(cached_data_folder_base, "hospital_bed_usage", sep = "/")
cached_vaccination_data_folder = paste(cached_data_folder_base, "vaccination_rates", sep = "/")
cached_zip_hrr_crosswalk_folder = paste(cached_data_folder_base, "crosswalk", sep = "/")

cached_hrr_shapes_filepath = paste(cached_data_folder_base, "hhr_shapes_arcgis_data.shape", sep = "/")
cached_hospital_bed_data_filepath = paste(cached_bed_data_folder, "hospital_bed_usage_healthdatagov_", sep = "/")
cached_vaccination_data_filepath = paste(cached_vaccination_data_folder, "vaccination_rates_cdc_", sep = "/")

cached_zip_hrr_crosswalk_compressed_filepath = paste(cached_zip_hrr_crosswalk_folder, "hrr_zip_crosswalk.zip", sep = "/")
cached_zip_hrr_crosswalk_filepath = paste(cached_zip_hrr_crosswalk_folder, "ZipHsaHrr19.csv", sep = "/")

#############################
######### FUNCTIONS ######### 
#############################

# Returns a dataframe from the given soda api endpoint. 
get_soda_dataframe <- function(endpoint, simple_filter = NULL, where_filter, select_filter, limit = 1000){
  
  #Build the query
  soda_query = paste0(endpoint, 
                      "?",                                      
                      simple_filter,
                      "&$where=", where_filter,
                      "&$select=", select_filter,
                      "&$limit=", limit,
                      "&$$app_token=", soda_app_key)
  
  print(paste("Query: ", soda_query))
  
  # Getting response details in API
  response_details = GET(url = URLencode(soda_query))
  
  # Getting status of HTTP Call
  response_status_code = status_code(response_details)
  
  print(paste0("SODA http response: ", response_status_code))
  
  # Converting content to text
  response_content_text = content(response_details,
                                  "text", encoding = "UTF-8")
  
  # Parse Json Data
  response_content_json = fromJSON(response_content_text,
                                   flatten = TRUE)
  
  # Converting into dataframe
  soda_dataframe = as.data.frame(response_content_json)
}

######### HOSPITAL BED DATA#########

# gets dataframe with total inpatient beds, used inpatient beds, and used inpatient beds for covid patients, 
# for each hospital on record for the week given (date). This request asks for the first 10,000 records. A typical
# week on this dataset has between 4,000 and 5,000 records.
#
# columns selected from dataset: 
#               state
#               hospital_name
#               city
#               zip
#               fips_code
#               inpatient_beds_7_day_avg
#               inpatient_beds_used_7_day_avg
#               inpatient_beds_used_covid_7_day_avg
get_bed_utilization_healthdata_gov_soda_api <- function(date){
  
  get_soda_dataframe(
    endpoint = hospital_data_soda_endpoint,
    #simple_filter = "state='WA'",                              # can add this for smaller dataset for testing (fast soda api call)
    where_filter = paste0("collection_week='",date, "'"),
    select_filter = "state,hospital_name,city,zip,fips_code,inpatient_beds_7_day_avg,inpatient_beds_used_7_day_avg,inpatient_beds_used_covid_7_day_avg",
    limit = 7000) %>% 
    
    # convert values to numeric
    mutate(
      fips_code = as.numeric(fips_code),
      zip = as.numeric(zip),
      inpatient_beds_7_day_avg = as.numeric(inpatient_beds_7_day_avg),
      inpatient_beds_used_7_day_avg = as.numeric(inpatient_beds_used_7_day_avg),
      inpatient_beds_used_covid_7_day_avg = as.numeric(inpatient_beds_used_covid_7_day_avg)) %>% 
    
    # replace negative bed counts with NA
    mutate(
      inpatient_beds_7_day_avg = ifelse(inpatient_beds_7_day_avg > -1, inpatient_beds_7_day_avg, NA),
      inpatient_beds_used_7_day_avg = ifelse(inpatient_beds_used_7_day_avg > -1, inpatient_beds_used_7_day_avg, NA),
      inpatient_beds_used_covid_7_day_avg = ifelse(inpatient_beds_used_covid_7_day_avg > -1, inpatient_beds_used_covid_7_day_avg, NA))
}

# Gets hospital bed usage dataframe.
# Checks for data in the cached data folder first, then looks for the data 
# using the soda rest api. If no data file is found locally, the downloaded 
# file is saved in the cached data folder.
#
# columns in dataset: 
#               state
#               hospital_name
#               city
#               fips_code
#               inpatient_beds_7_day_avg
#               inpatient_beds_used_7_day_avg
#               inpatient_beds_used_covid_7_day_avg
get_bed_utilization_data <- function(date){
  
  # this database likes this format
  date <- ymd(date)
  
  # first we need to determine the filename of the cached data file.
  filepath = paste0(cached_hospital_bed_data_filepath, date, ".csv")
  
  if( !file.exists(filepath) ) {
    
    # get from healthdata.gov
    
    print("Hospital bed cached data was not found. Retrieving from healthdata.gov (soda api)...")
    bed_data = get_bed_utilization_healthdata_gov_soda_api(date = date)
    
    write_csv(bed_data, filepath)
    print("File retrieved and cached for future use.")
  }
  else {
    
    # get from cached data file
    
    bed_data = read_csv(filepath, show_col_types = FALSE)
    print("Hospital bed data loaded from local cache.")
  }
  
  return(bed_data)
}


######### VACCINATION DATA #########

# gets dataframe with vaccination records from the CDC using the soda api
# converts the column's data types to those loaded by read_csv by default 
# so that data returned from the soda endpoint is the same as the data loaded 
# from the cache file.
#
# There are around 3300 records per date in this dataset.
#
# columns selected from dataset: 
#               fips
#               series_complete_pop_pct
#               administered_dose1_pop_pct
#               booster_doses_vax_pct
get_vaccination_rates_cdc_soda_api <- function(date){
  
  data = get_soda_dataframe(
    endpoint = vaccination_data_soda_endpoint,
    #    simple_filter = "recip_state='WA'",                              # can add this for smaller dataset for testing (fast soda api call)
    where_filter = paste0("date='",date, "'"),
    select_filter = "date,fips,series_complete_pop_pct,administered_dose1_pop_pct,booster_doses_vax_pct,completeness_pct,census2019",
    limit = 10000)
  
  ## Data needs to be converted so it is consistent. We want it to load from the csv 
  ## the same way it is retrieved from the soda endpoint
  
  data <- data %>% mutate(
    date = as_date(date))
  
  if("fips" %in% colnames(data))
    data <- data %>% mutate (fips = as.numeric(fips))
  
  if("series_complete_pop_pct" %in% colnames(data))
    data <- data %>% mutate (series_complete_pop_pct = as.numeric(series_complete_pop_pct))
  
  if("administered_dose1_pop_pct" %in% colnames(data))
    data <- data %>% mutate (administered_dose1_pop_pct = as.numeric(administered_dose1_pop_pct))
  
  if("booster_doses_vax_pct" %in% colnames(data))
    data <- data %>% mutate (booster_doses_vax_pct = as.numeric(booster_doses_vax_pct))
  
  return(data)
}


# gets dataframe with vaccination records.
# first checks for a locally cached data file
# If no data file is found locally, the downloaded file
# is saved in the cached data folder.
#
# columns selected from dataset: 
#               fips
#               series_complete_pop_pct
#               administered_dose1_pop_pct
#               booster_doses_vax_pct
get_vaccination_rates_data <- function(date){
  
  # ymd is how arguments should be entered and files should be saved
  date <- ymd(date)
  
  # first we need to determine the path/name of the cached data file.
  filepath = paste0(cached_vaccination_data_filepath, date, ".csv")
  
  if( !file.exists(filepath) ) {
    # get from CDC
    
    print("Vaccination Rates cached data was not found. Retrieving from data.cdc.gov (soda api)...")
    data = get_vaccination_rates_cdc_soda_api(date = date)
    
    write_csv(data, filepath)
    print("Vaccination records file retrieved and cached for future use.")
  }
  else {
    # get from cached data file
    
    data = read_csv(filepath, show_col_types = FALSE)
    print("Vaccination records loaded from local cache.")
  }
  
  return(data)
}


######### HRR BOUNDARY MAP DATA ######### 
# get hrr shapefile data from arcgis
get_hrr_shapes_arcgis <- function() {
  
  if( !file.exists(cached_hrr_shapes_filepath) ) {
    
    print("No cached hrr shapefile was found. Downloading from arcgis and saving to file...")
    
    # get and save the file
    url = parse_url(hrr_boundary_arcgis_endpoint)
    url$path = paste(url$path, "0/query", sep = "/")
    url$query = list(where = "HRRNUM > -1",
                     outFields = "*",
                     returnGeometry = "true",
                     f = "geojson")
    request = build_url(url)
    
    hrr_shape_data = st_read(request)
    
    st_write(hrr_shape_data, cached_hrr_shapes_filepath, driver = "GeoJSON")
  }
  else {
    hrr_shape_data = st_read(cached_hrr_shapes_filepath)
    print("HRR shapes data loaded from local cache.")
  }
  
  return(hrr_shape_data)
}

######### HRR CROSSWALK DATA ######### 
# Get hrr zipcode crosswalk from Dartmouth Atlas zip file
get_hrr_zip_crosswalk <- function(){
  if( !file.exists(cached_zip_hrr_crosswalk_compressed_filepath) ) {
    print("Downloadeing Zip HRR Crosswalk data from Dartmouth Atlas.")
    download.file(crosswalk_hrr_zip_url, cached_zip_hrr_crosswalk_compressed_filepath)
    print("Unzipping...")
    unzip(cached_zip_hrr_crosswalk_compressed_filepath, exdir = cached_zip_hrr_crosswalk_folder)  # unzip your file 
  }
  
  print("Loading crosswalk data from file.")
  return(read.csv(cached_zip_hrr_crosswalk_filepath) %>% select(zipcode19, hrrnum))
}


#### misc ####

create_data_folder_if_dne <- function(){
  if ( !dir.exists(file.path(cached_data_folder_base)) ) {
    print("Creating cached data folder...")
    dir.create(cached_data_folder_base)
    dir.create(cached_bed_data_folder)
    dir.create(cached_vaccination_data_folder)
    dir.create(cached_zip_hrr_crosswalk_folder)
  }
  else
    print("Founds cached data folder.")
}


########################################
######### MAPPING FUNCTIONS ############
########################################

### Graph Vaccination Rates
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_By_County <- function(date) {
  vaccination_data = get_vaccination_rates_data(date = date)
  
  vaccination_data = us_county_shape_data %>% 
    left_join(vaccination_data, by = "fips") 
  
  vaccination_data %>% 
    ggplot() +
    geom_sf(aes(fill = series_complete_pop_pct/100)) +
    scale_fill_continuous("Fully Vaccinated", low="yellow", high="red", labels = scales::percent) +
    ggtitle("COVID Vaccination Status", subtitle = "Percentage of county population that is fully vaccinated (from CDC)") +
    my_map_theme()
}

### Graph Hospital Bed Usage (county)
# Date must be given in yyyymmdd format
Graph_Hospital_Bed_Usage_By_County <- function(date){
  hospital_bed_data <- get_bed_utilization_data(date = date)
  
  # calculate ratios
  hospital_bed_data <- hospital_bed_data %>% 
    mutate(
      bed_usage_ratio = inpatient_beds_used_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_total_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_used_7_day_avg
    )
  
  
  county_bed_ratios <- hospital_bed_data %>% 
    group_by(fips_code) %>% 
    summarise(
      bed_usage_ratio = mean(bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_ratio = mean(covid_bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_total_bed_usage_ratio = mean(covid_bed_usage_total_bed_usage_ratio, na.rm = TRUE))
  
  
  
  us_county_shape_data %>% 
    left_join(county_bed_ratios, by = c("fips" = "fips_code")) %>% 
    
    ggplot() +
    geom_sf(aes(fill = covid_bed_usage_ratio/100)) +
    scale_fill_continuous("Beds Used for Covid", low="deepskyblue", high="green", labels = scales::percent) +
    ggtitle("Hospital Bed Usage", subtitle = "Percentage of county's total hospital beds used by covid patients") +
    my_map_theme()
}

### Graph Hospital Bed Usage (hrr)
# Date must be given in yyyymmdd format
Graph_Hospital_Bed_Usage_By_HRR <- function(date){
  hospital_bed_data <- get_bed_utilization_data(date)
  
  # calculate ratios
  hospital_bed_data <- hospital_bed_data %>% 
    mutate(
      bed_usage_ratio = inpatient_beds_used_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_7_day_avg * 100,
      covid_bed_usage_total_bed_usage_ratio = inpatient_beds_used_covid_7_day_avg / inpatient_beds_used_7_day_avg
    )
  
  hrr_bed_ratios <- hospital_bed_data %>% 
    left_join(zip_hrr_crosswalk_data, by = c("zip" = "zipcode19")) %>% 
    group_by(hrrnum) %>% 
    summarise(
      bed_usage_ratio = mean(bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_ratio = mean(covid_bed_usage_ratio, na.rm = TRUE), 
      covid_bed_usage_total_bed_usage_ratio = mean(covid_bed_usage_total_bed_usage_ratio, na.rm = TRUE))
  
  hrr_ggplot_data <- hrr_shape_data %>% 
    left_join(hrr_bed_ratios, by = c("HRRNUM" = "hrrnum")) %>% 
    select(!HRR)  %>% 
    st_transform(crs= "EPSG:2163")
  
  
  hrr_ggplot_data %>% 
    ggplot() +
    geom_sf(aes(fill = covid_bed_usage_ratio/100)) +
    scale_fill_continuous("Beds Used for Covid", low="purple", high="orange", labels = scales::percent) +
    ggtitle("Hospital Bed Usage", subtitle = "Percentage of hospital referral region's beds used by covid patients") +
    my_map_theme()
}


# ggplot theme function
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}


#############################
######### SETUP #############
#############################

# setup for file caching
create_data_folder_if_dne()

# get county shapes for mapping
us_county_shape_data <- counties_sf("laea") %>% 
  mutate(fips = as.numeric(as.character(fips)))

# Get hrr shapes for mapping
hrr_shape_data <- get_hrr_shapes_arcgis()

# Get hrr / zip code crosswalk
zip_hrr_crosswalk_data <- get_hrr_zip_crosswalk()








##### test
generate_map <- function(){
  Graph_Vaccination_Rates_By_County("2021/09/24")
}


#############################
######### SHINY UI ########## 
#############################

ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("usaMap")
        )
    )
)


##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output) {

    output$usaMap <- renderPlotly({
        generate_map()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
