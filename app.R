library(shiny)
library(plotly)
source("hospitalization_vaccination_data_loading.R")

### Load Required Data
# Get Geographic, Census, and Crosswalk Data

# Setup the data caching folder
create_data_folder_if_dne()

# get county shapes for mapping
us_county_shape_data <- counties_sf("laea") %>% 
  mutate(fips = as.numeric(as.character(fips)))

# get state shapes for mapping
us_state_shape_data <- usa_sf("laea")

# Get hrr shapes for mapping
hrr_shape_data <- get_hrr_shapes_arcgis()

# Get Required Crosswalks
zip_hrr_crosswalk_data <- get_hrr_zip_crosswalk()

# get population census data
zcta_population_data <- get_zcta_population_data()


### Calculate zcta-fips population percentages needed for 
### converting County statistics to Zip Code statistics

calculate_hrr_population_zip_slices <- function(){
  # get population totals of hrr
  hrr_population_zip_slice = zip_hrr_crosswalk_data %>% 
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
  
  print(paste(
    "After joining US state zip codes with ZCTAs, We keep about", 
    percent_retained, 
    "% of the population, loosing", 
    number_lost, "people." ))
  
  print(paste(
    "zctas also account for the population in us territories, which have a total population of", 
    format(us_territories_pop_wiki, big.mark = ","), "according to wikipedia."))
  print(paste(
    "This pushes the retained popoulation of the states closer to",
    format(total_hrr_pop / usa_zcta_pop * 100, digits = 4), "percent" ))
  
  return(hrr_population_zip_slice)
}

hrr_population_zip_slice <- calculate_hrr_population_zip_slices()


### Calculation of Vaccination Rates in HRRs for use with graphing functions
# combines fips vaccination statistics with zcta-fips population counts and
# zip code to HHR data to calculate the weighted percentages (by population) each
# county-based statistic contributes to the HRR.
calculate_hrr_vaccination_rates <- function(date) {
  
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
      vacc_complete_percent = sum(zip_slice_weighted_vacc_percent),
      single_dose_percent = sum(zip_slice_weighted_single_dose_percent))
}

### Vaccination Rate by HHR Graphing Function

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
# Values for `display_stat`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_By_Hrr <- function(date, display_stat) {
  
  graph_stat = enquo(display_stat)
  
  vaccination_data <- calculate_hrr_vaccination_rates(date)
  
  hrr_ggplot_data = hrr_shape_data %>% 
    left_join(vaccination_data, by = c("HRRNUM" = "hrrnum")) %>% 
    select(!HRR)  %>% 
    st_transform(crs= "EPSG:2163") %>% 
    mutate(text = paste0(
      "State: ", HRRSTATE_long, 
      "</b>\nFully Vaccinated: ", format(vacc_complete_percent, digits = 4), "%",
      "</b>\nHad Single Dose: ", format(single_dose_percent, digits = 4), "%",
      "</b>\nHRR #: ", HRRNUM,
      "</b>\nZip Code Count: ", hrr_population_zip_slice$zip_count[HRRNUM],
      "</b>\nHRR Pop: ", hrr_population_zip_slice$population[HRRNUM]))
  
  hrr_ggplot_data %>% 
    ggplot() +
    geom_sf(
      aes(fill = !!graph_stat/100 + runif(nrow(hrr_ggplot_data), min=0, max=0.001), 
          text=text), 
      linewidth = 0.1, 
      color=alpha("black",0.5)) +
    scale_fill_continuous(
      "HRR Population", 
      type = "viridis", 
      labels = scales::percent, breaks = c(0, .2, .40, .60, .8, 1),  
      limits= c( 0, 1)) +
    my_map_theme()
}


### Method for selecting appropriate dates for data

valid_vaccination_dates <- read_csv("valid_vaccination_dates.csv", show_col_types = F)$Date
valid_bed_usage_dates <- read_csv("valid_bed_usage_dates.csv", show_col_types = F)$collection_week

closest_valid_dates <- function(date, list) {
  
  closest_index = which.min(abs(ymd(date) - valid_vaccination_dates))
  vac_date = valid_vaccination_dates[closest_index]
  
  closest_index = which.min(abs(ymd(date) - valid_bed_usage_dates))
  bed_date = valid_bed_usage_dates[closest_index]
  
  return(c(vac_date, bed_date))
  
}


vaccination_stats <- list("Fully Vaccinated" = "vacc_complete_percent",
                          "At Least One Dose" = "single_dose_percent")

#############################
######### SHINY UI ########## 
#############################

ui <- fluidPage(

    # Application title
    titlePanel("COVID Vaccination Statistics"),

    # Create separate tabs and inputs for pitching and batting 
    tabsetPanel(id = "tab",
                tabPanel(title = "Vaccination Levels", value = "vaccination",
                         sidebarPanel(selectInput("vaccination_level",
                                                  "Vaccination Level:",
                                                  choices = vaccination_stats, 
                                                  selected = "vacc_complete_percent")),
                         sidebarPanel(sliderInput("selected_date",
                                                  "Selected vaccination status date:",
                                                  min = as.Date("2020-12-13", "%Y-%m-%d"),
                                                  max = as.Date("2023-03-03", "%Y-%m-%d"),
                                                  value = as.Date("2021/09/24"),
                                                  timeFormat="%Y-%m-%d"))
                ),
                tabPanel(title = "Hospital Bed Usage", value = "bed",
                         sidebarPanel(selectInput("p_stat",
                                                  "Statistic to Plot:",
                                                  choices = c("1", "2", "3"), 
                                                  selected = "W")),
                         sidebarPanel(sliderInput("selected_date2",
                                                  "Number of Players per Season to Display:",
                                                  min = as.Date("2020-12-13", "%Y-%m-%d"),
                                                  max = as.Date("2023-03-03", "%Y-%m-%d"),
                                                  value = as.Date("2021/09/24"),
                                                  timeFormat="%Y-%m-%d"))
                )
    ),
      
      # Show our plot
      mainPanel(
          h2("Vaccination Rates in the USA"),
          h3(textOutput("statistic_description")),
          h4("percent of Healthcare Referral Region's population"),
          plotlyOutput("usaMap"),
          h5(textOutput("vacc_data_date"))
        )
    )



##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output) {

  output$usaMap <- renderPlotly({
      valid_data_dates = closest_valid_dates(input$selected_date)
      Graph_Vaccination_Rates_By_Hrr(date = valid_data_dates[1], display_stat = vacc_complete_percent)
  })
  
  output$statistic_description <- renderText({
    stat = input$vaccination_level
    paste("Showing vaccination level:", stat)
  })
  
  output$vacc_data_date <- renderText({
    valid_data_dates = closest_valid_dates(input$selected_date)
    paste("Vaccination data from:", valid_data_dates[1])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
