library(plotly)

source("covid_study_data_wrangler.R")

### possible statistic graphing choices to pass to graphing functions

possible_axis_labels <- c(
  "vacc_complete_percent"                 = "Fully Vaccinated",
  "single_dose_percent"                   = "Has Single Dose",
  "bed_usage_ratio"                       = "Hospital Beds Used",
  "covid_bed_usage_ratio"                 = "Hospital Beds Used for Covid",
  "covid_bed_usage_total_bed_usage_ratio" = "Covid Bed to Total Bed Usage")


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

# converts generated plots to an interactive plot (plotly)
graph_interactive_map <- function(graph){
  ggplotly(graph, tooltip = "text") %>% 
    style(hoveron = "fills") 
}

graph_interactive_plot <- function(graph){
  ggplotly(graph, tooltip = "text") %>% 
    style(hoveron = "color") 
}

# ggplot theme function
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}


### Vaccination and Hospitalization by HHR Point Plot Function

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
#
# Values for `x_axis` and `y_axis`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
#         bed_usage_ratio:                         Percentage of hospital beds used in HRR
#         covid_bed_usage_ratio:                   Percentage of beds used for COVID in HRR
#         covid_bed_usage_total_bed_usage_ratio:   Ratio of COVID bed usage to total bed usage
#
# Date must be given in yyyymmdd format
Graph_Vaccination_Hospitalization_Plot <- function(date, x_axis, y_axis){
  
  x_axis_stat = rlang::parse_expr(x_axis)
  y_axis_stat = rlang::parse_expr(y_axis)
  
  x_axis_label = paste0(possible_axis_labels[x_axis])
  y_axis_label = paste0(possible_axis_labels[y_axis])  
  
  dates = closest_valid_dates(ymd(date))
  
  ### GET ALL NEEDED DATA
  calculate_bivariate_covid_vaccination_hospital_bed_aggrigate(dates) %>% 
    
    drop_na() %>%                                         #drop unplottable rows
    
    ### CREATE PLOTLY LABELS
    mutate(text = paste0(
      "HRR #: ", hrrnum,
      "</b>\nState: ", HRRSTATE_long, 
      "</b>\nFully Vaccinated: ", format(vacc_complete_percent, digits = 2), "%",
      "</b>\nHad Single Dose: ", format(single_dose_percent, digits = 1), "%",
      "</b>\nZip Code Count: ", zip_count,
      "</b>\nHRR Pop: ", format(population, big.mark = ","))) %>% 
    
    mutate(vacc_complete_percent = vacc_complete_percent / 100,
           single_dose_percent = single_dose_percent/100,
           covid_bed_usage_ratio = covid_bed_usage_ratio/100,
           bed_usage_ratio = bed_usage_ratio/100) %>% 
    
    ### PLOT DATA 
    ggplot(aes(x = !!x_axis_stat, y = !!y_axis_stat)) +
      geom_point(color = "black", size = 1.4) +
      geom_point(aes(color = population, text=text), size = 1.2) +
      scale_color_continuous(
        "HRR Population", 
        trans = "log10",
        type = "gradient", 
        labels = scales::comma,
        low = "blue",
        high = "gold") +
      geom_smooth(method = lm, se = F, linewidth = 0.5) +
      scale_x_continuous(x_axis_label, labels=scales::percent) +
      scale_y_continuous(y_axis_label, labels=scales::percent)
}


### Vaccination and Hospitalization by HHR Point Plot Function - STATIC

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
#
# Values for `x_axis` and `y_axis`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
#         bed_usage_ratio:                         Percentage of hospital beds used in HRR
#         covid_bed_usage_ratio:                   Percentage of beds used for COVID in HRR
#         covid_bed_usage_total_bed_usage_ratio:   Ratio of COVID bed usage to total bed usage
#
# Date must be given in yyyymmdd format
Graph_Vaccination_Hospitalization_Plot_Static <- function(date, x_axis, y_axis){
  
  x_axis_stat = rlang::parse_expr(x_axis)
  y_axis_stat = rlang::parse_expr(y_axis)
  
  x_axis_label = paste0(possible_axis_labels[x_axis])
  y_axis_label = paste0(possible_axis_labels[y_axis])  
  
  dates = closest_valid_dates(ymd(date))
  
  ### GET ALL NEEDED DATA
  calculate_bivariate_covid_vaccination_hospital_bed_aggrigate(dates) %>% 
    
    drop_na() %>%                                         #drop unplottable rows
    
    mutate(vacc_complete_percent = vacc_complete_percent / 100,
           single_dose_percent = single_dose_percent/100,
           covid_bed_usage_ratio = covid_bed_usage_ratio/100,
           bed_usage_ratio = bed_usage_ratio/100) %>% 
    
    ### PLOT DATA 
    ggplot(aes(x = !!x_axis_stat, y = !!y_axis_stat)) +
    geom_point(aes(color = population), size = 1.8, alpha = 0.7) +
    scale_color_continuous(
      "HRR Population", 
      trans = "log10",
      type = "gradient", 
      labels = scales::comma,
      low = "blue",
      high = "gold") +
    geom_smooth(method = lm, se = F, linewidth = 0.5) +
    scale_x_continuous(x_axis_label, labels=scales::percent) +
    scale_y_continuous(y_axis_label, labels=scales::percent)
}


### Vaccination Rate by HHR Choropleth Function

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
# Values for `display_stat`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_Choropleth_By_Hrr <- function(date, display_stat) {
  
  stat_label = paste0(possible_axis_labels[display_stat])
  graph_stat = rlang::parse_expr(display_stat)
  valid_data_dates = closest_valid_dates(date)
  
  print(paste("Mapping vaccination level: ", stat_label))
  
  vaccination_data <- calculate_hrr_vaccination_rates(valid_data_dates[1])
  
  hrr_ggplot_data = hrr_shape_data %>% 
    left_join(vaccination_data, by = c("HRRNUM" = "hrrnum")) %>% 
    select(!HRR)  %>% 
    st_transform(crs= "EPSG:2163") %>% 
    mutate(text = paste0(
      "HRR #: ", HRRNUM,
      "</b>\nState: ", HRRSTATE_long, 
      "</b>\nFully Vaccinated: ", format(vacc_complete_percent, digits = 4), "%",
      "</b>\nHad Single Dose: ", format(single_dose_percent, digits = 4), "%",
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
      stat_label, 
      type = "viridis", 
      labels = scales::percent, breaks = c(0, .2, .40, .60, .8, 1),  
      limits= c( 0, 1)) +
    my_map_theme()
}

### Vaccination Rate by HHR Choropleth Function - Static

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
# Values for `display_stat`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_Choropleth_By_Hrr_Static <- function(date, display_stat) {
  
  stat_label = paste0(possible_axis_labels[display_stat])
  graph_stat = rlang::parse_expr(display_stat)
  valid_data_dates = closest_valid_dates(date)
  
  print(paste("Mapping vaccination level: ", stat_label))
  
  vaccination_data <- calculate_hrr_vaccination_rates(valid_data_dates[1])
  
  hrr_ggplot_data = hrr_shape_data %>% 
    left_join(vaccination_data, by = c("HRRNUM" = "hrrnum")) %>% 
    select(!HRR)  %>% 
    st_transform(crs= "EPSG:2163")
  
  hrr_ggplot_data %>% 
    ggplot() +
    geom_sf(
      aes(fill = !!graph_stat/100 + runif(nrow(hrr_ggplot_data), min=0, max=0.001)), 
      linewidth = 0, 
      color=alpha("black",0.05)) +
    geom_sf(data = us_state_shape_data, fill = alpha("black", 0.0)) +
    scale_fill_continuous(
      stat_label, 
      type = "viridis", 
      labels = scales::percent, breaks = c(0, .2, .40, .60, .8, 1),  
      limits= c( 0, 1)) +
    my_map_theme()
}
