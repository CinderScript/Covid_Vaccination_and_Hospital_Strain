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
graph_plotly_choropleth <- function(graph){
  ggplotly(graph, tooltip = "text") %>% 
    style(hoveron = "fills") %>% 
    style(text = "No Data for HRR", traces = length(.$x$data)-2) %>%  # skip 2nd to last (NA values shape)
    style(hoverinfo = "skip", traces = length(.$x$data)-1)            # skip last trace (borders)
}

graph_plotly_point_plot <- function(graph){
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
      scale_y_continuous(y_axis_label, labels=scales::percent) +
      theme( panel.border = element_rect(fill = NA, colour = "black") )
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
    geom_point(
      aes(fill = population), 
      size = 3.5, pch = 21, 
      color = "black", 
      alpha = 0.9) +
    scale_fill_continuous(
      "HRR Population", 
      trans = "log10",
      type = "gradient", 
      labels = scales::comma,
      low = "blue",
      high = "gold",
      guide = guide_colorbar(
        frame.colour = "black"))  +
    geom_smooth(method = lm, se = F, linewidth = 1) +
    scale_x_continuous(x_axis_label, labels=scales::percent) +
    scale_y_continuous(y_axis_label, labels=scales::percent) +
    
    theme(legend.key.size = unit(1, 'cm'),
          text = element_text(size = 16),
          legend.key = element_rect(colour = "black"),
          panel.border = element_rect(fill = NA, colour = "black"))
}


### Vaccination Rate by HHR Choropleth Function

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
# Values for `display_stat`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_Choropleth_By_Hrr <- function(date, display_stat, is_scale_range_adaptive = F) {
  
  # translate input params
  stat_label = paste0(possible_axis_labels[display_stat])
  graph_stat = rlang::parse_expr(display_stat)
  valid_data_dates = closest_valid_dates(date)
  
  # GET VACCINATION STATS
  vaccination_data = calculate_hrr_vaccination_rates(valid_data_dates[1])
  
  # COMBINE WITH HHR SHAPES
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
  
  # GET SHAPE OF NA ZONES
  na_value_shapes = hrr_ggplot_data %>% 
    filter(is.na(!!graph_stat))
  
  
  # REMOVE TRACES THAT ARE COVERED 
  # (for performance when converting to plotly)
  hrr_ggplot_data = hrr_ggplot_data %>% 
    filter(!is.na(!!graph_stat))
  
  
  # CALCULATE LIMITS OF THE SCALE
  scale_limits = c(0,1)
  if (is_scale_range_adaptive) {
    max = max(vaccination_data[,display_stat])
    min = min(vaccination_data[,display_stat])
    scale_limits = c(min/100, max/100) #make percentage
  }  
  
  ### Graph it
  p = hrr_ggplot_data %>% 
    ggplot() +
    geom_sf(                                  # HRR data
      aes(fill = !!graph_stat/100 + runif(nrow(hrr_ggplot_data), min=0, max=0.001), 
          text=text), 
      linewidth = 0.1, 
      color=alpha("darkgreen",0.8))
  
  if (nrow(na_value_shapes) > 0) {
    p = p + geom_sf(data = na_value_shapes,            # regions with NA values (for tooltip value) - trace 290
                    fill = "gray",
                    color="gray")
  }
  
  p + geom_sf(data = us_state_shape_data,        # borders: trace 291 (skip hover info)
              fill = alpha("black", 0.0)) +   
    scale_fill_continuous(
      stat_label, 
      type = "viridis", 
      labels = scales::percent, breaks = c(0, .2, .40, .60, .8, 1),  
      limits = scale_limits) +
    my_map_theme()
}

### Vaccination Rate by HHR Choropleth Function - Static

### Stat Selectable Interactive Graph of Vaccination Rates by HRR
# Values for `display_stat`:
#         vacc_complete_percent: Percentage of people fully vaccinated in that HRR
#         single_dose_percent:   Percentage of people with one vaccine dose in that HRR
# Date must be given in yyyymmdd format
Graph_Vaccination_Rates_Choropleth_By_Hrr_Static <- function(date, display_stat, is_scale_range_adaptive = F) {
  
  stat_label = paste0(possible_axis_labels[display_stat])
  graph_stat = rlang::parse_expr(display_stat)
  valid_data_dates = closest_valid_dates(date)
  
  print(paste("Mapping vaccination level: ", stat_label))
  
  vaccination_data = calculate_hrr_vaccination_rates(valid_data_dates[1])
  
  hrr_ggplot_data = hrr_shape_data %>% 
    left_join(vaccination_data, by = c("HRRNUM" = "hrrnum")) %>% 
    select(!HRR)  %>% 
    st_transform(crs= "EPSG:2163")
  
  # Find Scale limits
  scale_limits = c(0,1)
  if (is_scale_range_adaptive) {
    max = max(vaccination_data[,display_stat])
    min = min(vaccination_data[,display_stat])
    scale_limits = c(min/100, max/100) #make percentage
  }
  
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
      limits = scale_limits) +
    my_map_theme() +
    theme(legend.key.size = unit(1, 'cm'),
          text = element_text(size = 16))
}
