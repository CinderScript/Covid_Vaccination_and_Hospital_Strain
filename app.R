library(shiny)
library(shinyjs)
#library(shinycssloaders)


source("covid_study_data_plotter.R")


vaccination_stat_choices <- list("Fully Vaccinated" = "vacc_complete_percent",
                          "At Least One Dose" = "single_dose_percent")

possible_axis_choices <- c(
  "Population Fully Vaccinated"   = "vacc_complete_percent",
  "Population With Single Dose"   = "single_dose_percent",
  "Hospital Beds Used"            = "bed_usage_ratio",
  "Hospital Beds Used for Covid"  = "covid_bed_usage_ratio",
  "Covid Bed to Total Bed Usage"  = "covid_bed_usage_total_bed_usage_ratio")


#############################
######### SHINY UI ########## 
#############################

ui <- fluidPage( useShinyjs(),

    # Application title
    titlePanel("Covid Vaccinations and their Effects on Healthcare"),

    # Create separate tabs and inputs for pitching and batting 
    tabsetPanel(id = "tab", 
                
                h4("Select Statistic and Date to Visualize"),
                
                tabPanel(title = "Vaccination Levels Choropleth", value = "vaccination_choropleth_tab",
                         sidebarPanel(selectInput("selected_map_vaccination_level",
                                                  "Vaccination Level:",
                                                  choices = vaccination_stat_choices, 
                                                  selected = "vacc_complete_percent")),
                         
                         sidebarPanel(sliderInput("selected_vaccination_map_date",
                                                  "Selected vaccination status date:",
                                                  min = as.Date("2020-12-13", "%Y-%m-%d"),
                                                  max = as.Date("2023-03-03", "%Y-%m-%d"),
                                                  value = as.Date("2021/09/24"),
                                                  timeFormat="%Y-%m-%d"))
                ),
                
                tabPanel(title = "Vaccination and Hospitalization Plot", value = "vaccination_hospitalization_plot_tab",
                         sidebarPanel(selectInput("selected_x_axis_stat",
                                                  "Statistic to Plot on X-Axis:",
                                                  choices = possible_axis_choices, 
                                                  selected = "vacc_complete_percent")),
                         
                         sidebarPanel(selectInput("selected_y_axis_stat",
                                                  "Statistic to Plot on Y-Axis:",
                                                  choices = possible_axis_choices, 
                                                  selected = "covid_bed_usage_ratio")),
                         
                         sidebarPanel(sliderInput("selected_vaccination_hospitalization_plot_date",
                                                  "Select Date of Vaccination/Hospitalization Records:",
                                                  min = as.Date("2020-12-13", "%Y-%m-%d"),
                                                  max = as.Date("2023-03-03", "%Y-%m-%d"),
                                                  value = as.Date("2021/09/24"),
                                                  timeFormat="%Y-%m-%d"))
                )
    ),
    
      # Show our plot
      mainPanel(
          h2(textOutput("graph_title")),
          h4("by Healthcare Referral Region"),
          h4(htmlOutput("statistic_description")),
          fluidRow(  column(12, plotlyOutput("graph_dynamic"))  ),
          fluidRow(  column(12, plotOutput("graph_static"))    ),
          fluidRow(
            column(2,checkboxInput("plot_dynamic_toggle", "Interactive Graph", F)), 
            column(2,checkboxInput("is_scale_range_adaptive_toggle", "Adaptive Scale Range", F))
            ),
          hr(),
          p(htmlOutput("vacc_data_date")),
          p(htmlOutput("bed_data_date"))
        )
)


##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output, session) {

  ##### RENDER GRAPHS
  output$graph_dynamic <- renderPlotly({
    #date
      is_choropleth_vacc_map = input$tab == "vaccination_choropleth_tab"
      is_graph_dynamic = input$plot_dynamic_toggle
      is_scale_range_adaptive = input$is_scale_range_adaptive_toggle
    
      selected_date <- ifelse(
        is_choropleth_vacc_map, 
        format(as.Date(input$selected_vaccination_map_date), "%Y-%m-%d"), 
        format(as.Date(input$selected_vaccination_hospitalization_plot_date), "%Y-%m-%d"))
      
    # statistic 1
      selected_x_axis <- ifelse(
        is_choropleth_vacc_map, 
        input$selected_map_vaccination_level, 
        input$selected_x_axis_stat)
    
    #statistic 2
      selected_y_axis <- input$selected_y_axis_stat
      
    # Show / Hide
      toggle("graph_dynamic", condition = is_graph_dynamic)
      toggle("graph_static", condition = !is_graph_dynamic)
      toggle("is_scale_range_adaptive_toggle", condition = is_choropleth_vacc_map)
      
      if (is_graph_dynamic) {         #Plot Selected Graph - dynamic
        
          ### Graph Choropleth
          if (is_choropleth_vacc_map)
            graph_interactive_map(
              Graph_Vaccination_Rates_Choropleth_By_Hrr(
                selected_date, 
                display_stat = selected_x_axis,
                is_scale_range_adaptive))
          
          ### Else Graph Point Plot
          else
            graph_interactive_plot(
              Graph_Vaccination_Hospitalization_Plot(
                selected_date, selected_x_axis, selected_y_axis))
      }
      else {
          return(NULL)
      }

  })
  
  output$graph_static <- renderPlot({
    
    is_choropleth_vacc_map = input$tab == "vaccination_choropleth_tab"
    is_graph_dynamic = input$plot_dynamic_toggle
    is_scale_range_adaptive = input$is_scale_range_adaptive_toggle
    
    #date
    selected_date <- ifelse(
      is_choropleth_vacc_map, 
      format(as.Date(input$selected_vaccination_map_date), "%Y-%m-%d"), 
      format(as.Date(input$selected_vaccination_hospitalization_plot_date), "%Y-%m-%d"))
    
    # statistic 1
    selected_x_axis <- ifelse(
      is_choropleth_vacc_map, 
      input$selected_map_vaccination_level, 
      input$selected_x_axis_stat)
    
    #statistic 2
    selected_y_axis <- input$selected_y_axis_stat
    
    # Show / Hide
    toggle("graph_dynamic", condition = is_graph_dynamic)
    toggle("graph_static", condition = !is_graph_dynamic)
    toggle("is_scale_range_adaptive_toggle", condition = is_choropleth_vacc_map)
    
    if (!is_graph_dynamic) {         #Plot Selected Graph - static
      
      ### Graph Choropleth
      if (is_choropleth_vacc_map)
        Graph_Vaccination_Rates_Choropleth_By_Hrr_Static(
            selected_date, 
            display_stat = selected_x_axis,
            is_scale_range_adaptive)
      
      ### Else Graph Point Plot
      else
        Graph_Vaccination_Hospitalization_Plot_Static(
            selected_date, selected_x_axis, selected_y_axis)
    }
    else {
      return(NULL)
    }
  })
  
  
  #### RENDER TITLES
  output$graph_title <- renderText({
    ifelse (input$tab == "vaccination_choropleth_tab",
            "Covid Vaccination Rates in the USA",
            "Covid Vaccination and Hospital Strain")
  })
  
  output$statistic_description <- renderUI({
      text = ""
      if (input$tab == "vaccination_choropleth_tab") {
          stat = possible_axis_labels[input$selected_map_vaccination_level]
          text = paste("HHR population statistic:", stat)
      }
      
      text
  })
  
  #### RENDER GRAPH INFO (bottom)
  output$vacc_data_date <- renderUI({
    valid_data_dates = closest_valid_dates(input$selected_vaccination_map_date)
    em(paste("Vaccination data from:", valid_data_dates[1]))
  })
  output$bed_data_date <- renderUI({
    
    if (input$tab == "vaccination_choropleth_tab") {
        return("")
    }
    
    valid_data_dates = closest_valid_dates(input$selected_vaccination_map_date)
    em(paste("Hospital bed usage data from:", valid_data_dates[2]))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
