library(shiny)
library(shinyjs)
library(shinycssloaders)


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
    titlePanel("COVID Vaccination Statistics"),

    # Create separate tabs and inputs for pitching and batting 
    tabsetPanel(id = "tab", 
                
                h4("Select Statistic and Date to Visualize"),
                
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
                ),
                
                tabPanel(title = "Vaccination Levels Choropleth", value = "vaccination_choropleth_tab",
                         sidebarPanel(selectInput("selected_map_vaccination_level",
                                                  "Vaccination Level:",
                                                  choices = vaccination_stat_choices, 
                                                  selected = "single_dose_percent")),
                         
                         sidebarPanel(sliderInput("selected_vaccination_map_date",
                                                  "Selected vaccination status date:",
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
          plotlyOutput("graph_dynamic") %>% withSpinner(),
          plotOutput("graph_static"),
          checkboxInput("plot_dynamic_toggle", "Make Graph Interactive", T),
          h4(htmlOutput("vacc_data_date")),
          h4(htmlOutput("bed_data_date"))
        )
    )




IS_GRAPH_DYNAMIC <<- F


##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output, session) {

  ##### RENDER GRAPHS
  output$graph_dynamic <- renderPlotly({
    #date
      selected_date <- ifelse(
        input$tab == "vaccination_choropleth_tab", 
        format(as.Date(input$selected_vaccination_map_date), "%Y-%m-%d"), 
        format(as.Date(input$selected_vaccination_hospitalization_plot_date), "%Y-%m-%d"))
      
    # statistic 1
      selected_x_axis <- ifelse(
        input$tab == "vaccination_choropleth_tab", 
        input$selected_map_vaccination_level, 
        input$selected_x_axis_stat)
    
    #statistic 2
      selected_y_axis <- input$selected_y_axis_stat
      
    
      is_graph_dynamic = input$plot_dynamic_toggle
      toggle("graph_dynamic", condition = input$plot_dynamic_toggle) 
      toggle("graph_static", condition = !input$plot_dynamic_toggle)
      
      if (is_graph_dynamic) {         #Plot Selected Graph - dynamic
        
          ### Graph Choropleth
          if (input$tab == "vaccination_choropleth_tab")
            graph_interactive_map(
              Graph_Vaccination_Rates_Choropleth_By_Hrr(
                selected_date, display_stat = selected_x_axis))
          
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
    #date
    selected_date <- ifelse(
      input$tab == "vaccination_choropleth_tab", 
      format(as.Date(input$selected_vaccination_map_date), "%Y-%m-%d"), 
      format(as.Date(input$selected_vaccination_hospitalization_plot_date), "%Y-%m-%d"))
    
    # statistic 1
    selected_x_axis <- ifelse(
      input$tab == "vaccination_choropleth_tab", 
      input$selected_map_vaccination_level, 
      input$selected_x_axis_stat)
    
    #statistic 2
    selected_y_axis <- input$selected_y_axis_stat
    
    
    is_graph_dynamic = input$plot_dynamic_toggle
    
    toggle("graph_dynamic", condition = input$plot_dynamic_toggle) 
    toggle("graph_static", condition = !input$plot_dynamic_toggle)
    
    if (!is_graph_dynamic) {         #Plot Selected Graph - static
      
      ### Graph Choropleth
      if (input$tab == "vaccination_choropleth_tab")
          Graph_Vaccination_Rates_Choropleth_By_Hrr(
            selected_date, display_stat = selected_x_axis)
      
      ### Else Graph Point Plot
      else
          Graph_Vaccination_Hospitalization_Plot(
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
    valid_data_dates = closest_valid_dates(input$selected_vaccination_map_date)
    em(paste("Hospital bed usage data from:", valid_data_dates[2]))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
