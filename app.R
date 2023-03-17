library(shiny)
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

possible_color_choices <- c(
  "Washington Post"   = "wp_palette",
  "Red Blue Palette"   = "rb_palette",
  "Soft Purple Green"   = "spg_palette")


#############################
######### SHINY UI ##########
#############################

ui <- htmlTemplate( 
  "app_ui.html",
  
  vacc_map_stat_select = selectInput( "vacc_map_stat",
                                      "Vaccination Level:",
                                      choices = vaccination_stat_choices, 
                                      selected = "vacc_complete_percent"),
  
  vacc_map_date_select = sliderInput( "vacc_map_date",
                                      "Select Date:",
                                      min = as.Date("2020-12-13", "%Y-%m-%d"),
                                      max = as.Date("2023-03-03", "%Y-%m-%d"),
                                      value = as.Date("2021/09/24"),
                                      timeFormat="%Y-%m-%d"),
  
  hosp_plot_x_select = selectInput( "selected_x_axis_stat",
                                    "Show On X-Axis:",
                                    choices = possible_axis_choices, 
                                    selected = "vacc_complete_percent"),
  
  hosp_plot_y_select = selectInput( "selected_y_axis_stat",
                                    "Show On Y-Axis:",
                                    choices = possible_axis_choices, 
                                    selected = "covid_bed_usage_ratio"),
  
  hosp_plot_date_select = sliderInput( "hosp_vacc_plot_date",
                                       "Select Date:",
                                       min = as.Date("2020-12-13", "%Y-%m-%d"),
                                       max = as.Date("2023-03-03", "%Y-%m-%d"),
                                       value = as.Date("2021/09/24"),
                                       timeFormat="%Y-%m-%d"),

  color_palette_select = selectInput( "selected_color_palette",
                                    "Color Palette:",
                                    choices = possible_color_choices, 
                                    selected = "wp_palette"),
  
  wp_map_date_select = sliderInput( "wp_map_date",
                                    "Select Date:",
                                    min = as.Date("2020-12-13", "%Y-%m-%d"),
                                    max = as.Date("2023-03-03", "%Y-%m-%d"),
                                    value = as.Date("2021/09/24"),
                                    timeFormat="%Y-%m-%d"),
  
  graph = uiOutput("graph"),
  
  graph_title = textOutput("graph_title"),
  graph_subtitle = htmlOutput("graph_subtitle"),
  
  vaccination_data_date = textOutput("vacc_data_date"),
  hospitalization_data_date = textOutput("bed_data_date"),
  
  is_plot_dynamic = checkboxInput("is_plot_dynamic", "Interactive Graph", F),
  is_scale_adaptive = checkboxInput("is_scale_adaptive", "Adaptive Scale", F),
  graph_size_slider = numericInput("num", 
                        h3("Numeric input"), 
                        value = 1)  

)

##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output, session) {
  
  ### Define reactive expressions to be used in several functions
  ### For a function to trigger, a reactive value must be inside that function
  vacc_map_date <- reactive(as.Date(input$vacc_map_date))
  hosp_plot_date <- reactive(as.Date(input$hosp_vacc_plot_date))
  wp_map_date <- reactive(as.Date(input$wp_map_date))
  
  vacc_stat <- reactive(input$vacc_map_stat)
  x_axis_stat <- reactive(input$selected_x_axis_stat)
  y_axis_stat <- reactive(input$selected_y_axis_stat)  
  
  wp_color_palette <- reactive(input$selected_color_palette)

  is_adaptive <- reactive(input$is_scale_adaptive)
  active_tab <- reactive(input$active_tab)

  graph_size <- reactive(input$size_slider_value)


  ### RENDER GRAPH
  output$graph <- renderUI({

    #### to make renderUI trigger on changes of these controls
    vacc_map_date()
    hosp_plot_date()
    wp_map_date()
    wp_color_palette()
    vacc_stat()
    x_axis_stat()
    y_axis_stat()
    is_adaptive()

    print(active_tab())
    
    if (input$is_plot_dynamic) {
      plotlyOutput("vacc_graph_dynamic", height = graph_size(), width = "100%") %>% withSpinner()
    } else {
      plotOutput("vacc_graph_static", height = graph_size(), width = "100%") %>% withSpinner()
    }
  })
  
  output$vacc_graph_dynamic <- renderPlotly(
    {
      req(active_tab())
      
      if (active_tab() == "nav_vacc_map_tab"){
        graph_plotly_vacc_choropleth(                 # need to turn it into a plotly object
          Graph_Vaccination_Rates_Choropleth_By_Hrr(
            date = as.Date(vacc_map_date()), 
            display_stat = vacc_stat(),
            is_scale_adaptive = is_adaptive()))
      }
      else if(input$active_tab == "nav_hosp_plot_tab"){
        graph_plotly_point_plot(                 # need to turn it into a plotly object
          Graph_Vaccination_Hospitalization_Plot(
            date = as.Date(hosp_plot_date()), 
            x_axis = x_axis_stat(),
            y_axis = y_axis_stat()))
      }
    })
  
  output$vacc_graph_static <- renderPlot(
    {
      req(active_tab())
      
      if (active_tab() == "nav_vacc_map_tab"){
        Graph_Vaccination_Rates_Choropleth_By_Hrr_Static(
          date = as.Date(vacc_map_date()), 
          display_stat = vacc_stat(),
          is_scale_adaptive = is_adaptive())
      }
      else if(active_tab() == "nav_hosp_plot_tab"){
        Graph_Vaccination_Hospitalization_Plot_Static(
          date = as.Date(hosp_plot_date()), 
          x_axis = x_axis_stat(),
          y_axis = y_axis_stat())
      }
      else if(active_tab() == "nav_wp_map_tab") {
        bivariate_palette = NULL

        if( wp_color_palette() == "wp_palette")
            bivariate_palette = generate_bivariate_palette(get_wp_palette())
        else if(wp_color_palette() == "rb_palette")
            bivariate_palette = generate_bivariate_palette(get_rb_palette())
        else if(wp_color_palette() == "spg_palette")
            bivariate_palette = generate_bivariate_palette(get_soft_pg_palette())

        Graph_Bivariate_Covid_Map(
          date = as.Date(wp_map_date()), 
          bivariate_palette)
      }
    })
  
  
  #### RENDER TITLES
  output$graph_title <- renderText({
    ifelse (active_tab() == "nav_vacc_map_tab",
            "Covid Vaccination Rates in the USA",
            "Covid Vaccination and Hospital Strain")
  })
  
  output$graph_subtitle <- renderText({
    req(active_tab())
    req(vacc_stat())
    if (active_tab() == "nav_vacc_map_tab") {
      stat = possible_axis_labels[vacc_stat()]
      text = paste("Population Vaccination Level:", stat)
    }
  })
  
  #### RENDER DATE OF DATASET (bottom)
  output$vacc_data_date <- renderText({
    req(active_tab())
    date_text = "Date of Vaccination Records: "
    
    if(active_tab() == "nav_vacc_map_tab")
      date_text = paste(date_text, closest_valid_dates(vacc_map_date())[1])
    else
      date_text = paste(date_text, closest_valid_dates(hosp_plot_date())[1])
  })
  output$bed_data_date <- renderText({
    req(active_tab())
    if(active_tab() == "nav_hosp_plot_tab")
      paste(
        "Date of Hospital Records: ",
        closest_valid_dates(hosp_plot_date())[2])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
