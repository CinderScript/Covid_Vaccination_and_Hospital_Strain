library(shiny)
library(shinyjs)


source("covid_study_data_plotter.R")


make_graph <- function(){
  ggplot(us_state_shape_data) +
  geom_sf()
}

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
    
    hosp_plot_date_select = sliderInput( "selected_vaccination_hospitalization_plot_date",
                                         "Select Date::",
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
    is_scale_adaptive = checkboxInput("is_scale_adaptive", "Adaptive Scale Range", F),

    console_messages = textOutput("console_message")

)

##################################
######### SHINY BACKEND ########## 
##################################

server <- function(input, output, session) {
  
    observeEvent(input$nav_vacc_map_tab, {
        print("button clicked")
    })
    observeEvent(input$nav_hosp_plot_tab, {
        print("button clicked")
    })
    observeEvent(input$nav_about_tab, {
        print("button clicked")
    })

  ### Define reactive expressions outside of the renderUI function
  vacc_map_date <- reactive(as.Date(input$vacc_map_date))
  hosp_plot_date <- reactive(as.Date(input$selected_vaccination_hospitalization_plot_date))
  vacc_stat <- reactive(input$vacc_map_stat)
  x_axis_stat <- reactive(input$selected_x_axis_stat)
  y_axis_stat <- reactive(input$selected_y_axis_stat)  
  is_adaptive <- reactive(input$is_scale_adaptive)


  ### RENDER GRAPH
  output$graph <- renderUI({

    #### to make renderUI trigger on changes of these controls
      vacc_map_date()
      hosp_plot_date()
      vacc_stat()
      x_axis_stat()
      y_axis_stat()
      is_adaptive()

      if (input$is_plot_dynamic) {
        plotlyOutput("vacc_graph_dynamic")
      } else {
        plotOutput("vacc_graph_static")
      }
    })

    output$vacc_graph_dynamic <- renderPlotly(
    {
      graph_plotly_vacc_choropleth(                 # need to turn it into a plotly object
        Graph_Vaccination_Rates_Choropleth_By_Hrr(
          date = as.Date(vacc_map_date()), 
          display_stat = vacc_stat(),
          is_scale_adaptive = is_adaptive()))
    })
  
  output$vacc_graph_static <- renderPlot(
    {
      Graph_Vaccination_Rates_Choropleth_By_Hrr_Static(
          date = as.Date(vacc_map_date()), 
          display_stat = vacc_stat(),
          is_scale_adaptive = is_adaptive())
    })

 
  #### RENDER TITLES
  output$graph_title <- renderText({
    "Graph Title - Stuff"
  })
  
  output$graph_subtitle <- renderUI({
     "subtitle - stuff"
  })
  
  #### RENDER GRAPH INFO (bottom)
  output$vacc_data_date <- renderText({
    paste("Date of Vaccination Records: ", input$vacc_map_date)
  })
  output$bed_data_date <- renderText({
    
    "dates"
  })
  
  output$console_message <- renderText({
    
        "example text: Vaccination data succesfully loaded from argvis"
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
