# install.packages(c("shiny", "ggplot2"))
# install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(plotly)

source("functions.R")
 
set.seed(070523)
  bed_occupation_time <- data.frame(
    category = 1:6,
    mean_time = c(4.43, 4.46, 6.67, 6.92, 7.74, 9.29)
  )

  stroke_level_prob <- c(0.0570, 0.342, 0.217, 0.136, 0.0971, 0.151)
  
  shiny::addResourcePath("myimages", "images")
  

  # ui <- fluidPage(
  #   theme = shinytheme("darkly"),
  #   navbarPage(title = "Stroke Simulation",
  #              tabPanel("Analysis",
  #                       fluidRow(
  #                         column(
  #                           12,
  #                           h3("Preliminary Analysis"),
  #                           p("For this analysis six Stroke categories were identified with corresponding probabilties, 
  #       and occupation times.
  #       While the patients with the mild-level stroke had the highest probability (0.34 or 34%), 
  #       the patients with the severe level of stroke occupied the bed for the longest duration
  #       of the time."),
  #                           br(),
  #                           
  #                           p("The average arrival rate for the analysis was calculated in the following way: Total number of patients/365. The arrival rate is the average number of patients
  #       arriving into the stroke ward. In this simulation the total number of patients were estimated to be 2000 across the year, hence the arrival was calculated as 2000/365: 5.47 ~ 6 patients/day.
  #       Furthermore, the inter-arrival time of the patients follow an exponential distribution."),
  #                           br(),
  #                           p("In plot below one can observe from plot the stroke categories, corresponding length of stay and probabilities.")
  #                         )
  #                       ),
  #                   
  #                       br(),
  #                       p("Our simulation analysis shows us that for annual patient load of 2000, 
  #   we require 40 beds in the stroke ward to ensure that at any given time there are 
  #   fewer than 5% of the stroke patients waiting to be admitted into the stroke, however,
  #   it is probable that there would be less than 5%
  #   of the patients waiting to be admitted since the simulation model assumes that there has to be
  #   on average 6 stroke patients."),
  #                       # br(),
  #                       # tags$img(
  #                       #   src = "myimages/percent_of_patients_waiting1.png",
  #                       #   width = 1100,
  #                       #   length = 500,
  #                       #   alt = "figure of percent of patients waiting vs the number of beds",
  #                       #   figcap = "figure of percent of patients waiting vs the number of beds"),
  #                       
  #                       br(),
  #                       p("Our analysis derived at these conclusions after running 10,000 replications of the simulation."),
  #                       br(),
  #                       helpText("Please enter an integer value between 500 and 2500 patients"),
  #                       numericInput("num_patients", "Number of Patients:", value = 2500, min = 500),
  #                       helpText("Please enter an integer value between 5 and 50 beds"),
  #                       numericInput("num_beds", "Number of Beds:", value = 25, min = 5),
  #                       actionButton("run_simulation", "Run Simulation"),
  #                       tabsetPanel(
  #                         tabPanel("Patients Waiting",
  #                                  plotOutput("patients_waiting_plot"),
  #                                  textOutput("patients_waiting_text")
  #                         ),
  #                         tabPanel("Utilization",
  #                                  plotOutput("utilization_plot"),
  #                                  textOutput("utilization_text")
  #                         )
  #                         
  #                       )
  #              )
  #   )
  # )
  
  # ui <- fluidPage(
  #   titlePanel("Stroke Simulation App"),
  #   tags$hr(),
  #   tags$div("Brief text goes here..."),
  #   tags$hr(),
  #   fluidRow(
  #     column(3,
  #            wellPanel(
  #              sliderInput("num_patients",
  #                          "Number of Patients (per year):",
  #                          min = 100,
  #                          max = 5000,
  #                          value = 1000,
  #                          step = 100),
  #              sliderInput("num_beds",
  #                          "Number of Beds:",
  #                          min = 5,
  #                          max = 50,
  #                          value = 20,
  #                          step = 1)
  #            )
  #     ),
  #     column(9,
  #            tabsetPanel(
  #              tabPanel("Percent of Patients Waiting",
  #                       plotOutput("patients_waiting_plot"),
  #                       textOutput("patients_waiting_text")),
  #              tabPanel("Bed Utilization",
  #                       plotOutput("utilization_plot"),
  #                       textOutput("utilization_text"))
  #            )
  #     )
  #   ),
  #   fluidRow(
  #     column(12,
  #            plotOutput("static_plot")
  #     )
  #   )
  # )
  
  ui <- fluidPage(
    titlePanel("Stroke Simulation App"),
    
    tags$div(
      tags$p("Stroke is one of the leading causes to disability and death in the Australia. 
      According to the Australian Institute of Health and Welfare, in 2020, stroke was 
      recorded as the underlying cause of 8,200 deaths, accounting for 5.1% of all deaths in 
      Australia. Stroke was one of the 5 leading causes of death in Australia – on average, 
      22 Australians died of stroke each day in 2020. Hence the stroke care units are crucial
      in reducing the death rates and speedying up the recovery for stroke patients. 
      This app simulates the percentage of stroke patients waiting for a bed and 
             bed utilization in a stroke unit at Monash Medical Center based on the number of 
             patients and the number of available beds.
             As per our analysis the optimal number of beds for the annual load of 2000 patients
             should be 40 in the stroke care unit.
             "),
      style = "background-color: #f0f0f0; padding: 10px;"
    ),
    br(),
    
    fluidRow(
      column(3,
             wellPanel(
               sliderInput("num_patients", "Total number of patients:",
                           min = 500, max = 2500, value = 1000, step = 50),
               sliderInput("num_beds", "Number of beds:",
                           min = 5, max = 50, value = 20, step = 1)
             )
      ),
      column(6,
             tabsetPanel(
               tabPanel("Percent of Patients Waiting", plotOutput("patients_waiting_plot")),
               tabPanel("Bed Utilization", plotOutput("utilization_plot"))
             )),
      column(3,
             plotlyOutput("static_plot"))
    ),
    
    fluidRow(
      column(6, textOutput("patients_waiting_text")),
      column(6, textOutput("utilization_text"))
    )
  )
  
  

server <- function(input, output) {
  set.seed(07052023)
  
  waiting_percents_data <- reactive({
    num_beds_range <- seq(5, 50, by = 1)
    waiting_percents <- sapply(num_beds_range, function(num_beds) {
      sim_results <- stroke_simulation(as.numeric(input$num_patients),
                                       as.numeric(input$num_patients) / 365,
                                       bed_occupation_time,
                                       stroke_level_prob,
                                       num_beds)
      sim_results$percent_patients_waiting
    })
    return(waiting_percents)
  })
  
  simulation_results <- reactive({
    set.seed(07052023)
    sim_results <- stroke_simulation(as.numeric(input$num_patients),
                                     as.numeric(input$num_patients) / 365,
                                     bed_occupation_time,
                                     stroke_level_prob,
                                     as.numeric(input$num_beds))
    return(sim_results)
  })
  
  output$patients_waiting_plot <- renderPlot({
    set.seed(07052023)
    num_beds_range <- seq(5, 50, by = 1)
    
    waiting_percents <- waiting_percents_data()
    
    data <- data.frame(
      num_beds = num_beds_range,
      percent_patients_waiting = waiting_percents,
      selected = num_beds_range == input$num_beds
    )
    
    ggplot(data, aes(x = num_beds, y = percent_patients_waiting, fill = selected)) +
      geom_col(width = 0.5) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5), 
                         labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      scale_fill_manual(values = c("steelblue", "red")) +
      labs(title = "Percent of Patients Waiting vs Number of Beds",
           x = "Number of Beds",
           y = "Percent of Patients Waiting") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 12),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.position = "none")
  })
  
  
  # Patients waiting text
  output$patients_waiting_text <- renderText({
    req(simulation_results())
    paste("Percent of patients waiting for a bed at", input$num_patients,"patients in total and" ,input$num_beds, "beds:", round(simulation_results()$percent_patients_waiting, 2), "%")
  })
  
  # Utilization plot
  utilization_percents_data <- reactive({
    num_beds_range <- seq(5, 50, by = 1)
    utilization_percents <- sapply(num_beds_range, function(num_beds) {
      sim_results <- stroke_simulation(as.numeric(input$num_patients),
                                       as.numeric(input$num_patients) / 365,
                                       bed_occupation_time,
                                       stroke_level_prob,
                                       num_beds)
      sim_results$bed_utilization
    })
    utilization_percents
  })
  
  output$utilization_plot <- renderPlot({
    set.seed(07052023)
    req(simulation_results())
    num_beds_range <- seq(5, 50, by = 1)
    
    utilization_percents <- utilization_percents_data()
    
    data <- data.frame(
      num_beds = num_beds_range,
      bed_utilization = utilization_percents,
      selected = num_beds_range == input$num_beds
    )
    
    ggplot(data, aes(x = num_beds, y = bed_utilization, color = selected)) +
      geom_point(size = 4) +
      geom_segment(aes(x = num_beds, xend = num_beds, y = 0, yend = bed_utilization), color = "steelblue") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5), labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      scale_color_manual(values = c("steelblue", "red")) +
      labs(title = "Bed Utilization vs Number of Beds",
           x = "Number of Beds",
           y = "Bed Utilization (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 12),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.position = "none")
  })

  # Utilization text
  output$utilization_text <- renderText({
    utilization_percents <- utilization_percents_data()
    selected_utilization_percent <- utilization_percents[input$num_beds - 4]
    paste("Bed utilization percent for a total of", input$num_patients,"patients and", input$num_beds, "beds:", round(selected_utilization_percent, 2), "%")
  })
  
  set.seed(07052023)
  bed_occupation_time_df <- data.frame(
    stroke_category = c("Very Mild", "Mild", "Mild-Moderate", "Moderate", 
                        "Moderate-Severe", "Severe"),
    bed_occupation_time = c(4.43,4.46,6.67,6.92,7.74,9.29),
    probability = c(0.0570,0.342, 0.217, 0.136, 0.0971, 0.151)
  )
  
  # Static plot
  output$static_plot <- renderPlotly({
    p <- ggplot(bed_occupation_time_df, aes(x = stroke_category, y = bed_occupation_time)) +
      geom_col(fill = "steelblue") +
      theme_minimal()+
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
                position = position_stack(vjust = 0.3), color = "white", size = 2.5) +
      theme_minimal() +
      labs(
        x = "Stroke Category",
        y = "Bed Occupation Time (days)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)
