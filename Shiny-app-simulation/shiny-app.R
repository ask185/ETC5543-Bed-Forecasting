#install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(shinythemes)

source("functions.R")
 
set.seed(070523)
  bed_occupation_time <- data.frame(
    category = 1:6,
    mean_time = c(4.43, 4.46, 6.67, 6.92, 7.74, 9.29)
  )

  stroke_level_prob <- c(0.0570, 0.342, 0.217, 0.136, 0.0971, 0.151)
  
  shiny::addResourcePath("myimages", "/Users/seear/Downloads/ETC5543-Forecasting-Project/ETC5543-Bed-Forecasting/Shiny-app-simulation/images")
  

  ui <- fluidPage(
    theme = shinytheme("darkly"),
    navbarPage(title = "Stroke Simulation",
               tabPanel("Analysis",
                        fluidRow(
                          column(
                            12,
                            h3("Preliminary Analysis"),
                            p("For this analysis six Stroke categories were identified with corresponding probabilties, 
        and occupation times.
        While the patients with the mild-level stroke had the highest probability (0.34 or 34%), 
        the patients with the severe level of stroke occupied the bed for the longest duration
        of the time."),
                            br(),
                            
                            p("The average arrival rate for the analysis was calculated in the following way: Total number of patients/365. The arrival rate is the average number of patients
        arriving into the stroke ward. In this simulation the total number of patients were estimated to be 2000 across the year, hence the arrival was calculated as 2000/365: 5.47 ~ 6 patients/day.
        Furthermore, the inter-arrival time of the patients follow an exponential distribution."),
                            br(),
                            p("In plot below one can observe from plot the stroke categories, corresponding length of stay and probabilities.")
                          )
                        ),
                        br(),
                        plotOutput("static_plot"),
                        br(),
                        p("Our simulation analysis shows us that for annual patient load of 2000, 
    we require 40 beds in the stroke ward to ensure that at any given time there are 
    fewer than 5% of the stroke patients waiting to be admitted into the stroke, however,
    it is probable that there would be less than 5%
    of the patients waiting to be admitted since the simulation model assumes that there has to be
    on average 6 stroke patients."),
                        br(),
                        tags$img(
                          src = "myimages/percent_of_patients_waiting1.png",
                          width = 1100,
                          length = 500,
                          alt = "figure of percent of patients waiting vs the number of beds",
                          figcap = "figure of percent of patients waiting vs the number of beds"),
                        
                        br(),
                        p("Our analysis derived at these conclusions after running 10,000 replications of the simulation.")
               ),
               
              tabPanel("Stroke Simulation",
                       sidebarLayout(
                         sidebarPanel( 
                           helpText("Please enter an integer value between 500 and 2500 patients"),
                           numericInput("num_patients", "Number of Patients:", value = 2500, min = 500),
                           helpText( "Please enter an integer value between 5 and 50 beds"),
                           numericInput("num_beds", "Number of Beds:", value = 25, min = 5),
                           actionButton("run_simulation", "Run Simulation")
                           ),
    mainPanel(
      tabsetPanel(
        tabPanel("Patients Waiting",
                 plotOutput("patients_waiting_plot"),
                 textOutput("patients_waiting_text")
        ),
        tabPanel("Utilization",
                 plotOutput("utilization_plot"),
                 textOutput("utilization_text")
        )
      )
    )
                       )
              )
    )
  )

server <- function(input, output) {
  set.seed(07052023)
  bed_occupation_time_df <- data.frame(
    stroke_category = c("Very Mild", "Mild", "Mild-Moderate", "Moderate", 
                        "Moderate-Severe", "Severe"),
    bed_occupation_time = c(4.43,4.46,6.67,6.92,7.74,9.29),
    probability = c(0.0570,0.342, 0.217, 0.136, 0.0971, 0.151)
  )
  output$static_plot <- renderPlot({
    ggplot(bed_occupation_time_df, aes(x = stroke_category, y = bed_occupation_time)) +
      geom_col(fill = "steelblue", width = 0.5) +
      theme_minimal()+
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
                position = position_stack(vjust = 0.8), color = "white", size = 6) +
      theme_minimal() +
      labs(title = "Bed Occupation Time vs. Stroke Category",
           x = "Stroke Category",
           y = "Bed Occupation Time (days)")
  })
  
  # output$my_simulation_results_plot <- renderPlot({
  #   ggplot(results_df, aes(x = num_beds, y = percent_patients_waiting)) +
  #     geom_point(size=1,
  #                alpha= 0.7) +
  #     geom_smooth(method = "loess") +
  #     theme_minimal() +
  #     geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
  #               position = position_stack(vjust = 0.8), color = "white", size = 6)+
  #     labs(title = "Percent of Patients Waiting vs. Number of Beds",
  #          x = "Number of Beds",
  #          y = "Percent of Patients Waiting") +
  #     geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 0.5)+
  #     scale_y_continuous(breaks = c(5,10,15,20,25,30,35,40,45))
  # })
  
  
  # Run simulation when the button is clicked
  simulation_results <- eventReactive(input$run_simulation, {
    set.seed(070523)
    stroke_simulation(as.numeric(input$num_patients),
                      as.numeric(input$num_patients) / 365,
                      bed_occupation_time,
                      stroke_level_prob,
                      as.numeric(input$num_beds))
  })
  
  # Patients waiting plot
  # output$patients_waiting_plot <- renderPlot({
  #   req(simulation_results())
  #   data <- data.frame(
  #     num_beds = input$num_beds,
  #     percent_patients_waiting = simulation_results()$percent_patients_waiting
  #   )
  #   ggplot(data, aes(x = num_beds, y = percent_patients_waiting)) +
  #     geom_point(size=1,
  #                alpha=0.8) +
  #     scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  #     scale_x_continuous(breaks = seq(5, 50, by = 1)) +
  #     labs(title = "Percent of Patients Waiting vs Number of Beds",
  #          x = "Number of Beds",
  #          y = "Percent of Patients Waiting") +
  #     theme_minimal()
  # })
  output$patients_waiting_plot <- renderPlot({
    req(simulation_results())
    data <- data.frame(
      num_beds = input$num_beds,
      percent_patients_waiting = simulation_results()$percent_patients_waiting
    )
    ggplot(data, aes(x = num_beds, y = percent_patients_waiting)) +
      geom_point(size=2.5,
                 alpha=2.5) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      labs(title = "Percent of Patients Waiting vs Number of Beds",
           x = "Number of Beds",
           y = "Percent of Patients Waiting") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 20), # Increase the size of x axis text
            axis.text.y = element_text(size = 20), # Increase the size of y axis text
            plot.title = element_text(size = 24), # Increase the size of plot title
            axis.title.x = element_text(size = 22), # Increase the size of x axis title
            axis.title.y = element_text(size = 22)) # Increase the size of y axis title
  })
  
  
  # Patients waiting text
  output$patients_waiting_text <- renderText({
    req(simulation_results())
    paste("Percent of patients waiting for a bed at", input$num_patients,"patients in total and" ,input$num_beds, "beds:", round(simulation_results()$percent_patients_waiting, 2), "%")
  })
  
  # Utilization plot
  output$utilization_plot <- renderPlot({
    req(simulation_results())
    data <- data.frame(
      num_beds = input$num_beds,
      bed_utilization = simulation_results()$bed_utilization
    )
    ggplot(data, aes(x = num_beds, y = bed_utilization)) +
      geom_point(size=2.5,
                 alpha=2.5) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      labs(title = "Bed Utilization vs Number of Beds",
           x = "Number of Beds",
           y = "Bed Utilization (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 20), # Increase the size of x axis text
            axis.text.y = element_text(size = 20), # Increase the size of y axis text
            plot.title = element_text(size = 24), # Increase the size of plot title
            axis.title.x = element_text(size = 22), # Increase the size of x axis title
            axis.title.y = element_text(size = 22)) # Increase the size of y axis title) 
    
  })
  
  # Utilization text
  output$utilization_text <- renderText({
    req(simulation_results())
    paste("Bed utilization percent for a total of",input$num_patients,"patients and", input$num_beds, "beds:", round(simulation_results()$bed_utilization, 2), "%")
  })
  
}

shinyApp(ui = ui, server = server)
