#install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

source("functions.R")
 
set.seed(070523)
  bed_occupation_time <- data.frame(
    category = 1:6,
    mean_time = c(4.43, 4.46, 6.67, 6.92, 7.74, 9.29)
  )

  stroke_level_prob <- c(0.0570, 0.342, 0.217, 0.136, 0.0971, 0.151)


  ui <- fluidPage(
    navbarPage(title = "Stroke Simulation",
               tabPanel("Abstract",
                        fluidRow(
                          column(12,
                                 h3("Background"),
                                 p("Geographical stroke units offer effective treatment for 
                                   ischemic and hemorrhagic stroke patients. 
                                   Optimal care is achieved when patients are 
                                   allocated to these wards. Determining the optimal number of 
                                   beds required is a complex task. Our study leverages discrete-event 
                                   simulation, grounded in queueing theory, to ascertain the ideal 
                                   number of beds for optimal care given patient influx."),
                                 h3("Method"),
                                 p("Our model incorporated patient arrival rates, stroke severity, 
                                 and length of stay using historical data. The distribution was of 
                                 stroke severity were: very mild (frequency=0.2), mild 
                                 (frequency=0.2, moderate (0.35), severe (0.1), and very severe (0.15). 
                                 Bed occupation times ranged from 1 day (very mild) to 3-21 days (severe). 
                                 We estimated an approximate arrival rate of 6 patients/day (2000 /365 days) 
                                 following exponential distribution. We explored 20-50 bed scenarios and considered 
                                 an increase in patient arrival to 2500 in the future. Simulation 
                                 ran for 10,000 replications, evaluating performance measures 
                                 (e.g., average queue length, wait times, maximum queue length,
                                 and capacity utilization) with an initial 25-bed provision."),
                                 h3("Results"),
                                 p("The current 25-bed provision was inadequate for the annual
                                 patient load of 2000, leading to prolonged wait times and queues. 
                                 The proportion of patients queuing was around 33% for 25 beds, 
                                 45% for 20 beds, 23% for 30 beds, and 7% when 40 beds were available."),
                                 h3("Conclusions"),
                                 p("Our study emphasizes the collaborative role of the modeller and clinician 
                                   in determining the optimal size of a geographical stroke unit. We are developing 
                                   a web-based application to aid clinicians in estimating stroke unit size,
                                   contributing to more effective patient care.")
                          )
                        )
               ),
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
                            p("Below one can observe from plot the stroke categories, corresponding length of stay and probabilities.")
                            ),
                          plotOutput("static_plot")
                          )
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
      geom_col(fill = "steelblue") +
      theme_minimal()+
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
                position = position_stack(vjust = 0.5), color = "white", size = 4) +
      theme_minimal() +
      labs(title = "Bed Occupation Time vs. Stroke Category",
           x = "Stroke Category",
           y = "Bed Occupation Time (days)")
  })
  
  
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
  output$patients_waiting_plot <- renderPlot({
    req(simulation_results())
    data <- data.frame(
      num_beds = input$num_beds,
      percent_patients_waiting = simulation_results()$percent_patients_waiting
    )
    ggplot(data, aes(x = num_beds, y = percent_patients_waiting)) +
      geom_point() +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      labs(title = "Percent of Patients Waiting vs Number of Beds",
           x = "Number of Beds",
           y = "Percent of Patients Waiting") +
      theme_minimal()
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
      geom_point() +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      scale_x_continuous(breaks = seq(5, 50, by = 1)) +
      labs(title = "Bed Utilization vs Number of Beds",
           x = "Number of Beds",
           y = "Bed Utilization (%)") +
      theme_minimal()
    
  })
  
  # Utilization text
  output$utilization_text <- renderText({
    req(simulation_results())
    paste("Bed utilization percent for a total of",input$num_patients,"patients and", input$num_beds, "beds:", round(simulation_results()$bed_utilization, 2), "%")
  })
  
}

shinyApp(ui = ui, server = server)
