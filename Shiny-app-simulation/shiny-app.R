#install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyverse)


set.seed(2023)
# Parameters

total_patients <- 2000 
total_patients_future <- 2500 
patients_per_month <- 2000/12
arrival_rate <- total_patients/365  
arrival_rate_future <- total_patients_future/365 



bed_occupation_time <- list(c(2.4), c(3.5), c(5.6), c(8.7), c(8), c(10.2))

stroke_level_prob <- c(0.0850,0.420, 0.208, 0.0781, 0.0820, 0.120)

generate_interarrival_times <- function(total_patients, arrival_rate) {
  interarrival_times <- rexp(total_patients, rate = arrival_rate)
  return(interarrival_times)
}


assign_stroke_levels <- function(total_patients, stroke_level_prob) {
  stroke_levels <- sample(1:length(stroke_level_prob), 
                          size = total_patients, 
                          replace = TRUE, 
                          prob = stroke_level_prob)
  return(stroke_levels)
}
stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)

stroke_levels_future <- assign_stroke_levels(total_patients = total_patients_future,
                                             stroke_level_prob = stroke_level_prob)

generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
  length_of_stay <- numeric(total_patients)
  
  for (i in 1:total_patients) {
    stroke_level <- stroke_levels[i]
    
    if (length(bed_occupation_time[[stroke_level]]) == 1) {
      length_of_stay[i] <- bed_occupation_time[[stroke_level]][1]
    } else {
      length_of_stay[i] <- sample(bed_occupation_time[[stroke_level]][1]:bed_occupation_time[[stroke_level]][2], 1)
    }
  }
  
  return(length_of_stay)
}


# Generating length_of_stay for each patient
length_of_stay <- generate_length_of_stay(total_patients, 
                                          bed_occupation_time, 
                                          stroke_levels)


stroke_simulation <- function(total_patients, arrival_rate, 
                              bed_occupation_time, stroke_level_prob, 
                              num_beds) {
  
  interarrival_times <- generate_interarrival_times(total_patients, arrival_rate)
  stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
  length_of_stay <- generate_length_of_stay(total_patients, 
                                            bed_occupation_time, 
                                            stroke_levels)
  

  beds <- rep(0, num_beds)
  queue <- list()
  
  update_bed_occupation_times <- function(beds, interarrival_time) {
    beds <- pmax(beds - interarrival_time, 0)
    return(beds)
  }
  
  for (i in 1:total_patients) {
    
    beds <- update_bed_occupation_times(beds, 
                                        interarrival_times[i])
    
    available_beds <- which(beds == 0)
    
    if (length(available_beds) > 0) {
      beds[available_beds[1]] <- length_of_stay[i]
    } else {
      queue <- append(queue, length_of_stay[i])
    }
  }
  
  return(list(beds = beds, queue = queue, length_of_stay =length_of_stay))
}

run_simulation <- function(arrival_rate, 
                           bed_occupation_time, 
                           stroke_level_prob, 
                           total_patients, 
                           num_simulations,
                           num_beds) {
  num_bed_range <- seq(20,50, by=1)
  
  performance_metrics_list <- lapply(1:num_simulations, function(simulation) {
    lapply(num_bed_range, function(num_beds) {
      stroke_sim <- stroke_simulation(total_patients =  total_patients,
                                      arrival_rate = arrival_rate,
                                      bed_occupation_time = bed_occupation_time,
                                      stroke_level_prob = stroke_level_prob,
                                      num_beds = num_beds)
      
      num_beds <- length(stroke_sim$beds)
      total_patients <- total_patients
      
      occupied_beds <- sum(stroke_sim$beds > 0)
      utilization <- occupied_beds / num_beds
      
      total_wait_times <- sum(unlist(stroke_sim$queue))
      num_patients_queue <- length(stroke_sim$queue)
      avg_wait_times <- total_wait_times / num_patients_queue
      
      average_time_in_queue = total_wait_times / num_patients_queue
      max_queue_length <- length(stroke_sim$queue)
      
      proportion_patients_waiting <- num_patients_queue / total_patients
      percent_patients_waiting <- proportion_patients_waiting * 100
      
      avg_length_of_stay <- (sum(stroke_sim$length_of_stay)) / total_patients
      
      performance_metrics_month <- data.frame(num_beds = num_beds,
                                              utilization = utilization,
                                              avg_wait_times = avg_wait_times,
                                              max_queue_length = max_queue_length,
                                              proportion_patients_waiting = proportion_patients_waiting,
                                              percent_patients_waiting = percent_patients_waiting,
                                              avg_length_of_stay = avg_length_of_stay)
      
      return(performance_metrics_month)
    })
  })
  
  return(performance_metrics_list)
}

results <- run_simulation(arrival_rate = arrival_rate,
                          bed_occupation_time = bed_occupation_time,
                          stroke_level_prob = stroke_level_prob,
                          total_patients = total_patients,
                          num_simulations = 100,
                          num_beds = num_beds)
# 
# results_future <- run_simulation(arrival_rate = arrival_rate_future, 
#                                  bed_occupation_time = bed_occupation_time,
#                                  stroke_level_prob = stroke_level_prob, 
#                                  total_patients = total_patients_future,
#                                  num_simulations = 120,
#                                  num_beds = num_beds)
# 
# 
 results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))


library(shiny)
library(ggplot2)
library(shinydashboard)

 
                    
  ui <- dashboardPage(
    dashboardHeader(title = "Stroke Ward Bed Optimization"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Abstract", tabName = "abstract", icon = icon("file-text")),
        menuItem("Preliminary Analysis", tabName = "prelim", icon = icon("dashboard")),
        menuItem("Simulation", tabName = "simulation", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "abstract",
                fluidRow(
                  box(title = "Background", width = 12, solidHeader = TRUE, status = "primary",
                      "Geographical stroke units offer effective treatment for ischemic and hemorrhagic stroke patients. Optimal care is achieved when patients are allocated to these wards. Determining the optimal number of beds required is a complex task. Our study leverages discrete-event simulation, grounded in queueing theory, to ascertain the ideal number of beds for optimal care given patient influx."
                  ),
                  box(title = "Method", width = 12, solidHeader = TRUE, status = "primary",
                      "Our model incorporated patient arrival rates, stroke severity, and length of stay using historical data. The distribution was of stroke severity were: very mild (frequency=0.2), mild (frequency=0.2, moderate (0.35), severe (0.1), and very severe (0.15). Bed occupation times ranged from 1 day (very mild) to 3-21 days (severe). We estimated an approximate arrival rate of 6 patients/day (2000 /365 days) following exponential distribution. We explored 20-50 bed scenarios and considered an increase in patient arrival to 2500 in the future. Simulation ran for 10,000 replications, evaluating performance measures (e.g., average queue length, wait times, maximum queue length, and capacity utilization) with an initial 25-bed provision."
                  ),
                  box(title = "Results", width = 12, solidHeader = TRUE, status = "primary",
                      "The current 25-bed provision was inadequate for the annual patient load of 2000, leading to prolonged wait times and queues. The proportion of patients queuing was around 33% for 25 beds, 45% for 20 beds, 23% for 30 beds, and 7% when 40 beds were available."
                  ),
                  box(title = "Conclusions", width = 12, solidHeader = TRUE, status = "primary",
                      "Our study emphasizes the collaborative role of the modeller and clinician in determining the optimal size of a geographical stroke unit. We are developing a web-based application to aid clinicians in estimating stroke unit size, contributing to more effective patient care."
                  )
                )
        ),
        tabItem(tabName = "prelim",
                fluidRow(
                  box(title = "Preliminary Analysis", width = 12, solidHeader = TRUE, status = "primary",
                      "Parameters used in modeling were: Stroke categories (identified to be six), stroke categories probabilties, and occupation times of the corresponding stroke levels/categories.
                      While the patients with the mild-level stroke had the highest probability (0.42 or 42%), the patients with the severe level of stroke occupied the bed for the longest duration of the time.
                      In the plot below, one can see the bed occupation times for each category of stroke and their corresponding probabilties.",
                      plotOutput("prelim_plot")
                      )
                )
        ),
        tabItem(tabName = "simulation",
                fluidRow(
                  box(title = "Parameters", width = 3, solidHeader = TRUE, status = "primary",
                      sliderInput("num_patients", "Number of Patients:", min = 500, max = 2500, value = 2000, step = 100),
                      selectInput("num_beds", "Number of Beds:", choices = seq(5, 50, by = 1), selected = 30)
                  ),
                  box(title = "Results", width = 9, solidHeader = TRUE, status = "primary",
                      textOutput("percent_patients_waiting"),
                      textOutput("utilization")
                  )
                )
        )
      )
    )
  )
 

# server <- function(input, output) {
#   
#   simulation_results <- reactive({
#     run_simulation(arrival_rate = arrival_rate, 
#                    bed_occupation_time = bed_occupation_time, 
#                    stroke_level_prob = stroke_level_prob, 
#                    total_patients = input$total_patients, 
#                    num_simulations = 120,
#                    num_beds = input$num_beds)
#   })
#   
#   output$percent_patients_waiting_plot <- renderPlot({
#     results_df <- do.call(rbind, lapply(simulation_results(), function(x) x))
#     results_df <- data.frame(results_df)
#     current_simulation <- results_df[results_df$num_beds == input$num_beds, ]
#     ggplot(results_df, aes(x = num_beds, y = percent_patients_waiting)) +
#       geom_point(size=0.7,
#                  alpha= 0.5) +
#       geom_smooth(method = "loess") +
#       theme_minimal() +
#       labs(title = "Percent of Patients Waiting vs. Number of Beds",
#            x = "Number of Beds",
#            y = "Percent of Patients Waiting") +
#       geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 0.5)+
#       scale_y_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50,
#                                     55,60,65,70,75,80,85,90)) +
#       
#       # Add the point for the current input values
#       geom_point(data = current_simulation, aes(x = num_beds, y = percent_patients_waiting), 
#                  color = "gray", size = 3, shape = 18)
#   })
#   
#   output$utilization_plot <- renderPlot({
#     results_df <- do.call(rbind, lapply(simulation_results(), function(x) x))
#     results_df <- data.frame(results_df) 
#     ggplot(results_df, aes(x = input$num_beds, y = utilization)) +
#       geom_point(size=0.7,
#                  alpha= 0.5) +
#       geom_smooth(method = "loess") +
#       theme_minimal() +
#       labs(title = "Percent of Patients Waiting vs. Number of Beds",
#            x = "Number of Beds",
#            y = "Utilization") +
#       geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.5)                                                             
#   })
#   
# }
# shinyApp(ui = ui, server = server)

server <- function(input, output) {
  
  output$prelim_plot <- renderPlot({
    bed_occupation_time_df <- data.frame(
      stroke_category = c("Very Mild", "Mild", "Mild-Moderate", "Moderate", "Moderate-Severe", "Severe"),
      bed_occupation_time = c(2.4, 3.5, 5.6, 8.7, 8, 10.2),
      probability = c(0.0850, 0.420, 0.208, 0.0781, 0.0820, 0.120)
    )
    
    ggplot(bed_occupation_time_df, aes(x = stroke_category, y = bed_occupation_time)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
                position = position_stack(vjust = 0.5), color = "white", size = 4) +
      theme_minimal() +
      labs(title = "Bed Occupation Time vs. Stroke Category",
           x = "Stroke Category",
           y = "Bed Occupation Time (days)")
  })
  
  
  # simulation_results <- reactive({
  #   total_patients <- input$num_patients
  #   num_beds <- input$num_beds
  #   
  #   run_simulation(total_patients = total_patients, 
  #                  arrival_rate = arrival_rate, 
  #                  bed_occupation_time = bed_occupation_time, 
  #                  stroke_level_prob = stroke_level_prob, 
  #                  num_simulations = 100,
  #                  num_beds = num_beds)
  #   
  # })
  # 
  # output$percent_patients_waiting <- renderText({
  #   percent_patients_waiting <- simulation_results()[[as.character(input$num_beds)]]
  #   paste0("Percentage of Patients Waiting: ", round(percent_patients_waiting, 2), "%")
  # })
  # 
  # output$max_queue_length <- renderText({
  #   sim <- stroke_simulation(total_patients = input$num_patients, 
  #                            arrival_rate = arrival_rate, 
  #                            bed_occupation_time = bed_occupation_time, 
  #                            stroke_level_prob = stroke_level_prob, 
  #                            num_beds = input$num_beds)
  #   
  #   max_queue_length <- length(sim$queue)
  #   paste0("Max Queue Length: ", max_queue_length)
  # })
  # 
  # output$avg_wait_times <- renderText({
  #   sim <- stroke_simulation(total_patients = input$num_patients, 
  #                            arrival_rate = arrival_rate, 
  #                            bed_occupation_time = bed_occupation_time, 
  #                            stroke_level_prob = stroke_level_prob, 
  #                            num_beds = input$num_beds)
  #   
  #   avg_wait_times <- sum(sim$queue) / length(sim$queue)
  #   paste0("Average Wait Time: ", round(avg_wait_times / 24, 2), " days")
  # })
  # 
  # output$results_plot <- renderPlot({
  #   num_beds_range <- seq(5, 50, by = 1)
  #   simulation_results_all <- run_simulation(total_patients = input$num_patients, 
  #                                            arrival_rate = arrival_rate, 
  #                                            bed_occupation_time = bed_occupation_time, 
  #                                            stroke_level_prob = stroke_level_prob, 
  #                                            num_beds = input$num_beds)
  #   
  #   results_df <- data.frame(num_beds = as.integer(names(simulation_results_all)), 
  #                            percent_patients_waiting = unlist(simulation_results_all))
  #   
  #   ggplot(results_df, aes(x = num_beds, y = percent_patients_waiting)) +
  #     geom_line() +
  #     geom_point(color = "blue") +
  #     geom_point(data = results_df[results_df$num_beds == input$num_beds, ],
  #                aes(x = num_beds, y = percent_patients_waiting), 
  #                color = "red", size = 3, shape = 18) +
  #     theme_minimal() +
  #     labs(title = "Percent of Patients Waiting vs. Number of Beds",
  #          x = "Number of Beds",
  #          y = "Percent of Patients Waiting") +
  #     scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))
  # })
}

shinyApp(ui = ui, server = server)
