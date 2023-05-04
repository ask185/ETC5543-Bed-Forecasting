#install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)

library(tidyverse)
library(ggplot2)

set.seed(2023)
# Parameters

total_patients <- 2000 # assuming there are 2000 all around the year
total_patients_future <- 2500 # assuming the 2500 patients in the future 
patients_per_month <- 2000/12 # patients per month
arrival_rate <- total_patients/365  # average 6 patients per day
arrival_rate_future <- total_patients_future/365 # average 7 patients per day

# Bed occupation time (in days) for each stroke level

bed_occupation_time <- list(
  very_mild = 1,
  mild = c(3, 7),
  moderate = c(3,14),  # patients sent to rehab center may stay up to 2 weeks in the ward
  severe_nursing = c(3,21),  # patients sent to nursing center may stay up to 3 weeks
  severe = c(1,21) # die
)

# Probability of each stroke level
stroke_level_prob <- c(0.2, 0.2, 0.35, 0.1, 0.15)

set.seed(2023)


generate_interarrival_times <- function(total_patients, arrival_rate) {
  interarrival_times <- rexp(total_patients, rate = arrival_rate)
  return(interarrival_times)
}


assign_stroke_levels <- function(total_patients, stroke_level_prob) {
  stroke_levels <- sample(1:length(stroke_level_prob), 
                          size = total_patients, replace = TRUE,
                          prob = stroke_level_prob)
  return(stroke_levels)
}


stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
stroke_levels_future <- assign_stroke_levels(total_patients = total_patients_future,
                                             stroke_level_prob = stroke_level_prob)

generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
  length_of_stay <- numeric(total_patients)
  # for every patient in total patients (2000)
  for (i in 1:total_patients) {

    stroke_level <- stroke_levels[i]

    if (stroke_level == 1) {
      length_of_stay[i] <- bed_occupation_time[[stroke_level]]
    } else {

      length_of_stay[i] <- sample(bed_occupation_time[[stroke_level]][1]:bed_occupation_time[[stroke_level]][2], 1)
    }
  }
  
  return(length_of_stay)
}



# Generate length_of_stay for each patient
length_of_stay <- generate_length_of_stay(total_patients, bed_occupation_time, stroke_levels)

# Add number_of_patients and length_of_stay to arrivals_data

interarrival_times <- generate_interarrival_times(total_patients, arrival_rate)

stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)

length_of_stay<- generate_length_of_stay(total_patients, bed_occupation_time, stroke_levels)

arrivals_data <- data.frame(interarrival_times, stroke_levels, length_of_stay)


head(arrivals_data)
# future
interarrival_times_future <- generate_interarrival_times(total_patients = total_patients_future,
                                                         arrival_rate_future)
stroke_levels_future <- assign_stroke_levels(total_patients = total_patients_future,
                                             stroke_level_prob = stroke_level_prob)
length_of_stay_future <- generate_length_of_stay(total_patients_future, bed_occupation_time, stroke_levels_future)

arrivals_data_future <- data.frame(interarrival_times_future,stroke_levels_future,
                                   length_of_stay_future)
set.seed(2023)

stroke_simulation <- function(total_patients, arrival_rate, 
                              bed_occupation_time, stroke_level_prob, 
                              num_beds) {
  
  interarrival_times <- generate_interarrival_times(total_patients, arrival_rate)
  stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
  length_of_stay <- generate_length_of_stay(total_patients, bed_occupation_time, stroke_levels)
  
  # initializing the beds and queue 
  beds <- rep(0, num_beds)
  queue <- list()

  
  update_bed_occupation_times <- function(beds, interarrival_time) {
    beds <- pmax(beds - interarrival_time, 0)

    return(beds)
  }
  
  # for every patient in total patients (1000)
  
  for (i in 1:total_patients) {

    beds <- update_bed_occupation_times(beds, interarrival_times[i])
    available_beds <- which(beds == 0)
    if (length(available_beds) > 0) {
      beds[available_beds[1]] <- length_of_stay[i]
    } else {
      # if there is no bed available then append the queue list
      queue <- append(queue, length_of_stay[i])
    }
  }
  
  return(list(beds = beds, queue = queue, length_of_stay =length_of_stay))
}

# stroke sim current

stroke_sim_50 <- stroke_simulation(total_patients= total_patients, 
                                   arrival_rate= arrival_rate, 
                                   bed_occupation_time = bed_occupation_time, 
                                   stroke_level_prob= stroke_level_prob, 
                                   num_beds= 50)

stroke_sim_40 <- stroke_simulation(total_patients= total_patients, 
                                   arrival_rate= arrival_rate, 
                                   bed_occupation_time = bed_occupation_time, 
                                   stroke_level_prob= stroke_level_prob, 
                                   num_beds= 40)

stroke_sim_30 <- stroke_simulation(total_patients= total_patients, 
                                   arrival_rate= arrival_rate, 
                                   bed_occupation_time = bed_occupation_time, 
                                   stroke_level_prob= stroke_level_prob,
                                   num_beds= 30)

stroke_sim_20 <- stroke_simulation(total_patients= total_patients, 
                                   arrival_rate= arrival_rate, 
                                   bed_occupation_time = bed_occupation_time, 
                                   stroke_level_prob= stroke_level_prob, 
                                   num_beds= 20)

# stroke sim future

stroke_sim_50_f <- stroke_simulation(total_patients= total_patients_future, 
                                     arrival_rate= arrival_rate_future, 
                                     bed_occupation_time = bed_occupation_time, 
                                     stroke_level_prob= stroke_level_prob, 
                                     num_beds= 50)

stroke_sim_40_f <- stroke_simulation(total_patients= total_patients_future, 
                                     arrival_rate= arrival_rate_future, 
                                     bed_occupation_time = bed_occupation_time, 
                                     stroke_level_prob= stroke_level_prob, 
                                     num_beds= 40)

stroke_sim_30_f <- stroke_simulation(total_patients= total_patients_future, 
                                     arrival_rate= arrival_rate_future, 
                                     bed_occupation_time = bed_occupation_time,
                                     stroke_level_prob= stroke_level_prob, 
                                     num_beds= 30)

stroke_sim_20_f <- stroke_simulation(total_patients= total_patients_future, 
                                     arrival_rate= arrival_rate_future, 
                                     bed_occupation_time = bed_occupation_time,
                                     stroke_level_prob= stroke_level_prob, 
                                     num_beds= 20)
set.seed(2023)

run_simulation <- function(arrival_rate, 
                           bed_occupation_time, 
                           stroke_level_prob, 
                           total_patients, 
                           num_simulations) {
  num_bed_range <- seq(25,50, by=1)
  
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
                          num_simulations = 120)

results_future <- run_simulation(arrival_rate = arrival_rate_future, 
                                 bed_occupation_time = bed_occupation_time,
                                 stroke_level_prob = stroke_level_prob, 
                                 total_patients = total_patients_future, 
                                 num_simulations = 120)

# Combine all the data frames into one
results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
results_df_future <- do.call(rbind.lapply(results_future, function(x) do.call(rbind,x)))

# Load the ggplot2 library
library(ggplot2)

# Plot average waiting times against the number of beds
ggplot(results_df, aes(x = num_beds, y = avg_wait_times)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Average Waiting Times vs. Number of Beds",
       x = "Number of Beds",
       y = "Average Waiting Time (Days)")

# Plot the proportion of patients waiting against the number of beds
ggplot(results_df, aes(x = num_beds, y = percent_patients_waiting)) +
  geom_point(size=0.7,
             alpha= 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Percent of Patients Waiting vs. Number of Beds",
       x = "Number of Beds",
       y = "Percent of Patients Waiting") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 0.5)+
  scale_y_continuous(breaks = c(5,10,15,20,25,30,35,40,45))


# Plot utilization against the number of beds
ggplot(results_df, aes(x = num_beds, y = utilization)) +
  geom_point(size=0.7) +
  geom_smooth(method = "loess") +
  theme_minimal()+
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.5)+
  labs(title = "Utilization vs. Number of Beds",
       x = "Number of Beds",
       y = "Utilization")

# Plot max queue length against the number of beds
ggplot(results_df, aes(x = num_beds, y = max_queue_length)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Maximum Queue Length vs. Number of Beds",
       x = "Number of Beds",
       y = "Maximum Queue Length")


ui <- fluidPage(
  titlePanel("Stroke Simulation Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("total_patients", "Total Patients:", min = 500, max = 5000, value = 2000, step = 100),
      sliderInput("total_patients_future", "Total Patients in the Future:", min = 500, max = 5000, value = 2500, step = 100),
      sliderInput("num_simulations", "Number of Simulations:", min = 1, max = 200, value = 120, step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Waiting Times vs. Number of Beds", plotOutput("waiting_times_plot")),
        tabPanel("Percent of Patients Waiting vs. Number of Beds", plotOutput("percent_patients_waiting_plot")),
        tabPanel("Utilization vs. Number of Beds", plotOutput("utilization_plot")),
        tabPanel("Maximum Queue Length vs. Number of Beds", plotOutput("max_queue_length_plot"))
      )
    )
  )
)

server <- function(input, output) {
  output$waiting_times_plot <- renderPlot({
    results <- run_simulation(arrival_rate = arrival_rate, 
                              bed_occupation_time = bed_occupation_time, 
                              stroke_level_prob = stroke_level_prob, 
                              total_patients = input$total_patients, 
                              num_simulations = input$num_simulations)
    
    results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
    
    ggplot(results_df, aes(x = num_beds, y = avg_wait_times)) +
      geom_point() +
      geom_smooth(method = "loess") +
      labs(title = "Average Waiting Times vs. Number of Beds",
           x = "Number of Beds",
           y = "Average Waiting Time (Days)")
  })
  
  output$percent_patients_waiting_plot <- renderPlot({
    results <- run_simulation(arrival_rate = arrival_rate, 
                              bed_occupation_time = bed_occupation_time, 
                              stroke_level_prob = stroke_level_prob, 
                              total_patients = input$total_patients, 
                              num_simulations = input$num_simulations)
    
    results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
    
    ggplot(results_df, aes(x = num_beds, y = percent_patients_waiting)) +
      geom_point(size=0.7,
                 alpha= 0.5) +
      geom_smooth(method = "loess") +
      theme_minimal() +
      labs(title = "Percent of Patients Waiting vs. Number of Beds",
           x = "Number of Beds",
           y = "Percent of Patients Waiting") +
      geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 0.5)+
      scale_y_continuous(breaks = c(5,10,15))
    
  })
  
}
shinyApp(ui = ui, server = server)