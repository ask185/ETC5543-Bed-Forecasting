library(shiny)
library(shinydashboard)



ui <- dashboardPage(
  dashboardHeader(title = "Stroke Ward Simulation"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Abstract", tabName = "abstract", icon = icon("info-circle")),
      menuItem("Simulation", tabName = "simulation", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Abstract tab
      tabItem(tabName = "abstract",
              h2("Abstract"),
              h3("Background"),
              p("Geographical stroke units offer effective treatment for ischemic and hemorrhagic stroke patients. Optimal care is achieved when patients are allocated to these wards. Determining the optimal number of beds required is a complex task. Our study leverages discrete-event simulation, grounded in queueing theory, to ascertain the ideal number of beds for optimal care given patient influx."),
              h3("Method"),
              p("Our model incorporated patient arrival rates, stroke severity, and length of stay using historical data. The distribution was of stroke severity were: very mild (frequency=0.2), mild (frequency=0.2, moderate (0.35), severe (0.1), and very severe (0.15). Bed occupation times ranged from 1 day (very mild) to 3-21 days (severe). We estimated an approximate arrival rate of 6 patients/day (2000 /365 days) following exponential distribution. We explored 20-50 bed scenarios and considered an increase in patient arrival to 2500 in the future. Simulation ran for 10,000 replications, evaluating performance measures (e.g., average queue length, wait times, maximum queue length, and capacity utilization) with an initial 25-bed provision."),
              h3("Results"),
              p("The current 25-bed provision was inadequate for the annual patient load of 2000, leading to prolonged wait times and queues. The proportion of patients queuing was around 33% for 25 beds, 45% for 20 beds, 23% for 30 beds, and 7% when 40 beds were available."),
              h3("Conclusions"),
              p("Our study emphasizes the collaborative role of the modeller and clinician in determining the optimal size of a geographical stroke unit. We are developing a web-based application to aid clinicians in estimating stroke unit size, contributing to more effective patient care.")
      ),
      
      # Simulation tab
      tabItem(tabName = "simulation",
              fluidRow(
                column(4,
                       wellPanel(
                         p("Average arrival rate is calculated by dividing the total number of patients by the total number of days in a year. (Arrival rate = total patients / 365.25)"),
                         
                         sliderInput("arrival_rate",
                                     "Arrival rate:",
                                     min = 1,
                                     max = 8,
                                     value = 6,
                                     step = 1),
                         
                         sliderInput("num_patients",
                                     "Number of patients:",
                                     min = 500,
                                     max = 2500,
                                     value = 2000,
                                     step = 500),
                         
                         selectInput("num_beds",
                                     "Number of beds:",
                                     choices = seq(25, 50, by = 5),
                                     selected = 30)
                       )
                ),
                
                column(8,
                       tabsetPanel(
                         tabPanel("Percent of Patients Waiting",
                                  plotOutput("percent_patients_waiting_plot"),
                                  textOutput("percent_patients_waiting_text")),
                         
                         tabPanel("Utilization",
                                  plotOutput("utilization_plot"),
                                  textOutput("utilization_text")),
                         
                         tabPanel("Recommendations"
                                  # Add content for the Recommendations tab here
                         )
                       )
                )
              )
      )
    )
  )
)


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


generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
  length_of_stay <- numeric(total_patients)
  
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

run_simulation <- function(arrival_rate, bed_occupation_time, stroke_level_prob, total_patients, num_simulations) {
  num_bed_range <- seq(25,50, by=1)
  
  performance_metrics_list <- lapply(num_bed_range, function(num_beds) {
    metrics <- lapply(1:num_simulations, function(simulation) {
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

server <- function(input, output) {
  output$percent_patients_waiting_plot <- renderPlot({
    num_simulations <- 150
    results <- run_simulation(arrival_rate = input$arrival_rate, 
                              bed_occupation_time = bed_occupation_time,
                              stroke_level_prob = stroke_level_prob, 
                              total_patients = input$num_patients,
                              num_simulations = num_simulations)
    
    percent_waiting_data <- results[[input$num_beds - 24]]
    
    ggplot(percent_waiting_data, aes(x = num_beds, y = percent_patients_waiting)) +
      geom_point() +
      labs(title = "Percentage of Patients Waiting",
           x = "Number of Beds",
           y = "Percent of Patients Waiting")
  })
  
  output$utilization_plot <- renderPlot({
    utilization_data <- results[[input$num_beds - 24]]
    
    ggplot(utilization_data, aes(x = num_beds, y = utilization)) +
      geom_point() +
      labs(title = "Bed Utilization",
           x = "Number of Beds",
           y = "Utilization")
  })
  
  output$percent_patients_waiting_text <- renderText({
    sprintf("Percent of patients waiting with %d beds: %.2f%%", 
            input$num_beds, mean(results[[input$num_beds - 24]]$percent_patients_waiting))
  })
  
  output$utilization_text <- renderText({
    sprintf("Utilization with %d beds: %.2f%%", 
            input$num_beds, mean(results[[input$num_beds - 24]]$utilization) * 100)
  })
}

shinyApp(ui = ui, server = server)
