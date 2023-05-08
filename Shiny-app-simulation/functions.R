library(tidyverse)

total_patients <- 2000 # assuming there are 2000 all around the year
total_patients_future <- 2500 # assuming the 2500 patients in the future 
patients_per_month <- 2000/12 # patients per month
arrival_rate <- total_patients/365  # average 6 patients per day
arrival_rate_future <- total_patients_future/365 # average 7 patients per day

bed_occupation_time <- data.frame(
  category = 1:6,
  mean_time = c(4.43, 4.46, 6.67, 6.92, 7.74, 9.29)
)

stroke_level_prob <- c(0.0570,0.342, 0.217, 0.136, 0.0971, 0.151)

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

generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
  length_of_stay <- numeric(total_patients)
  
  for (i in 1:total_patients) {
    stroke_level <- sample(stroke_levels, 1, prob = bed_occupation_time$probability)
    #print(paste("stroke_level:", stroke_level)) # Print the value of stroke_level
    #print(paste("bed_occupation_time$mean_time:", bed_occupation_time$mean_time)) # Print the value of bed_occupation_time$mean_time
    
    length_of_stay[i] <- bed_occupation_time$mean_time[stroke_level]
    #print(paste("length_of_stay[", i, "] =", length_of_stay[i]))
  }
  
  return(length_of_stay)
}


# generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
#   length_of_stay <- numeric(total_patients)
#   
#   for (i in 1:total_patients) {
#     stroke_level <- stroke_levels[i]
#     
#     if (length(bed_occupation_time[[stroke_level]]) == 1) {
#       length_of_stay[i] <- bed_occupation_time[[stroke_level]][1]
#     } else {
#       length_of_stay[i] <- sample(bed_occupation_time[[stroke_level]][1]:bed_occupation_time[[stroke_level]][2], 1)
#     }
#   }
#   
#   return(length_of_stay)
# }

stroke_simulation <- function(total_patients, 
                              arrival_rate, 
                              bed_occupation_time, 
                              stroke_level_prob, 
                              num_beds) {
  
  interarrival_times <- generate_interarrival_times(total_patients, arrival_rate)
  stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
  length_of_stay <- generate_length_of_stay(total_patients, bed_occupation_time, stroke_levels)
  
  beds <- rep(0, num_beds)
  waiting_times <- numeric(total_patients)
  
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
      waiting_times[i] <- length_of_stay[i]
    }
  }
  
  patients_waiting <- sum(waiting_times > 0)
  percent_patients_waiting <- (patients_waiting / total_patients) * 100
  bed_utilization <- sum(length_of_stay) / (num_beds * sum(interarrival_times)) * 100
  
  return(list(percent_patients_waiting = percent_patients_waiting,
              bed_utilization = bed_utilization))
}
# run_simulation <- function(arrival_rate, 
#                            bed_occupation_time, 
#                            stroke_level_prob, 
#                            total_patients, 
#                            num_simulations) {
#   num_bed_range <- seq(20,50, by=1)
#   
#   performance_metrics_list <- lapply(1:num_simulations, function(simulation) {
#     lapply(num_bed_range, function(num_beds) {
#       stroke_sim <- stroke_simulation(total_patients =  total_patients,
#                                       arrival_rate = arrival_rate,
#                                       bed_occupation_time = bed_occupation_time,
#                                       stroke_level_prob = stroke_level_prob,
#                                       num_beds = num_beds)
#       
#       num_beds <- length(stroke_sim$beds)
#       total_patients <- total_patients
#       
#       occupied_beds <- sum(stroke_sim$beds > 0)
#       utilization <- occupied_beds / num_beds
#       
#       total_wait_times <- sum(unlist(stroke_sim$queue))
#       num_patients_queue <- length(stroke_sim$queue)
#       avg_wait_times <- total_wait_times / num_patients_queue
#       
#       average_time_in_queue = total_wait_times / num_patients_queue
#       max_queue_length <- length(stroke_sim$queue)
#       
#       proportion_patients_waiting <- num_patients_queue / total_patients
#       percent_patients_waiting <- proportion_patients_waiting * 100
#       
#       avg_length_of_stay <- (sum(stroke_sim$length_of_stay)) / total_patients
#       
#       performance_metrics_month <- data.frame(num_beds = num_beds,
#                                               utilization = utilization,
#                                               avg_wait_times = avg_wait_times,
#                                               max_queue_length = max_queue_length,
#                                               proportion_patients_waiting = proportion_patients_waiting,
#                                               percent_patients_waiting = percent_patients_waiting,
#                                               avg_length_of_stay = avg_length_of_stay)
#       
#       return(performance_metrics_month)
#     })
#   })
#   
#   return(performance_metrics_list)
# }
# 
# results <- run_simulation(arrival_rate = 6, 
#                           bed_occupation_time = bed_occupation_time, 
#                           stroke_level_prob = stroke_level_prob, 
#                           total_patients = 2000, 
#                           num_simulations = 100)
# 
# results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
# 
