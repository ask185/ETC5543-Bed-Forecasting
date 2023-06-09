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


```

```{r}
set.seed(2023)
# the function below is created to generate the inter-arrival times between the patients.
# the inter arrival follows an exponential distribution hence rexp(), arrival rate is
# defined above

generate_interarrival_times <- function(total_patients, arrival_rate) {
  interarrival_times <- rexp(total_patients, rate = arrival_rate)
  return(interarrival_times)
}

# the function is created to assign patients (from total_patients) randomly a stroke level.
# This assignment is based on the probabilities defined in the stroke_level_prob. 
# To achieve this we use sample()

assign_stroke_levels <- function(total_patients, stroke_level_prob) {
  stroke_levels <- sample(1:length(stroke_level_prob), 
                          size = total_patients, replace = TRUE,
                          prob = stroke_level_prob)
  return(stroke_levels)
}

# if the probability of the severe stroke is really high, say 0.7, 
# then the sample() function would generate random samples of 2000 patients 
# wherein most patients would be assigned the severe stroke level category. 

# Function to generate length of stay for each patient
# this function takes in three arguments each of which has been defined earlier

stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
stroke_levels_future <- assign_stroke_levels(total_patients = total_patients_future,
                                             stroke_level_prob = stroke_level_prob)

generate_length_of_stay <- function(total_patients, bed_occupation_time, stroke_levels) {
  length_of_stay <- numeric(total_patients)
  # for every patient in total patients (2000)
  for (i in 1:total_patients) {
    # creating a variable stroke_level that stores the severity of the stroke 
    # (levels/category) of the current (i the) patient in the iteration
    stroke_level <- stroke_levels[i]
    # if the stroke level is 1 (very mild) then set the length of the stay == bed occupation
    # times of the patient with the level 1 stroke. This is done for every patient with level
    # 1 stroke
    if (stroke_level == 1) {
      length_of_stay[i] <- bed_occupation_time[[stroke_level]]
    } else {
      # else if the stroke level is not 1 
      # then set the length of stay of the patient equal to 
      # random number of days between the lower and upper bound of the days 
      # the patient with a certain level of severity would need to stay
      # in the ward
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

```

```{r}
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
  
  
  # inter-arrival_time is an input parameter for this function, 
  # which represents the time between patient arrivals. 
  # This variable is based on the generate_interarrival_times function, 
  # where we input the total number of patients and arrival rate.
  
  # The pmax function ensures that the bed occupation times do not become negative. 
  # A value of 0 in this context means that the bed is available 
  # (i.e., the patient has completed their stay, 
  # and the bed is now available for other patients).
  
  update_bed_occupation_times <- function(beds, interarrival_time) {
    beds <- pmax(beds - interarrival_time, 0)
    
    # 'beds' is a numeric vector that represents the current remaining 
    # bed occupation time for each bed in the simulation.
    # Each element in this vector corresponds to a bed in the hospital
    # ward. A value of 0 means that the bed is available for use.
    # Subtracting the interarrival_time from each element of 'beds' 
    # updates the remaining bed occupation times, considering the 
    # passage of time between patient arrivals.
    # The pmax function is used to ensure that the resulting bed 
    # occupation times do not become negative, meaning that a 
    # bed with a remaining time less than or equal to the interarrival_time
    # will be considered available (set to 0).
    return(beds)
  }
  
  # for every patient in total patients (1000)
  
  for (i in 1:total_patients) {
    
    # update the beds variable with the bed occupation times 
    # of all the patients arriving at the inter-arrival times
    
    beds <- update_bed_occupation_times(beds, interarrival_times[i])
    # creating a new variable available beds to keep track 
    # of the available beds the beds that are available will
    # have the bed occupation time of 0 so in this variable 
    # I store the beds that are available hence which(beds==0)
    
    available_beds <- which(beds == 0)
    
    # here if the length of the available beds vector 
    # is greater than 0 which means that 
    # there is at least one bed with bed occupation times ==0 
    # then assign the first available bed to the patient 
    # by setting this available bed (bed occupation times ==0) 
    # to the length of the stay of the patient 
    # (whatever the severity of that patient might be)
    
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
```



```{r}
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

```

```{r}
# Combine all the data frames into one
results_df <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))

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



```