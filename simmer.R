# Discrete Event Simulation

library(simmer)
num_beds <- 5  # Number of beds for stroke patients
V_mild_stroke <- 24 # hours it takes for patient for the very mild stroke to recover (24*7) 
t_enter <- 3 # new patient every ~365*24/700 hours
sim_time <- 24*7 # Simulation time in 7 days

# setup

set.seed(2023)
env <- simmer()
patient <- trajectory() %>%
  log_("addmitted to the stroke ward") %>%
  seize("bed", 1) %>%
  timeout(V_mild_stroke) %>%
  set_attribute("patient_recovered", function() sample(50:99, 1)) %>%
  release("bed", 1) %>%
  log_("leaves the ward")
env %>%
  add_resource("bed", num_beds) %>%
  # feed the trajectory with 4 initial patients
  add_generator("patient_initial", patient, at(rep(0, 4))) %>%
  # new patient approx. every t_enter minutes
  add_generator("patient", patient, function() sample((t_enter-2):(t_enter +2), 1)) %>%
  # start the simulation
  run(sim_time)

resource <- get_mon_resources(env)
plot(resource)

