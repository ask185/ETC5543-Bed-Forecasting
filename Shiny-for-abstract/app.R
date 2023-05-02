# Load the Shiny package
library(shiny)

# Define the UI
ui <- navbarPage("Stroke Unit Simulation Study",
                 tabPanel("Main Page",
                          fluidRow(
                            column(6,
                                   h3("Authors"),
                                   tags$ul(
                                     tags$li("Professor Thanh Phan"),
                                     tags$li("Professor Henry Ma"),
                                     tags$li("Aryan Sultan Khan")
                                   )),
                            column(6,
                                   h3("Institutions"),
                                   tags$ul(
                                     tags$li("Monash Medical Research Institute (MMRI)"),
                                     tags$li("Monash University Business School (Faculty of Economics and Business Statistics)")
                                   ))
                          )
                 ),
                 tabPanel("Background",
                          fluidPage(
                            h3("Background"),
                            p("Geographical stroke units offer effective treatment for ischemic and hemorrhagic stroke patients. Optimal care is achieved when patients are allocated to these wards. Determining the optimal number of beds required is a complex task. Our study leverages discrete-event simulation, grounded in queueing theory, to ascertain the ideal number of beds for optimal care given patient influx.")
                          )
                 ),
                 tabPanel("Method",
                          fluidPage(
                            h3("Method"),
                            p("Our model incorporated patient arrival rates, stroke severity, and length of stay using historical data. The distribution was of stroke severity were: very mild (frequency=0.2), mild (frequency=0.2, moderate (0.35), severe (0.1), and very severe (0.15). Bed occupation times ranged from 1 day (very mild) to 3-21 days (severe). We estimated an approximate arrival rate of 6 patients/day (2000 /365 days) following exponential distribution. We explored 20-50 bed scenarios and considered an increase in patient arrival to 2500 in the future. Simulation ran for 10,000 replications, evaluating performance measures (e.g., average queue length, wait times, maximum queue length, and capacity utilization) with an initial 25-bed provision.")
                          )
                 ),
                 tabPanel("Results",
                          fluidPage(
                            h3("Results"),
                            p("The current 25-bed provision was inadequate for the annual patient load of 2000, leading to prolonged wait times and queues. The proportion of patients queuing was around 33% for 25 beds, 45% for 20 beds, 23% for 30 beds, and 7% when 40 beds were available.")
                          )
                 ),
                 tabPanel("Conclusions",
                          fluidPage(
                            h3("Conclusions"),
                            p("Our study emphasizes the collaborative role of the modeller and clinician in determining the optimal size of a geographical stroke unit. We are developing a web-based application to aid clinicians in estimating stroke unit size, contributing to more effective patient care.")
                          )
                 )
)

# Define the server function
server <- function(input, output) {
}

# Run the app
shinyApp(ui = ui, server = server)
