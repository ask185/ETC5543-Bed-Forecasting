# install.packages(c("shiny", "ggplot2"))
# install.packages("shinythemes")
# library(shiny)
# library(ggplot2)
# library(tidyverse)
# library(shinydashboard)
# library(shinythemes)
# library(plotly)
# library(shinycssloaders)
# 
# source("functions.R")
# 
# set.seed(070523)
#   bed_occupation_time <- data.frame(
#     category = 1:6,
#     mean_time = c(4.43, 4.46, 6.67, 6.92, 7.74, 9.29)
#   )
# 
#   stroke_level_prob <- c(0.0570, 0.342, 0.217, 0.136, 0.0971, 0.151)
# 
#   shiny::addResourcePath("myimages", "images")
# 
# 
#   ui <- fluidPage(
#     theme = shinytheme("superhero"),
#     titlePanel("Stroke Simulation App"),
# 
#     tags$div(
#       tags$p("Stroke is one of the leading causes to disability and death in the Australia.
#     According to the Australian Institute of Health and Welfare, in 2020, stroke was
#     recorded as the underlying cause of 8,200 deaths, accounting for 5.1% of all deaths in
#     Australia. Stroke was one of the 5 leading causes of death in Australia – on average,
#     22 Australians died of stroke each day in 2020. Hence the stroke care units are crucial
#     in reducing the death rates and speedying up the recovery for stroke patients.
#     This app simulates the percentage of stroke patients waiting for a bed and
#            bed utilization in a stroke unit at Monash Medical Center based on the number of
#            patients and the number of available beds.
#            As per our analysis the optimal number of beds for the annual load of 2000 patients
#            should be 40 in the stroke care unit.
#            "),
#       style = "background-color: #f0f0f0; color: black; padding: 10px;"
#     ),
#     br(),
# 
#     fluidRow(
#       column(3,
#              wellPanel(
#                selectInput("num_patients", "Number of Patients:",
#                            choices = c(350, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500),
#                            selected = 350),
#                sliderInput("num_beds", "Number of beds:",
#                            min = 5, max = 50, value = 12, step = 1)
#              )
#       ),
#       column(6,
#              tabsetPanel(
#                tabPanel("Percent of Patients Waiting",
#                         withSpinner(plotlyOutput("patients_waiting_plot")),
#                         tags$div(
#                           textOutput("subtitle_text"),
#                           style = "background-color: steelblue; color: white; padding: 8px;
#                           border-radius: 3px; text-align: center;"),
#                         tags$div(
#                           textOutput("patients_waiting_text"),
#                           style = "background-color: #f0f0f0; color: black; padding:
#                           8px; border-radius: 3px; text-align: center;"
#                         ),
#                         tags$div(
#                           textOutput("utilization_text"),
#                           style = "background-color: lightgreen; color: black;
#                           padding: 8px; border-radius: 3px; text-align: center;"
#                         )
#                ),
#                tabPanel("Bed Utilization",
#                         withSpinner(plotOutput("utilization_plot")),
#                         tags$div(
#                           "The optimal utilization rate is considered to be 80% or over.",
#                           style = "padding: 8px; text-align: center;"
#                         )
#                )
#              )
#       ),
#       column(3,
#              plotlyOutput("static_plot"))
#     )
#   )
# 
# 
# 
# server <- function(input, output) {
#   set.seed(07052023)
# 
#   waiting_percents_data <- reactive({
#     num_beds_range <- seq(5, 50, by = 1)
#     waiting_percents <- sapply(num_beds_range, function(num_beds) {
#       sim_results <- stroke_simulation(as.numeric(input$num_patients),
#                                        as.numeric(input$num_patients) / 365,
#                                        bed_occupation_time,
#                                        stroke_level_prob,
#                                        num_beds)
#       sim_results$percent_patients_waiting
#     })
#     return(waiting_percents)
#   })
# 
#   simulation_results <- reactive({
#     set.seed(07052023)
#     sim_results <- stroke_simulation(as.numeric(input$num_patients),
#                                      as.numeric(input$num_patients) / 365,
#                                      bed_occupation_time,
#                                      stroke_level_prob,
#                                      as.numeric(input$num_beds))
#     return(sim_results)
#   })
# 
#   optimal_beds <- function(num_patients) {
#     if (num_patients == 350) {
#       return(paste(10, 11, sep = "-"))
#     } else if (num_patients == 500) {
#       return(paste(13, 14, sep = "-"))
#     } else if (num_patients == 750) {
#       return(paste(17, 18, sep = "-"))
#     } else if (num_patients == 1000) {
#       return(paste(22, 23, sep = "-"))
#     } else if (num_patients == 1250) {
#       return(paste(27, 28, sep = "-"))
#     } else if (num_patients == 1500) {
#       return(paste(31, 32, sep = "-"))
#     } else if (num_patients == 1750) {
#       return(paste(35, 36, sep = "-"))
#     } else if (num_patients == 2000) {
#       return(paste(40, 41, sep = "-"))
#     } else if (num_patients == 2250) {
#       return(paste(44, 45, sep = "-"))
#     } else if (num_patients==2500) {
#       return(paste(48, 49, sep = "-"))
#     }
# 
#   }
# 
# 
#   output$subtitle_text <- renderText({
#     subtitle_text <- paste("Optimal number of beds for", input$num_patients, "patients:",
#                            optimal_beds(input$num_patients), "\n")
#     return(subtitle_text)
#   })
# 
#   output$patients_waiting_plot <- renderPlotly({
#     set.seed(07052023)
#     num_beds_range <- seq(5, 50, by = 1)
# 
#     waiting_percents <- waiting_percents_data()
# 
#     data <- data.frame(
#       num_beds = num_beds_range,
#       percent_patients_waiting = waiting_percents,
#       selected = num_beds_range == input$num_beds
#     )
# 
#     # optimal_bed_data <- data.frame(
#     #   num_patients = c(500, 1000, 1500, 2000, 2500),
#     #   optimal_beds = c(12, 23, 32, 41, 49),
#     #   label = c("Optimal Beds (500 patients): 12-13",
#     #             "Optimal Beds (1000 patients): 22-24",
#     #             "Optimal Beds (1500 patients): 31-33",
#     #             "Optimal Beds (2000 patients): 40-42",
#     #             "Optimal Beds (2500 patients): 49-50")
#     # )
# 
# 
#     p_one <- ggplot(data, aes(x = num_beds, y = percent_patients_waiting, fill = selected)) +
#       geom_point(size=2, stroke=0) +
#       geom_segment(aes(x = num_beds, xend = num_beds, y = 0, yend = percent_patients_waiting),
#                   color = "steelblue")+
#      geom_hline(yintercept = 5, color = "red", size = 0.3) +
#      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5),
#                         labels = function(x) paste0(x, "%"))+
#       scale_x_continuous(breaks = seq(5, 50, by = 1), expand = c(0, 0.5)) +
#       scale_fill_manual(values = c("steelblue", "red")) +
#       labs(title = "Percent of Patients Waiting vs Number of Beds",
#            x = "Number of Beds",
#            y = "Percent of Patients Waiting") +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 90 ,hjust = 1),
#             legend.position = "none")+
#       theme(axis.text.x = element_text(size = 10),
#             axis.text.y = element_text(size = 10),
#             plot.title = element_text(size = 14),
#             axis.title.x = element_text(size = 13),
#             axis.title.y = element_text(size = 13),
#             legend.position = "none")
#       # geom_text(data = optimal_bed_data,
#       #           aes(x = num_patients, y = 0, label = label),
#       #           size = 3, hjust = 0, vjust = 1.5, angle = 90)
# 
#    ggplotly(p_one)
#   })
# 
# 
#   # Patients waiting text
#   output$patients_waiting_text <- renderText({
#     req(simulation_results())
#     paste("Percent of patients waiting for a bed at", input$num_patients,"patients in total and" ,
#           input$num_beds, "beds:", round(simulation_results()$percent_patients_waiting, 2), "%")
#   })
# 
#   # Utilization plot
#   utilization_percents_data <- reactive({
#     num_beds_range <- seq(5, 50, by = 1)
#     utilization_percents <- sapply(num_beds_range, function(num_beds) {
#       sim_results <- stroke_simulation(as.numeric(input$num_patients),
#                                        as.numeric(input$num_patients) / 365,
#                                        bed_occupation_time,
#                                        stroke_level_prob,
#                                        num_beds)
#       sim_results$bed_utilization
#     })
#     utilization_percents
#   })
# 
#   output$utilization_plot <- renderPlot({
#     set.seed(07052023)
#     req(simulation_results())
#     num_beds_range <- seq(5, 50, by = 1)
# 
#     utilization_percents <- utilization_percents_data()
# 
#     data <- data.frame(
#       num_beds = num_beds_range,
#       bed_utilization = utilization_percents,
#       selected = num_beds_range == input$num_beds
#     )
# 
#      ggplot(data, aes(x = num_beds, y = bed_utilization, color = selected)) +
#       geom_point(size = 4) +
#       geom_segment(aes(x = num_beds, xend = num_beds, y = 0, yend = bed_utilization),
#                    color = "steelblue") +
#       scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5),
#                          labels = scales::label_number(accuracy = 0.01)) +
#       scale_x_continuous(breaks = seq(5, 50, by = 1), expand = c(0, 0)) +
#       scale_color_manual(values = c("steelblue", "red")) +
#       labs(title = "Bed Utilization vs Number of Beds",
#            x = "Number of Beds",
#            y = "Bed Utilization (%)") +
#       theme_minimal() +
#        theme(axis.text.x = element_text(angle=90, hjust = 1),
#              legend.position = "none") +
#       theme(axis.text.x = element_text(size = 10),
#             axis.text.y = element_text(size = 10),
#             plot.title = element_text(size = 12),
#             axis.title.x = element_text(size = 11),
#             axis.title.y = element_text(size = 11),
#             legend.position = "none")
# 
#   })
# 
#   # Utilization text
#   output$utilization_text <- renderText({
#     utilization_percents <- utilization_percents_data()
#     selected_utilization_percent <- utilization_percents[input$num_beds - 4]
#     paste("Bed utilization percent for a total of", input$num_patients,"patients and",
#           input$num_beds, "beds:", round(selected_utilization_percent, 2), "%")
#   })
# 
#   set.seed(07052023)
#   bed_occupation_time_df <- data.frame(
#     stroke_category = c("Very Mild", "Mild", "Mild-Moderate", "Moderate",
#                         "Moderate-Severe", "Severe"),
#     bed_occupation_time = c(4.43,4.46,6.67,6.92,7.74,9.29),
#     probability = c(0.0570,0.342, 0.217, 0.136, 0.0971, 0.151)
#   )
# 
#   # Static plot
#   output$static_plot <- renderPlotly({
#     p_two <- ggplot(bed_occupation_time_df, aes(x = stroke_category, y = bed_occupation_time)) +
#       geom_col(fill = "steelblue") +
#       theme_minimal()+
#       geom_text(aes(label = scales::percent(probability, accuracy = 0.1)),
#                 position = position_stack(vjust = 0.3), color = "white", size = 2.5) +
#       theme_minimal() +
#       labs(
#         x = "Stroke Category",
#         y = "Bed Occupation Time (days)") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#     ggplotly(p_two)
#   })
# 
# }
# 
# shinyApp(ui = ui, server = server)
###############################################################################

#install.packages(c("shiny", "ggplot2"))
#install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(shinycssloaders)
#install.packages("DT")
library(DT)



ui <- fluidPage(
  theme = shinytheme("superhero"),
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
           As per our analysis the optimal number of beds for the annual load of 350 patients
           should be 10-11 beds in the stroke care unit.
           "),
    style = "background-color: #f0f0f0; color: black; padding: 10px;"
  ),
  br(),
  
  fluidRow(
    column(3,
           wellPanel(
             selectInput("num_patients", "Number of Patients:",
                         choices = c(350, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500),
                         selected = 350),
             sliderInput("num_beds", "Number of beds:",
                         min = 5, max = 50, value = 12, step = 1),
             sliderInput("very_mild", "Very Mild Stroke Occupation Time:", min = 1, max = 7, value = 4),
             sliderInput("mild", "Mild Stroke Occupation Time:", min = 1, max = 7, value = 4),
             sliderInput("moderate", "Moderate Stroke Occupation Time:", min = 3, max = 10, value = 7),
             sliderInput("moderate_severe", "Moderate-Severe Stroke Occupation Time:", min = 3, max = 10, value = 7),
             sliderInput("severe", "Severe Stroke Occupation Time:", min = 3, max = 10, value = 8),
             sliderInput("very_severe", "Very Severe Stroke Occupation Time:", min = 3, max = 10, value = 10)
           )
    ),
    column(6,
           tags$head(tags$style(HTML("
    #length_of_stay_table table {
      width: 100%;
      margin: 0 auto;
      margin-top: 0;
      color: white;
      background-color: steelblue;
    }
    #length_of_stay_table table td, #length_of_stay_table table th {
      text-align: center;
    }
    #table_title {
      padding-bottom: 0;
    }
  "))),
           tagList(
             tags$h5(
               "Mean, Min., and Max., length of stay (days) across all six the stroke categories.",
               id = "table_title",
               style = "background-color: steelblue; color: white; padding: 4px;
               border-radius: 1px; text-align: center;"
             ),
             tableOutput("length_of_stay_table")
           )
    ),
    
    column(6,
           tabsetPanel(
             tabPanel("Percent of Patients Waiting",
                      withSpinner(plotlyOutput("patients_waiting_plot", height = 500)),
                      tags$div(
                        textOutput("optimal_beds_text"),
                        style = "background-color: steelblue; color: white; padding: 8px;
                    border-radius: 3px; text-align: center;"
                      ),
                      tags$div(
                        textOutput("patients_waiting_text"),
                        style = "background-color: #f0f0f0; color: black; padding: 8px; border-radius: 3px; text-align: center;"
                      )
             ),
             tabPanel("Bed Utilization",
                      withSpinner(plotOutput("utilization_plot", height = 500)), 
                      tags$div(
                        textOutput("optimal_beds_text_utilization"),
                        style = "background-color: steelblue; color: white; padding: 8px;
                    border-radius: 3px; text-align: center;"
                      ),
                      tags$div(
                        textOutput("utilization_text"),
                        style = "background-color: lightgreen; color: black; padding: 8px; border-radius: 3px; text-align: center;"
                      )
             )
           )
    ),
    column(3,
           plotlyOutput("static_plot"),
           tags$p("Length of Stay) for each category of stroke as selected by the user.",
                  style = "background-color: steelblue; color: white; padding: 8px;
                          border-radius: 3px; text-align: right;")
    ),
  )
)



server <- function(input, output) {
  set.seed(07052023)
  
  bed_occupation_time <- reactive({
    data.frame(
      category = 1:6,
      mean_time = c(input$very_mild, input$mild, input$moderate, input$moderate_severe, input$severe, input$very_severe)
    )
  })
  
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
  
  generate_length_of_stay <- function(total_patients, stroke_levels) {
    length_of_stay <- numeric(total_patients)
    bot <- bed_occupation_time()
    
    for (i in 1:total_patients) {
      stroke_level <- sample(stroke_levels, 1, prob = bot$probability)
      length_of_stay[i] <- bot$mean_time[stroke_level]
    }
    return(length_of_stay)
  }
  
  stroke_simulation <- function(total_patients, num_beds, bed_occupation_time) {
    # total_patients <- as.numeric(input$num_patients)
    # num_beds <- as.numeric(input$num_beds)
    bed_occupation_time <- bed_occupation_time()
    arrival_rate <- total_patients/365
    
    interarrival_times <- generate_interarrival_times(total_patients, arrival_rate)
    stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
    length_of_stay <- generate_length_of_stay(total_patients, stroke_levels)
    
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
  
  
  length_of_stay_output <- reactive({
    total_patients <- as.numeric(input$num_patients)
    stroke_levels <- assign_stroke_levels(total_patients, stroke_level_prob)
    length_of_stay <- generate_length_of_stay(total_patients, stroke_levels)
    return(length_of_stay)
  })
  
  output$length_of_stay_table <- renderTable({
    req(length_of_stay_output())
    los_values <- length_of_stay_output()
    los_table <- data.frame(
      Mean = round(mean(los_values), 2),
      Min = round(min(los_values), 2),
      Max = round(max(los_values), 2)
    )
    colnames(los_table) <- c("Mean Length (days)", "Minimum Length (days)", "Maximum Length (Days)")
    return(los_table)
  }, row.names = FALSE)
  
  waiting_percents_data <- reactive({
    num_beds_range <- seq(5, 50, by = 1)
    total_patients <- as.numeric(input$num_patients)
    bed_occupation_time <- bed_occupation_time()
    
    waiting_percents <- sapply(num_beds_range, function(num_beds) {
      sim_results <- stroke_simulation(total_patients, num_beds, bed_occupation_time)
      sim_results$percent_patients_waiting
    })
    
    return(waiting_percents)
  })
  
  simulation_results <- reactive({
    set.seed(07052023)
    sim_results <- stroke_simulation(as.numeric(input$num_patients),
                                     as.numeric(input$num_beds),
                                     bed_occupation_time())
    return(sim_results)
  })
  
  output$patients_waiting_plot <- renderPlotly({
    # Get the reactive data
    waiting_percents <- waiting_percents_data()
    
    # Create the data frame
    df <- data.frame(
      NumBeds = seq(5, 50, by = 1),
      PercentPatientsWaiting = as.integer(round(waiting_percents)))
    
    find_optimal_beds <- function(total_patients, bed_occupation_time) {
      for (num_beds in 5:50) {
        sim_results <- stroke_simulation(total_patients, num_beds, bed_occupation_time)
        if (sim_results$percent_patients_waiting <= 5) {
          return(num_beds)
        }
      }
      return(NA)
    }
    
    optimal_beds <- reactive({
      find_optimal_beds(as.numeric(input$num_patients), bed_occupation_time())
    })
    
    output$optimal_beds_text <- renderText({
      optimal_beds_val <- optimal_beds()
      if (is.na(optimal_beds_val)) {
        "No optimal number of beds found (i.e., no number of beds results in <= 5% of patients waiting)"
      } else {
        paste("Optimal number of beds (<= 5% of patients waiting): ", optimal_beds_val)
      }
    })
    optimal_beds <- reactive({
      find_optimal_beds(as.numeric(input$num_patients), bed_occupation_time())
    })
    
    # Create the lollipop plot
    p <- ggplot(df, aes(x = NumBeds, y = PercentPatientsWaiting)) +
      geom_point(data = subset(df, NumBeds != input$num_beds), size = 2, stroke = 0) +
      geom_point(data = subset(df, NumBeds == input$num_beds), size = 2, stroke = 0, color = "red") +
      geom_segment(aes(x = NumBeds, xend = NumBeds, y = 0, yend = PercentPatientsWaiting),
                   color = "steelblue") +
      geom_hline(yintercept = 5, color = "red", size = 0.3) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5),
                         labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = seq(5, 50, by = 1), expand = c(0, 0.5)) +
      labs(title = "Percent of Patients Waiting vs Number of Beds",
           x = "Number of Beds",
           y = "Percent of Patients Waiting") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90 ,hjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 14),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = "none")
    
    # p <- p + geom_text(data = optimal_bed_data,
    #                    aes(x = NumBeds, y = 0, label = label),
    #                    size = 3, hjust = 0, vjust = 1.5, angle = 90)
    
    ggplotly(p)
  })
  
  
  output$static_plot <- renderPlotly({
    p_two <- ggplot(bed_occupation_time(), aes(x = factor(category), y = mean_time)) +
      geom_col(fill = "steelblue") +
      theme_minimal()+
      labs(
        x = "Stroke Category",
        y = "Length of stay (days)") +
      scale_x_discrete(labels = c("Very Mild", "Mild", "Moderate", "Moderate-Severe", "Severe", "Very Severe")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p_two)
  })
  
  utilization_percents_data <- reactive({
    utilization_percents <- sapply(5:50, function(x) {
      stroke_simulation(as.numeric(input$num_patients), x, bed_occupation_time())$bed_utilization
    })
    utilization_percents
  })
  
  output$utilization_plot <- renderPlot({
    # Get the reactive data
    utilization_percents <- utilization_percents_data()
    
    # Create the data frame
    df <- data.frame(
      NumBeds = seq(5, 50, by = 1),
      Utilization = utilization_percents)
    
    # Create the lollipop plot
    p <- ggplot(df, aes(x = NumBeds, y = Utilization)) +
      geom_point(data = subset(df, NumBeds != input$num_beds), size = 4, stroke = 0) +
      geom_point(data = subset(df, NumBeds == input$num_beds), size = 4, stroke = 0, color = "red") +
      geom_segment(aes(x = NumBeds, xend = NumBeds, y = 0, yend = Utilization),
                   color = "steelblue") +
      geom_hline(yintercept = 80, color = "red", size = 0.5) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5),
                         labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = seq(5, 50, by = 1), expand = c(0, 0.5)) +
      labs(title = "Bed Utilization vs Number of Beds",
           x = "Number of Beds",
           y = "Bed Utilization (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90 ,hjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 14),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = "none")
    
    print(p)
  })
  
  output$patients_waiting_text <- renderText({
    paste0("Percent of patients waiting at the selected number of beds (", input$num_beds, 
           ") and the annual patient load of ", input$num_patients, " are ", sprintf("%.2f", simulation_results()$percent_patients_waiting * 100), "%.")
  })
  
  output$utilization_text <- renderText({
    paste0("Utilization at the selected number of beds (", input$num_beds, 
           ") and the annual patient load of ", input$num_patients, " is ", sprintf("%.2f", simulation_results()$bed_utilization), "%.")
  })
  
  
}


shinyApp(ui = ui, server = server)

