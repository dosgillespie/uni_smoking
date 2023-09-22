#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Single site smoking estimator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs_day",
                  "Number of observation times in a day:",
                  min = 1,
                  max = 5,
                  value = 3),
      
      sliderInput("obs_week",
                  "Number of observation days in a week:",
                  min = 1,
                  max = 5,
                  value = 3),
      
      sliderInput("n_weeks",
                  "Number of weeks of repeated observation:",
                  min = 1,
                  max = 5,
                  value = 3),
      
      
      sliderInput("obs_length",
                  "Length of observation period (min):",
                  min = 1,
                  max = 60,
                  value = 20),
      
      sliderInput("n_people",
                  "Expected number of people seen in an observation period:",
                  min = 0,
                  max = 150,
                  value = 50),
      
      sliderInput("prop_smoke",
                  "Percentage of people who use the site who smoke:",
                  min = 0,
                  max = 100,
                  value = 10),
      
      sliderInput("cigs_day",
                  "Average number of cigarettes smoked per day by a typical smoker:",
                  min = 1,
                  max = 40,
                  value = 10),
      
      actionButton("getests", "Resample")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$getests, ignoreNULL = F, {
  
  output$table <- renderTable({
    
    
    
    unismoke <- function(
    obs_day, # Number of times in a day
    obs_week, # Number of days in a week
    n_weeks, # Number of weeks
    n_repeat, # Number of repeat observations at each point
    obs_length, # Length of observation period (min)
    prop_smoke, # Proportion of people on campus who smoke
    n_people, # Number of people seen in an observation period
    cigs_day, # Average cigarettes per day
    hours_day # Waking hours in a day
    ) {
      
      #set.seed(1)
      
      hours_day <- 15 # Waking hours in a day
      
      # Total observations
      obs_total <- obs_day * obs_week * n_weeks
      
      # Probability of smoking a cigarette in an observation period
      prob_smoke <- obs_length * (cigs_day / hours_day) / 60
      
      prob_smoke <- ifelse(prob_smoke > 1, 1, prob_smoke)
      
      # Expected number of smokers in an observation period
      n_smokers_obs <- rbinom(obs_total, rpois(obs_total, n_people), (prop_smoke / 100) * prob_smoke)
      
      # Average number of smokers in an observation period
      av_smoke <- mean(n_smokers_obs)
      
      var_smoke <- mean(n_smokers_obs ^ 2) - (mean(n_smokers_obs) ^ 2)
      
      # Standard error
      se_smoke <- sqrt(var_smoke / obs_total)
      
      lower95_smoke <- av_smoke - 1.96 * se_smoke
      upper95_smoke <- av_smoke + 1.96 * se_smoke
      
      #out_vec <- c(av_smoke, lower95_smoke, upper95_smoke)
      
      # Expected smokers in a day (9am to 5pm - 8 hours)
      #smoke_vec <- out_vec / obs_length
      
      smoke_day_table <- data.frame(Period = c("Typical observation period", "Typical hour", "Typical 8-hour working day"),
                                    exp_smokers = c(av_smoke, 60 * av_smoke / obs_length, 8 * 60 * av_smoke / obs_length),
                                    lower95 = c(lower95_smoke, 60 * lower95_smoke / obs_length, 8 * 60 * lower95_smoke / obs_length),
                                    upper95 = c(upper95_smoke, 60 * upper95_smoke / obs_length, 8 * 60 * upper95_smoke / obs_length)
                                    )
      
      colnames(smoke_day_table) <- c("Period", "Expected number of smokers", "Lower 95% confidence interval", "Upper 95% confidence interval")
      
      return(smoke_day_table)
    }
    
    unismoke(obs_day = input$obs_day, # Number of times in a day
             obs_week = input$obs_week, # Number of days in a week
             n_weeks = input$n_weeks, # Number of weeks
             obs_length = input$obs_length, # Length of observation period (min)
             prop_smoke = input$prop_smoke, # Proportion of people on campus who smoke
             n_people = input$n_people, # Number of people seen in an observation period
             cigs_day = input$cigs_day) # Average cigarettes per day
  })
})
}

# Run the application 
shinyApp(ui = ui, server = server)
