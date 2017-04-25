library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Defect Rate Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("ave_speed",
                  "Average truck speed (thousand miles per month)",
                  min = 1,
                  max = 20,
                  value = 10),
      sliderInput("delivery_to_service",
                  "Average time from part delivery to field deployment (months)",
                  min = 0,
                  max = 20,
                  value = 10),
      sliderInput("mean_mtf",
                  "Average miles to failure (x1000)",
                  min = 10,
                  max = 400,
                  value = 160),
      sliderInput("min_mtf",
                  "Minimum miles before a failure is possible (x1000)",
                  min = 0,
                  max = 200,
                  value = 70),
      sliderInput("detected_parts",
                  "Defective parts detected",
                  min = 1,
                  max = 20,
                  value = 9)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Distribution of Miles Driven"),
      plotOutput("miles_dist_plot", height = 500),
      h3("Summary of Probability"),
      tableOutput("summary_table"),
      h2(textOutput("incidence_rate"))
    )
  )
))