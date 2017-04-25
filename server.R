library(dplyr)
library(shiny)
library(ggplot2)
#library(markovchain)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  source("markov_chain_model.R")
  time_step = 0.25
  max_miles = 800
  bucket_miles = 10
  production_range = 22 #months from Jan 2014 to Oct 2015
  produced_parts = 728112
  months_today = 40  # months from Jan 2014 to today

  probs <- reactive({
    detection_rate <- input$detected_parts / produced_parts
    start_to_service <- input$delivery_to_service + production_range * 0.5
    steps_to_today <- ceiling( months_today / time_step )
    ir <- find_incidence_rate(detection_prob = detection_rate, steps = steps_to_today,
                              max_miles = max_miles, bucket_miles = bucket_miles, time_step = time_step,
                              ave_speed = input$ave_speed, mean_mtf = input$mean_mtf, min_mtf = input$min_mtf,
                              start_to_service = start_to_service)
    mc <- get_markov_model(max_miles = max_miles, bucket_miles = bucket_miles, time_step = time_step,
                           ave_speed = input$ave_speed, mean_mtf = input$mean_mtf, min_mtf = input$min_mtf,
                           start_to_service = start_to_service,
                           incidence_rate = ir$root ) 
    compute_state_probabilities(mc = mc, steps = steps_to_today)
  })
  
    
  output$miles_dist_plot <- renderPlot({
    p <- ggplot(probs() %>% dplyr::filter( quality != "UNKNOWN") )
    print(
      p + geom_bar(aes(x=miles_driven, y=prob), stat = "identity") + 
        facet_grid( detection + quality ~ ., scale = "free_y") + xlab("Thousand Miles Driven") + 
        ylab("Probability") + xlim(0,600) +
        theme_set(theme_gray(base_size = 22)) +
        theme(strip.text.y = element_text(size = 12))
    )
  })
  
  output$summary_table <- renderTable({
    tbl <-  probs() %>% dplyr::group_by(quality, detection) %>%
            summarize(average_miles = sum( prob * miles_driven ) / sum(prob), probability = sum(prob))
    data.frame(tbl)
  }, digits = c(1, 1, 1, 1, 2), display = c("s","s","s","f","e"))
  
  output$incidence_rate <- renderText({
      bad <- sum( probs() %>% dplyr::filter(quality == "BAD") %>% dplyr::select(prob))
      good <- sum( probs() %>% dplyr::filter(quality == "GOOD") %>% dplyr::select(prob))
      sprintf("Defect rate %.2e", bad / good)
    })                                                                                            
  
})