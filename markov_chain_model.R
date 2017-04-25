#library(markovchain)
library(dplyr)
library(ggplot2)
library(expm)

#' Gets a Markov model of a part's lifecycle
#' @param max_miles The maximum number of miles to consider in the state space
#' @param bucket_miles Determines the size of the grid in the state space
#' @param time_step The length in time units of a time step in the Markov chain
#' @param ave_speed Average miles per time unit driven by the truck
#' @param mean_mtf Mean miles to failure of a part
#' @param min_mtf Minimum miles at which a failure can occur
#' @param start_to_service The mean time for a part to get into service (includes time before production)
#' @param incidence_rate An estimate on the probability that a part is defective
get_markov_model <- function(max_miles, bucket_miles, time_step, ave_speed, mean_mtf,
                             min_mtf, start_to_service, incidence_rate) {
  
  miles_seq <- seq(0, max_miles, bucket_miles)
  bad_notdetected <- paste0("B-","ND-", miles_seq)
  bad_detected <- paste0("B-","D-", miles_seq)
  good <- paste0("G-", miles_seq)
  
  state_names <- c("start", good, bad_notdetected, bad_detected)
  state_ids <- seq(length(state_names))
  names(state_ids) <- state_names
  num_states <- length(state_names)
  
  transition_matrix <- matrix(0, nrow=num_states, ncol=num_states)
  
  #Transition from start state
  steps_to_service <- max(start_to_service, time_step) / time_step
  transition_matrix[ state_ids["start"], state_ids["B-ND-0"] ] <- incidence_rate / steps_to_service
  transition_matrix[ state_ids["start"], state_ids["G-0"] ] <- ( 1 - incidence_rate ) / steps_to_service
  transition_matrix[ state_ids["start"], state_ids["start"] ] <- 1 - 1 / steps_to_service
  
  #Transitions for good states
  steps_to_next_bucket <- max(bucket_miles / ave_speed, time_step) / time_step
  move_prob <- 1 / steps_to_next_bucket
  for ( i in seq_along(miles_seq) ) {
    from <- state_ids[paste0("G-", miles_seq[i])]
    if ( i < length(miles_seq) ) {
      to <- state_ids[paste0("G-", miles_seq[i+1])]
      transition_matrix[from, to] <- move_prob
      transition_matrix[from, from] <- 1 - move_prob
    } else {
      transition_matrix[from, from] <- 1    
    }
  }
  
  #Transitions for bad undetected states
  mttf <- mean_mtf / ave_speed
  mttf_after_min <- mttf - min_mtf / ave_speed
  failure_prob_after_min <- 1 / ( max( mttf_after_min, time_step ) / time_step )
  for ( i in seq_along(miles_seq) ) {
    from <- state_ids[paste0("B-ND-", miles_seq[i])]
    to_d <- state_ids[paste0("B-D-", miles_seq[i])]
    if ( i < length(miles_seq) ) {
      if ( miles_seq[i] < min_mtf ) {
        failure_prob <- 0
      } else {
        failure_prob <- failure_prob_after_min
      }
      to_nd <- state_ids[paste0("B-ND-", miles_seq[i+1])]
      transition_matrix[from, to_nd] <- (1 - failure_prob) * move_prob
      transition_matrix[from, from] <- (1 - failure_prob) * ( 1 - move_prob )
      transition_matrix[from, to_d] <- failure_prob
    } else {
      transition_matrix[from, to_d] <- 1
    }
  }
  
  #Transitions for bad detected states (absorbing)
  for ( i in seq_along(miles_seq) ) {
    from <- state_ids[paste0("B-D-", miles_seq[i])]
    transition_matrix[from, from] <- 1
  }
  
  return( list("transition_matrix" = transition_matrix, "states" = state_names ) )
  #new("markovchain", states = state_names, transitionMatrix = transition_matrix, name = "part_state")
}

#' Returns the state probabilities after the specified number of steps
#' @param number of transition steps
compute_state_probabilities <- function(mc, steps) {
  num_states <- length( mc[["states"]] )
  initial_state <- rep(0, num_states)
  initial_state[1] <- 1
  probs <- initial_state %*% ( mc[["transition_matrix"]] %^% steps )
  good_states_idx <- grep("G-", mc[["states"]])
  good_states <- mc[["states"]][good_states_idx]
  P_start <- data.frame(state="start", miles_driven = 0, quality = "UNKNOWN", detection = "NOT_DETECTED", prob = probs[1])
  P_good <- data.frame(state = good_states,
                       miles_driven = as.numeric(gsub("G-","", good_states)),
                       quality = "GOOD",
                       detection = "NOT_DETECTED",
                       prob = probs[good_states_idx])
  bad_nd_states_idx <- grep("B-ND-", mc[["states"]])
  bad_nd_states <- mc[["states"]][bad_nd_states_idx]
  P_bad_nd <- data.frame(state = bad_nd_states,
                         miles_driven = as.numeric(gsub("B-ND-", "", bad_nd_states)),
                         quality = "BAD",
                         detection = "NOT_DETECTED",
                         prob = probs[bad_nd_states_idx]
                         )
  bad_d_states_idx <- grep("B-D-", mc[["states"]])
  bad_d_states <- mc[["states"]][bad_d_states_idx]
  P_bad_d <- data.frame(state = bad_d_states,
                        miles_driven = as.numeric(gsub("B-D-", "", bad_d_states)),
                        quality = "BAD",
                        detection = "DETECTED",
                        prob = probs[bad_d_states_idx]
                        )
  P <- rbind(P_start, P_good) %>% rbind(P_bad_nd) %>% rbind(P_bad_d)
  return(P)
}

find_detection_probability <- function(mc, steps){
  P <- compute_state_probabilities(mc, steps) 
  sum(P[!is.na(P$detection) & P$detection == "DETECTED", "prob"])
}

#' Determines the rate of incidence corresponding to the given detection probability
#' @detection_prob The probability that a bar is bad and has been detected
#' @steps Number of transition steps
#' @param max_miles The maximum number of miles to consider in the state space
#' @param bucket_miles Determines the size of the grid in the state space
#' @param time_step The length in time units of a time step in the Markov chain
#' @param ave_speed Average miles per time unit driven by the truck
#' @param mean_mtf Mean miles to failure of a part
#' @param min_mtf Minimum miles at which a failure can occur
#' @param start_to_service The mean time for a part to get into service (includes time before production)
#'
#' @returns incidence_rate The probability that a part is defective
find_incidence_rate <- function(detection_prob, steps, max_miles, bucket_miles, time_step, 
                                ave_speed, mean_mtf, min_mtf, start_to_service) {
  ir <- uniroot(f = function(x){
    mc <- get_markov_model(max_miles = max_miles, bucket_miles = bucket_miles, time_step = time_step, ave_speed = ave_speed,
                           mean_mtf = mean_mtf, min_mtf = min_mtf, start_to_service = start_to_service, incidence_rate = x)
    p <- find_detection_probability(mc, steps)
    p - detection_prob
  }, c(0,1.0), tol = 1e-12)
  ir
}

