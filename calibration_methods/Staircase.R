# 1-up 1-down objective staircase procedure (Levitt, 1971)

library(dplyr)
library(binom)
library(foreach)
library(doParallel)



run_single_calibration <- function(s_ind,
                                   different_participents_functions) {
  
  full_res_df_sub <- data.frame(subj = c(), 
                                trial = c(), 
                                ISI = c(), 
                                true_p_correct = c(),
                                trial_correct = c(),
                                repetition=c(),
                                step_size=c(),
                                real_threshold=c(),
                                reversals=c(),
                                num_of_reversals=c())
  

  # Calibration settings
  repetition_number <- 2         # Number of repetitions
  max_reversals <- 35            # Stop after reaching this many reversals
  min_ISI <- 0                   # Minimum allowed ISI
  max_ISI <- 200                 # Maximum allowed ISI
  min_step <- 4                  # Minimum allowed step size
  initial_step <- 32             # Initial step size
  initial_ISI <- 200             # Starting ISI
  down_ratio <- -1               # Direction change for correct response
  up_ratio <- 1                  # Direction change for incorrect response
  
  
  # Extract psychometric parameters for this participant
  uc_threshold <- different_participents_functions[[s_ind]]$uc_threshold
  alpha <- different_participents_functions[[s_ind]]$alpha
  beta <- different_participents_functions[[s_ind]]$beta
  guess <- 0.5 #2AFC
  
  thresholds <- numeric(repetition_number)
  
  
  # define the starting point configuration for each subject
  initial_sim_conf <- list(initial_ISI = initial_ISI,
                           initial_step = initial_step, 
                           first_correct = 1)
  
  for(calibration in 1:repetition_number) {
    
    # Initialize repetition-specific variables
    num_of_reversals <- 0
    trial <- 1
    ISI <- initial_sim_conf$initial_ISI
    step <- initial_sim_conf$initial_step
    ISI_list <- c(ISI)
    min_ISI_obs <- ISI
    reversals <- 0
    
    # Initialize a variable to keep track of the last adjustment direction
    step <- initial_step
    last_adj_dir <- NA

    reversals_ISI_list <- c()
    
    while (num_of_reversals < max_reversals) {
      # according to the psychometric function, find the true proportion correct for the participant
      subj_p_correct <- psy_f_new(x = ISI, alpha=alpha, beta=beta, guess=guess)

      # sample correctness according to the true proportion correct
      correct <- rbinom(1, 1, subj_p_correct) 
    
      # Compute adjustment direction
      adj_dir <- ifelse(correct, -1, 1)
      reversals <- 0
      
      # Check for reversal
      if (!is.na(last_adj_dir) && adj_dir != last_adj_dir) {
        num_of_reversals <-num_of_reversals+1 
        reversals_ISI_list <- c(reversals_ISI_list, ISI)
        reversals <-1
      }
      
      
      # save results per trial
      cur_sim_res <- list(subj = s_ind, 
                          trial = trial, 
                          ISI = ISI,
                          true_p_correct = subj_p_correct,
                          trial_correct = correct,
                          repetition= calibration,
                          step_size=step*adj_dir,
                          real_threshold=uc_threshold,
                          reversals=reversals,
                          num_of_reversals = num_of_reversals
                          )
      
      full_res_df_sub <- rbind(full_res_df_sub, cur_sim_res)
      
      
      # Adjust ISI
      ISI <- ISI + (adj_dir * step)
      ISI  <- min(max_ISI, ISI)
      ISI  <- max(min_ISI, ISI)
      ISI_list <- c(ISI_list, ISI)

      # Decrease step size but not below minimum
      step <- max(min_step, step - min_step)

      # Update tracking variables
      trial <- trial + 1
      min_ISI_obs <- min(min_ISI_obs, ISI)
      last_adj_dir <- adj_dir
      
    }
    
    # Store threshold estimate for this repetition
    thresholds[calibration]<- mean(tail(reversals_ISI_list,33)) 
    
  }
  
  # Final threshold is the average across repetitions
  mean_threshold <- mean(thresholds)
  
  # Summary results
  cur_sim_sum <- list(subj = s_ind, 
                      real_threshold=uc_threshold,
                      alpha=alpha, 
                      beta=beta,
                      mean_threshold = mean_threshold,
                      lowest_ISI = min(min_ISI_obs),
                      end_trial = trial)
  
  
  return (list(cur_sim_sum = cur_sim_sum, full_res_df_sub = full_res_df_sub))
}
