#Biderman & Mudrik, 2018: 1-up 1-down objective staircase procedure
#Final threshold: second-lowest ISI


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
                                reversals=c()
                                )
  
  
  # Calibration settings
  repetition_number <- 1         # Number of repetitions
  max_trials <- 72               # Number of trials 
  min_ISI <- 0                   # Minimum allowed ISI
  max_ISI <- 200                 # Maximum allowed ISI
  step <- 15                     #  step size
  initial_ISI <- 200             # Starting ISI
  down_ratio <- -1               # Direction change for correct response
  up_ratio <- 1                  # Direction change for incorrect response
  
  max_trials <- 72              # Number of trials 
  threshold_calculation_method <- "Biderman & Mudrik, 2018"

  # Extract psychometric parameters for this participant
  uc_threshold <- different_participents_functions[[s_ind]]$uc_threshold
  alpha <- different_participents_functions[[s_ind]]$alpha
  beta <- different_participents_functions[[s_ind]]$beta
  guess <- 0.5 #2AFC
  
  thresholds <- numeric(repetition_number)
  
  # define the starting point configuration for each subject
  initial_sim_conf <- list(initial_ISI = initial_ISI,
                           first_correct = 1)
  
  for(calibration in 1:repetition_number) {
    
    # Initialize repetition-specific variables
    trial <- 1
    ISI <- initial_sim_conf$initial_ISI
    ISI_list <- c(ISI)  # vector to hold ISI values for each trial
    min_ISI_obs <- ISI
    reversals <- 0
    
    last_adj_dir<-NA
    
    while (trial <=  max_trials) {  
      # according to the psychometric function, find the true proportion correct for the participant
      subj_p_correct <- psy_f_new(x = ISI, alpha=alpha, beta=beta, guess=guess)
      
      # sample correctness according to the true proportion correct
      correct <- rbinom(1, 1, subj_p_correct) 
      
      # Compute adjustment direction
      adj_dir <- ifelse(correct, -1, 1)
      reversals <- 0
      
      # Check for reversal
      if (!is.na(last_adj_dir) && adj_dir != last_adj_dir) {
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
                          reversals=reversals
      )
      
      full_res_df_sub <- rbind(full_res_df_sub, cur_sim_res)
      
      # Adjust ISI
      ISI <- ISI + (adj_dir * step)
      ISI  <- min(max_ISI, ISI)
      ISI  <- max(min_ISI, ISI)
      ISI_list <- c(ISI_list, ISI)
      
      # Update tracking variables
      trial <- trial + 1
      min_ISI_obs <- min(min_ISI_obs, ISI)
    }
    
    if (threshold_calculation_method == "Biderman & Mudrik, 2018") {
      unique_sorted_numbers <- sort(unique(ISI_list))
    } else if (threshold_calculation_method == "Tal & Mudrik, 2024") {
      last_40_ISI_list <- tail(ISI_list, 40) 
      unique_sorted_numbers <- sort(unique(last_40_ISI_list))
    } else {
      stop(paste("Unsupported threshold_calculation_method:", threshold_calculation_method))
   }

    second_lowest_ISI <- unique_sorted_numbers[2]
    thresholds[calibration] = second_lowest_ISI
    
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

