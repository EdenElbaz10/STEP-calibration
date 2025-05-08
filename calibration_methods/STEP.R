# STEP calibration method by Elbaz et al., 2025 

library(dplyr)
library(binom)
library(foreach)
library(doParallel)


run_single_calibration <- function(s_ind,
                                   different_participents_functions) {

  # Detailed trial-level results (saved and returned)
  full_res_df_sub <- data.frame(subj=c(), 
                                trial=c(), 
                                ISI=c(), 
                                true_p_correct=c(),
                                trial_correct=c(),
                                repetition=c(),
                                step_size=c(),
                                real_threshold=c(),
                                max_isi=c(),
                                cumulative_correct=c(),
                                cumulative_trials=c(),
                                CWIR=c(),
                                correct_seen=c())
  

 # Data frame tracking trial counts and correct responses at each ISI
  binom_res_df_sub <- data.frame(ISI=integer(), 
                                  num_of_trials=integer(),
                                  num_of_correct=integer(), 
                                  num_of_seen=integer())
  

  Subjective_test <- 0           # If 1, include PAS-based adjustments; if 0, run Objective STEP only
  repetition_number <- 2         # Number of calibration runs (repetitions) per participant
  max_CWIR <- 50                 # Stopping criterion: maximum Cumulative Weighted Incorrect Responses
  min_number_of_trials <- 50     # Minimum number of trials per calibration run

  initial_step <- 32           # Initial step size 
  max_step <- 32               # Maximum step size allowed
  min_step <- 4                # Minimum step size allowed
  initial_ISI <- 200           # Starting ISI for the calibration
  min_ISI <- 0                 # Minimal ISI values
  down_ratio <- -1             # Step direction for correct response (decrease ISI)
  up_ratio <- 1                # Step direction for incorrect response (increase ISI)
    
  thresholds <- numeric(repetition_number) # Vector of estimated thresholds for each repetition

  # Objective parameters
  uc_threshold <- different_participents_functions[[s_ind]]$uc_threshold
  alpha <- different_participents_functions[[s_ind]]$alpha
  beta <- different_participents_functions[[s_ind]]$beta
  guess <- 0.5

  # Subjective parameters
  seen_a <- different_participents_functions[[s_ind]]$seen_a
  seen_b <- different_participents_functions[[s_ind]]$seen_b
  
  # define the starting point configuration for each participant
  initial_sim_conf <- list(initial_ISI=initial_ISI,
                           initial_step=initial_step, 
                           first_correct=1)
  
  
  for(calibration in 1:repetition_number) {
    # initialize the variables for the next repetition
    binom_res_df_sub <- data.frame( ISI=integer(),
                                    num_of_trials=integer(),
                                    num_of_correct=integer(),
                                    num_of_seen=integer() )
    

    

    max_ISI <- 200   # Upper boundery for ISI values (adjusted during run)
    
    trial <- 1
    ISI <- initial_sim_conf$initial_ISI      # Current ISI
    step <- initial_sim_conf$initial_step    # Current step size 
    ISI_list <- c(ISI)  # Vector to hold ISI values for each trial
    min_ISI_obs <- initial_sim_conf$initial_ISI
    adj <-1   # Adjustment factor based on correctness

    # Initialize lists to keep track of trials and correct responses at each ISI
    trials_at_ISI <- list()   
    correct_at_ISI <- list() 

    cumulative_correct <-0 # Number of correct responses for trials with ISI values within the current ISI and the current boundary  
    cumulative_seen <- 0   # Total number of trials marked as 'seen' with ISI values within the current ISI and the current boundary 
    cumulative_trials <-0  # Number of trials with ISI values within the current ISI and the current boundary  
    
    num_of_CWIR <- 0 # Number of Cumulative Weighted Incorrect Responses (CWIR)
    CWIR <- 0        # Latest CWIR value
    CWIR_list <- c() # History of CWIR weights used to compute final threshold
    
    w_list<- c()    # List of ISIs for weighting during error sequences
    num_incorrect_seq <- 0   # Number of incorrect trials in a row
    num_incorrect_seq_to <- 0  # Total count of errors during an error sequence

    # stop if reached to the predifined number of CWIR and to the minimal number of trials
    while (num_of_CWIR < max_CWIR || trial <= min_number_of_trials ) {

      # according to the psychometric function, find the true proportion correct for the participant
      subj_p_correct <- psy_f_new(x=ISI, alpha=alpha, beta=beta, guess=guess)
      subj_p_seen <- seen_psy_f_new(x=ISI, seen_a=seen_a, seen_b=seen_b)
      
      # sample correctness according to the true proportion correct
      correct <- rbinom(1, 1, subj_p_correct) #what are the chances to correct i =n one trial out of one trial
      correct_seen <- rbinom(1, 1, subj_p_seen) #what are the chances to correct i =n one trial out of one trial

      
      # Update binom_res_df_sub
      if (!(ISI %in% binom_res_df_sub$ISI)) {
          binom_res_df_sub <- rbind(binom_res_df_sub, data.frame(ISI=ISI,
                                                                 num_of_trials=1, 
                                                                 num_of_correct=correct, 
                                                                 num_of_seen=correct_seen))
        } else {
          for (i in 1:nrow(binom_res_df_sub)) {
            if (binom_res_df_sub$ISI[i] == ISI) {
              binom_res_df_sub$num_of_trials[i] <- binom_res_df_sub$num_of_trials[i] + 1
              binom_res_df_sub$num_of_correct[i] <- binom_res_df_sub$num_of_correct[i] + correct
              binom_res_df_sub$num_of_seen[i] <- binom_res_df_sub$num_of_seen[i] + correct_seen
              
              break
            }
          }
        }
        
        
        # Calculate the cumulative correct ratio for the current and all higher ISIs
        relevant_ISI_data <- binom_res_df_sub[binom_res_df_sub$ISI >= ISI & binom_res_df_sub$ISI <= max_ISI, ]
        
        cumulative_correct <- sum(relevant_ISI_data$num_of_correct)
        cumulative_seen <- sum(relevant_ISI_data$num_of_seen)
        cumulative_trials <- sum(relevant_ISI_data$num_of_trials)
        
        if (cumulative_trials>=0){
          # Check that there are at least x trials before calculating the correct ratio
          correct_ratio <- cumulative_correct / cumulative_trials
          seen_ratio <- cumulative_seen / cumulative_trials
          binom_result <- binom.test(cumulative_correct,cumulative_trials,  p=0.5,  alternative="greater")
          
          if (binom_result$p.value <=  0.05/(num_of_CWIR+1)) {
            if (Subjective_test==1){
              max_ISI <- ((1-seen_ratio) *max_ISI) +(seen_ratio *ISI)
            }else {
              max_ISI <- (max_ISI+ISI)/2
            }
          }
        } 
        
      # Determine the adjustment direction 
      adj_dir <- ifelse(correct, -1, 1)
      
        if (adj_dir==-1) {
          if (length(w_list)!=0){
            w_last <- mean(w_list)
            for (kk in 1:num_incorrect_seq_to) {
              CWIR_list <- c(CWIR_list, w_last)
            }
          }
          
          num_incorrect_seq_to <- 0
          num_incorrect_seq <- 0
          w_list <- c()
        }
        else{
          num_incorrect_seq <- num_incorrect_seq+1
          w_list <- c(w_list, ISI)
          num_incorrect_seq_to <- num_incorrect_seq_to +num_incorrect_seq
          num_of_CWIR <- num_of_CWIR+num_incorrect_seq
          
          if (num_of_CWIR>= max_CWIR){
            w_last <- mean(w_list)
            
            for (kk in 1:num_incorrect_seq_to) {
              CWIR_list <- c(CWIR_list, w_last)
            }
          }
        }

      # depending on accuracy, set ISI adjustment direction 
      adj <- ifelse(correct, down_ratio, up_ratio)
      
      if (Subjective_test==1 && step!=min_step && correct==0 && correct_seen==1){
        seen_adj <- 0
      }else{
        seen_adj <- 1
      }
      
      # save results per trial
      cur_sim_res <- list(subj=s_ind, trial=trial, ISI=ISI,
                          true_p_correct=subj_p_correct,
                          trial_correct=correct,
                          repetition=calibration,
                          step_size=step*adj,
                          real_threshold=uc_threshold,
                          max_isi=max_ISI,
                          cumulative_correct=cumulative_correct,
                          cumulative_trials=cumulative_trials,
                          CWIR=num_of_CWIR,
                          correct_seen=correct_seen)
      
      ISI <- ISI + (adj_dir * step*seen_adj)
      
      # keep ISI within borders
      ISI <- min(max_ISI, ISI)
      ISI <- max(min_ISI, ISI)
      ISI_list <- c(ISI_list, ISI)  # append ISI to ISI_list
      
      trial <- trial + 1
      step <- step - min_step
      step <- max(min_step, step)
      
      min_ISI_obs <- min(min_ISI_obs, ISI)
      full_res_df_sub <- rbind(full_res_df_sub, cur_sim_res)
    }

      thresholds[calibration]<- mean(CWIR_list) 
    
  }
  
  
  mean_threshold <- mean(thresholds)
  
  cur_sim_sum <- list(subj=s_ind, 
                      real_threshold=uc_threshold,
                      alpha=alpha, 
                      beta=beta,
                      mean_threshold=mean_threshold,
                      lowest_ISI=min(min_ISI_obs),
                      end_trial=trial)
  
  return (list(cur_sim_sum=cur_sim_sum, 
               full_res_df_sub=full_res_df_sub))

  }


