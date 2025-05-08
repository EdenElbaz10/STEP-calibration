#Rothkirch & Hesselmann, 2018 : a 1-up 1-down subjective staircase procedure
#25 trials x 2 repetitions
#Highest ISI consistently judged as invisible

# For Handschack et al., 2022 change the number of repetition to 1 and the number of trials to 20 trials. 

run_single_calibration <- function(s_ind, different_participents_functions) {
  
  full_res_df_sub <- data.frame(subj = c(), 
                                trial = c(), 
                                ISI = c(), 
                                true_p_seen = c(),
                                repetition=c(),
                                step_size=c(),
                                real_threshold=c(),
                                correct_seen=c())

  
  
  # Parameters for the staircase procedure
  min_ISI <- 0             # Minimum ISI allowed
  max_ISI <- 200           # Maximum ISI allowed
  initial_ISI <- 200       # Starting ISI value
  down_ratio <- -1         # Direction for correct response (go down)
  up_ratio <- 1            # Direction for incorrect response (go up)
  
  max_trials <- 25          # Foe Handschack et al., 2022- change to 20 trials 
  repetition_number <- 2   # For Handschack et al., 2022 change to 1 repetition
  overall_trials <- repetition_number * max_trials

  # Get psychometric function parameters for this subject
  seen_a <-  different_participents_functions[[s_ind]]$seen_a
  seen_b  <-  different_participents_functions[[s_ind]]$seen_b
  
  uc_threshold <- different_participents_functions[[s_ind]]$uc_threshold
  alpha <- different_participents_functions[[s_ind]]$alpha
  beta <- different_participents_functions[[s_ind]]$beta

  thresholds <- numeric(repetition_number)
    
  # Starting configuration for the first calibration repetition
  initial_sim_conf <- list(initial_ISI = initial_ISI, first_correct = 1)
  ISI_values <- numeric(overall_trials)
  seen_responses <- numeric(overall_trials)
  
  for(calibration in 1:repetition_number) {
    # Initialize the variables for the next repetition
    trial <- 1
    max_ISI <- 200
    min_ISI_obs <- initial_sim_conf$initial_ISI
    adj  <- 1
    last_seen <- NA
    ISI_list <- c()  # vector to hold ISI values for each trial
    ISI <- initial_sim_conf$initial_ISI
    ISI_list <- c(ISI_list, ISI)  # append ISI to ISI_list

    while (trial <= max_trials ) {
      # Get probability of seeing the stimulus at current ISI
      subj_p_seen <- seen_psy_f_new(x = ISI, seen_a=seen_a, seen_b=seen_b)
      # Sample correctness according to the true proportion correct
      correct_seen <- rbinom(1, 1, subj_p_seen) 
      
      # Determine the adjustment direction 
      adj_dir <- ifelse(correct_seen, down_ratio, up_ratio) 
      step <- ifelse(adj_dir == -1, ISI - ISI * 0.7197, ISI / 0.7197 - ISI)
            
      # Adjust ISI based on response
      ISI_values[trial + (max_trials*(calibration -1))] <- ISI
      seen_responses[trial + (max_trials*(calibration -1))] <- correct_seen 
      
      # save results per trial
      cur_sim_res <- list(subj = s_ind,
                          trial = trial,
                          ISI = ISI,
                          true_p_seen = subj_p_seen,
                          repetition= calibration,
                          step_size=step*adj_dir,
                          real_threshold=uc_threshold,
                          max_isi=max_ISI,
                          correct_seen=correct_seen)
      
      step <- ifelse(adj_dir == -1, ISI - ISI * 0.7197, ISI / 0.7197 - ISI)
      ISI <- ifelse(adj_dir == -1, ISI * 0.7197, ISI / 0.7197)
      
      # Keep ISI within borders
      ISI  <-  min(max_ISI, ISI)
      ISI  <-  max(min_ISI, ISI)
      ISI_list <- c(ISI_list, ISI)
      
      
      trial <- trial + 1
      min_ISI_obs <- min(min_ISI_obs, ISI)
      last_seen <- correct_seen

      full_res_df_sub <- rbind(full_res_df_sub, cur_sim_res)
  
    }
  }
  
  
  # Find the highest ISI where all responses were unseen
  unique_ISI_list <- unique(ISI_values)
  final_threshold <- NA
  
  for (ISI_dur in rev(sort(unique_ISI_list))) {
    if (all(seen_responses[ISI_values == ISI_dur] == 0)) {
      final_threshold  <- ISI_dur
      break
    }
  }
  
  if (is.na(final_threshold)){
    final_threshold <- min_ISI  #fallback if no unseen-only ISI found
    
  }
  
  mean_threshold <- final_threshold
  

  cur_sim_sum <- list(subj = s_ind, 
                      real_threshold=uc_threshold,
                      alpha=alpha, 
                      beta=beta,
                      mean_threshold = mean_threshold,
                      lowest_ISI = min(min_ISI_obs),
                      end_trial = min(trial, max_trials)
                      )
  
  return (list(cur_sim_sum = cur_sim_sum, full_res_df_sub = full_res_df_sub))

}
