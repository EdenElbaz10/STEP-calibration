# --- Calibration Simulation Script ---
# Author: Eden Elbaz
# Description: Simulation of different threshold calibration methods for subliminal perception.
# This script runs a full calibration simulation using participant data and generates summary results.


library(dplyr) 
library(binom)
library(foreach)
library(doParallel)

# USER INPUTS

BASE_PROJECT_PATH <- "<full-path-to-project-folder>"
METHOD_NAME <-  "STEP" # Choose your desired method from either the UC methods or standard methods below

UC_PAPERS_METHODS <- c(
  "Hung et al., 2023", 
  "Peel et al., 2018",
  "Biderman & Mudrik, 2018",
  "Bernstein et al., 1989",
  "Rothkirch & Hesselmann, 2018"
  # "Handschack et al., 2022", # See associated markdown
  # "Tal & Mudrik, 2024", # See associated markdown
  # To run `Kiepe & Hesselmann, 2024`, See its associated markdown under "calibration_methods_UC_papers" folder

)
STANDARD_METHODS <- c(
  "STEP", 
  "ASA", 
  "PEST", 
  "Staircase"
  # Note:
  # - To run `Objective STEP`, See its associated markdown under "calibration_methods" folder
  # - For `QUEST` and `QUEST_Plus` you should run their python scripts
)

# ----------------------
# Psychometric Functions
# ----------------------

# Objective psychometric function
psy_f_new <- function(x, alpha, beta, guess) {
  if (x <= alpha){
    return (0.5)
  }
  else{
    return (0.5+ 0.48*(1 - exp(-(x - alpha)/beta)))
  }
}



# Subjective psychometric function
seen_psy_f_new <- function(x, seen_a, seen_b) {
  return (1/(1 + exp((seen_a - x)/seen_b)))
}


# ------------------------------
# Load participant parameters
# ------------------------------

get_participents_data <- function(participent_csv_path) {
  
  participents_data <- read.csv(participent_csv_path)
  participents_data$participnt_num <- as.character(participents_data$subject_num)
  
  participents <- list()
  for (i in 1:nrow(participents_data)){
    row<-participents_data[i,]
    key<-as.character(row$subject_num)
    
    participents[[key]]<-list(alpha=row$alpha,
                              beta=row$beta,
                              uc_threshold=row$alpha, 
                              guess=row$guess,
                              seen_a=row$seen_a,
                              seen_b=row$seen_b)
  }
  return (participents)
} 


# ---------------------------
# Run Calibration Simulation
# ---------------------------

run_calibration_task <- function() {
  seed <- 1
  chance <- .5 
  n_subjects <- 10000
  
  # Load participant parameter sets
  participent_csv_path <- file.path(BASE_PROJECT_PATH, "psychometric_functions/participant_parameters.csv")

  
  participents <- get_participents_data(participent_csv_path=participent_csv_path)
  different_participents_functions <- vector('list',n_subjects)
  
  for (i in 1:n_subjects){
    participents_index <- (i-1)%%length(participents) +1
    obs_key <- names(participents)[participents_index]
    different_participents_functions[[i]]=participents[[obs_key]]
  }
  
  
  # Initialize result data frames
  
  # define the results data frames (a summary per participant and trial-by-trial results)
  sum_results_df <- data.frame(subj=c(), 
                               real_threshold=c(),
                               alpha=c(), 
                               beta=c(), 
                               threshold = c(), 
                               lowest_ISI = c(),
                               end_trial = c())
  
  full_res_df <- data.frame(subj = c(), 
                            trial = c(), 
                            ISI = c(), 
                            true_p_correct = c(),
                            trial_correct = c(),
                            repetition=c(),
                            step_size=c(),
                            real_threshold=c(),
                            reversals=c(),
                            max_isi=c(),
                            cumulative_correct=c(),
                            cumulative_trials=c(),
                            correct_seen=c())
  
  
  # Set up parallel processing
  cl <- makeCluster(detectCores() - 1)
  
  registerDoParallel(cl)
  
  # start simulation
  
  on.exit({
    stopCluster(cl)
    foreach::registerDoSEQ()
    gc()
  })
  
  set.seed(seed)
  
  # Run calibrations
  results <- foreach(s_ind = seq_len(n_subjects)) %do% {
    run_single_calibration(s_ind,different_participents_functions)
  }
  
  
  # Aggregate results
  for (i in seq_along(results)) {
    result_i <- results[[i]]
    sum_results_df <- rbind(sum_results_df, result_i$cur_sim_sum)
    full_res_df <- rbind(full_res_df, result_i$full_res_df_sub)
  }
  
  return(list(sum_results_df = sum_results_df, full_res_df = full_res_df))
}


# -----------------------
# Main execution section
# -----------------------

if (METHOD_NAME %in% UC_PAPERS_METHODS) {
  method_dir <- file.path(BASE_PROJECT_PATH, "calibration_methods_UC_papers")
} else if (METHOD_NAME %in% STANDARD_METHODS) {
  method_dir <- file.path(BASE_PROJECT_PATH, "calibration_methods")
} else {
  stop("Unknown method name: please check the spelling.")
}

output_dir <- file.path(BASE_PROJECT_PATH, "calibration_simulation")

# --- Source the method file dynamically ---
method_path <- file.path(method_dir, paste0(METHOD_NAME, ".R"))
source(method_path)

# --- Run simulation ---
results <- run_calibration_task()
sum_results_df <- results$sum_results_df
full_res_df <- results$full_res_df

# --- Save results ---
write.csv(full_res_df, file.path(output_dir, paste0(METHOD_NAME, ".csv")), row.names = TRUE)
write.csv(sum_results_df, file.path(output_dir, paste0(METHOD_NAME, "_sum.csv")), row.names = TRUE)

# Calculate error metrics
estimated_thresholds <- sum_results_df$mean_threshold
real_threshold <- sum_results_df$real_threshold

sum_results_df$rel_resid <- (estimated_thresholds - real_threshold) / real_threshold
NRMSE <- sqrt(mean(sum_results_df$rel_resid^2))
RMSE <- sqrt(mean((estimated_thresholds - real_threshold)^2))
score <- mean(estimated_thresholds)

# Estimate expected performance from thresholds
pred_real <- estimated_thresholds - real_threshold
resultss <- sapply(seq_along(estimated_thresholds), function(i) {
  psy_f_new(estimated_thresholds[i],
            alpha = sum_results_df$alpha[i],
            beta = sum_results_df$beta[i],
            guess = 0.5)
})

PRMSE <- sqrt(mean((resultss - 0.5)^2))

# Calculate the RMSE of the relative residuals
cat("RMSE:", RMSE, "\n")
cat("NRMSE:", NRMSE, "\n")
cat("PRMSE:", PRMSE, "\n")
