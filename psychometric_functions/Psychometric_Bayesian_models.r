# Bayesian Hierarchical Psychometric Fitting
# This script fits objective and subjective psychometric functions using Bayesian hierarchical approach 

library(brms)
library(rstan)
library(bayesplot)
library(ggplot2)
library(dplyr)

# ------------------------------
# Objective function
# ------------------------------

# Load objective performance data
data=read.csv("all_grouped_data.csv")
data$participant_id <- as.factor(data$participant_id)


#Non-linear formula:
formula <- bf(
  r | trials(m)    ~
    (0.5 + (0.48 * (1 - exp(-((ISI - b1) / b2))) * (ISI > b1))),
  nl = TRUE,
  b1 +b2 ~ 1 +(1|participant_id))  

# Set priors for threshold (b1) and slope (b2)
##Both normally distributed with mean=25 and SD=5, constrained to positive values
priors_obj <- c(
  prior(normal(25, 5), nlpar = "b1", lb = 0),
  prior(normal(25, 5), nlpar = "b2", lb = 0)
)

# Fit the model
objective_model <- brm(
  formula = formula,
  family = binomial("identity"),
  prior = priors_obj,
  data = data,
  chains = 4,
  cores = 4,
  seed = 1234,
  warmup = 1000, 
  iter = 2000,
  control = list(adapt_delta = 0.95)
)

# Save model and output summary
save(objective_model, file = "Objective_Bayesian_hierarchical_fitting.rdata")
summary(objective_model)

# Plot model diagnostics
plot(objective_model)
pp_check(objective_model)

# Extract random effects (posterior means per participant)
random_effects <- coef(objective_model)

ranef_list <- coef(objective_model)$participant_id  
ranef_estimates <- ranef_list[, ,"b1_Intercept"]
ranef_estimates2 <- ranef_list[, ,"b2_Intercept"]

# Convert the matrix to a data frame and add the participant ids as a column
objective_ranef_df <- data.frame(
  participant_id = rownames(ranef_estimates),
  b1 = ranef_estimates[,"Estimate" ],
  b2 = ranef_estimates2[,"Estimate" ]
)

write.csv(objective_ranef_df, "random_effects_objective.csv", row.names = FALSE)

# ------------------------------
# Subjective function (Logistic)
# ------------------------------

# Load PAS data
subjective_data=read.csv("all_grouped_data_pas.csv")
subjective_data$participant_id <- as.factor(subjective_data$participant_id)

# Logistic formula
formula <- bf(
  r | trials(m)    ~
  1/(1 + exp((b1-ISI)/b2)),
  nl = TRUE,
  b1 +b2 ~ 1 +(1|participant_id))  


# Set priors for threshold (b1) and slope (b2)
priors <- c(
  prior(normal(25, 5), nlpar = "b1", lb=0), 
  prior(normal(25, 5), nlpar = "b2", lb=0)) 

subjective_model <- brm(
  formula = formula,
  family = binomial("identity"),
  prior = priors,
  data = subjective_data,
  chains = 4, 
  cores = 4,
  seed = 1234,
  warmup = 1000, 
  iter = 2000,
  control = list(adapt_delta = 0.95)
)

save(subjective_model, file = "Bayesian_hierarchical_fitting_subjective.rdata")
summary(subjective_model)
plot(subjective_model)

# Extract random effects
random_effects <- coef(subjective_model)

ranef_list <- coef(subjective_model)$participant_id  
ranef_estimates <- ranef_list[, , "b1_Intercept"]
ranef_estimates2 <- ranef_list[, ,"b2_Intercept"]

# Convert the matrix to a data frame and add the participant ids as a column
subjective_ranef_df <- data.frame(
  participant_id = rownames(ranef_estimates),
  seen_a = ranef_estimates[,"Estimate" ],
  seen_b = ranef_estimates2[,"Estimate" ]
)

write.csv(subjective_ranef_df, "random_effects_PAS.csv", row.names = FALSE)

# Merge both data frames by participant_id
combined_df <- merge(objective_ranef_df, subjective_ranef_df, by = "participant_id")
write.csv(combined_df, "participant_parameters.csv", row.names = FALSE)
