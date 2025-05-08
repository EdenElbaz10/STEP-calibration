# Psychometric Function Fitting (Bayesian Models)

This folder contains the data and R script used to fit Bayesian hierarchical psychometric functions to both objective (2AFC performence)and subjective (PAS ratings) results collected using the Method of Constant Stimuli (MOCS; Laming & Laming, 1992).

## Contents
- `all_grouped_data.csv`: objective accuracy data across different ISIs for 40 participants.
- `all_grouped_data_pas.csv`: subjective visibility responses (PAS ratings) for the same 40 participants across ISIs.
- `Psychometric_Bayesian_models.R`: A script that fits separate non-linear hierarchical models for the objective and subjective data using `brms` package.
- `participant_parameters.csv`: Output file containing participant-level parameter estimates:  
  - `b1`, `b2` (objective function)  
  - `seen_a`, `seen_b` (subjective function)  

## Objective Psychometric Function

The objective function models the proportion of correct responses as a function of ISI:

$$
f(ISI; b_1, b_2) =
\begin{cases}
0.5 + (0.5 - \lambda) \cdot \left(1 - e^{- \frac{ISI - b_1}{b_2}} \right), & \text{if } ISI > b_1 \\\\
0.5, & \text{if } ISI \leq b_1
\end{cases}
$$

Where:
- `b1` is the ISI upper subliminal threshold 
- `b2` denotes the rate of performance improvement after the threshold is passed 
- `λ = 0.02` is a fixed lapse rate

## Subjective Psychometric Function

The subjective function models visibility report as a logistic function of ISI:

$$
f(ISI; \text{seen\_a}, \text{seen\_b}) =
\frac{1}{1 + e^{\frac{\text{seen\_a} - ISI}{\text{seen\_b}}}}
$$

Where:
- `seen_a` is the ISI midpoint of the logistic function 
- `seen_b` determines the spread or steepness of the curve  

## Usage

To run the analysis, open `Psychometric_Bayesian_models.R` in R and execute the script. Ensure that the required packages (`brms`, `rstan`, `bayesplot`) are installed.

The output file `participant_parameters.csv` will contain four fitted parameters per participant
