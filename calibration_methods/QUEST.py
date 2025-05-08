
import numpy as np
import pandas as pd
from psychopy.data import QuestHandler
from multiprocessing import Pool, cpu_count

# CONFIGURATION
PARAM_CSV = "psychometric_functions/participant_parameters.csv" # Input CSV with participant parameters
RESULT_PATH = "QUEST_sum.csv"              # Output CSV path
N_TRIALS = 156                             # Number of trials per QUEST run
N_REPEATS = 250                            # Repeats per participant
N_CORES = max(cpu_count() - 2, 1)          # Number of CPU cores to use
np.random.seed(1)

# Load participant parameters
df = pd.read_csv(PARAM_CSV)
participants = df.to_dict(orient='records')

# Psychometric function
def psychometric_function(intensity_log, threshold, slope, gamma=0.5, delta=0.02):
    """
    Computes the probability of a correct response at a given stimulus ISI.
    """
    intensity = 10 ** intensity_log
    if intensity <= threshold:
        return gamma
    else:
        return gamma + (1 - gamma - delta) * (1 - np.exp(-(intensity - threshold) / slope))
    
# Simulate one QUEST run
def simulate_quest(args):
    repeat_idx, participant = args
    pid = participant['subject_num']
    true_threshold = participant['alpha']
    real_slope = participant['beta']


    gamma = 0.5   # chance level for 2AFC
    delta = 0.02  # lapse rate

    # Create QUEST handler
    quest = QuestHandler(
        startVal=np.log10(100),  # Centered at 100 (midpoint of 0–200)
        startValSd=1,                
        pThreshold=0.51,  # Slightly above chance to make sure QUEST works correctly
        gamma=gamma,
        delta=0.02,
        beta=3.5,
        nTrials=N_TRIALS, 
        minVal=np.log10(1),    # Lower bound: 1 (log10(1) = 0)
        maxVal=np.log10(200),  # Upper bound: 200 (log10(200) ≈ 2.3)
    )

    n_correct = 0

    for i, intensity_log in enumerate(quest):
        intensity_log = max(1e-6000, intensity_log)  # Avoid log/zero issues
        p_correct = psychometric_function(intensity_log, threshold=true_threshold, slope=real_slope, gamma=gamma, delta=delta)
        response = int(np.random.rand() < p_correct)
        n_correct += response
        quest.addResponse(response)
        
    # Optional: print accuracy
    accuracy = n_correct / N_TRIALS

    return {
        'participant': pid,
        'repeat': repeat_idx,
        'real_threshold': true_threshold,
        'mean_threshold': 10 ** quest.quantile(),  #Estimated threshold
        'beta': real_slope,
        'accuracy': accuracy
    }

# Build simulation jobs
jobs = [(r, p) for r in range(N_REPEATS) for p in participants]
print(f"Running {len(jobs)} QUEST simulations using {N_CORES} cores...")

# Run simulations in parallel
if __name__ == "__main__":
    import time
    start = time.time()
    
    with Pool(N_CORES) as pool:
        results = pool.map(simulate_quest, jobs)

    # Save results to CSV
    df_out = pd.DataFrame(results)

    # Add subj column: just 1, 2, 3, ...
    df_out['subj'] = range(1, len(df_out) + 1)

    df_out.to_csv(RESULT_PATH, index=False)
