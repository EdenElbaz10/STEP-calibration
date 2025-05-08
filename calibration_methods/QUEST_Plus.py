import numpy as np
import pandas as pd
import questplus as qp # Note! the package was modified locally as described in the folders' README.md
from multiprocessing import Pool, cpu_count

# --- CONFIG ---
N_REPEATS = 250       # Number of repeats per participant
N_TRIALS = 156        # Trials per QUEST+ run
N_CORES = cpu_count()  -2
RESULT_PATH = "quest_plus_sum.csv"
PARAM_CSV = "psychometric_functions/participant_parameters.csv"
np.random.seed(1)

# --- Load participant parameters once ---
participant_params_df = pd.read_csv(PARAM_CSV)
participants = participant_params_df.to_dict(orient='records')

# --- Psychometric function (same as before) ---
def psychometric_function(intensity, threshold, real_slope, lapse_rate=0.02, lower_asymptote=0.5):
    if intensity <= threshold:
        return lower_asymptote
    else:
        return lower_asymptote + (1 - lower_asymptote - lapse_rate) * (1 - np.exp(-((intensity - threshold) / real_slope)))
    
# --- Define stimulus & parameter domains  ---
stim_domain = dict(intensity=np.linspace(0, 150, 100))  # 100 stimulus levels

param_domain = dict(
    threshold=np.linspace(0, 150 , 100),                 # 100 thresholds
    slope=np.linspace(1, 150, 100) ,                   # 100 slopes
    lower_asymptote=0.5,
   lapse_rate=0.02
)

# --- Main simulation function for one repeat of one participant ---
def simulate_one_task(args):
    repeat_idx, participant = args
    real_threshold = participant['alpha']
    real_slope = participant['beta']
    pid = participant['subject_num']

    q = qp.QuestPlus(
        stim_domain=stim_domain,
        func='weibull',
        stim_scale='linear',
        param_domain=param_domain,
        outcome_domain=dict(response=['Correct', 'Incorrect']),
        stim_selection_method='min_entropy',
        param_estimation_method='mean'
    )

    for _ in range(N_TRIALS):
        next_stim = q.next_stim
        intensity = next_stim['intensity']
        p_correct = psychometric_function(intensity, real_threshold, real_slope,  lapse_rate=0.02, lower_asymptote=0.5)
        response = 'Correct' if np.random.rand() < p_correct else 'Incorrect'
        q.update(stim=next_stim, outcome=dict(response=response))


    est = q.param_estimate
    return {
        'participant': pid,
        'repeat': repeat_idx,
        'real_threshold': real_threshold,
        'mean_threshold': est['threshold'],
        'beta': real_slope,
        'estimated_slope': est['slope']
    }

# --- Prepare all jobs: each repeat of each participant ---
jobs = [(r, p) for r in range(N_REPEATS) for p in participants]

# --- Run in parallel ---
if __name__ == "__main__":
    import time
    start = time.time()
    print(f"Running {len(jobs)} simulations on {N_CORES} cores...")

    with Pool(N_CORES) as pool:
        results = pool.map(simulate_one_task, jobs) 

    # Save to CSV
    df = pd.DataFrame(results)

    # Add subj column: 
    df['subj'] = range(1, len(df) + 1)

    df.to_csv(RESULT_PATH, index=False)

    print(f"Done in {time.time() - start:.2f} seconds. Saved to: {RESULT_PATH}")

