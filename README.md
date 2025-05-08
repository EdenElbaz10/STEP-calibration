# The Subliminal Threshold Estimation Procedure (STEP): A calibration method tailored for successfully estimating subliminal thresholds

This repository contains all code used in the simulation and model-fitting analyses described in our paper:  
*The Subliminal Threshold Estimation Procedure (STEP): A calibration method tailored for successfully estimating subliminal thresholds*.

In this work, we present a novel calibration procedure: The **S**ubliminal **T**hreshold **E**stimation **P**rocedure (STEP),  specifically designed for estimating the individual upper subliminal threshold.


## Structure
- `psychometric_functions/`: Bayesian hierarchical model fitting for subjective and objective psychometric functions.
    - Refer to the folders' `README.md` for additional information about the method.
- `calibration_methods_UC_papers/`: Simulations of calibration methods reported in past unconscious processing studies.

- `calibration_methods/`: Implementation of common calibration procedures (Staircaese, PEST, QUEST, QUEST+,and ASA) and the STEP method.
    - Refer to the folders' `README.md` for additional information about reproducig the results
- `calibration_simulation/`: Main calibration runner that wraps each method and evaluates estimation performance.

## Usage
**Requirements:**
- R ≥ 4.2
- Python ≥ 3.8  

**Running the simulations:**
1. Clone the repository 
2. Navigate to `calibration_simulation\main_calibration.r` script and modify the following:
   - Set `BASE_PROJECT_PATH` to the full path of the repository on your machine
   - Set `METHOD_NAME <-  "STEP"` (or any other method listed in the script).
3. Run the script:
```r
source("calibration_simulation/main_calibration.r")
```

## Contact
For questions or comments, contact the corresponding author: edenelbaz1@mail.tau.ac.il 
