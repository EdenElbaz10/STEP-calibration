# Handschack et al., 2022 – Implementation Notes

This calibration method is equivalent to the implementation used **Rothkirch & Hesselmann, 2018**. The only differences are:
- **Number of repetitions**:  
  Set to **1**

- **Number of trials per run**:  
  Set to **20**

Because the underlying calibration logic is identical, no separate R script is provided.  


To reproduce this method
1. In `Rothkirch & Hesselmann, 2018.R` file, modify the following parameters:
```r
repetition_number <- 1
max_trials <- 20
```
2. Then, run the `main_calibration.r` script using `METHOD_NAME <- "Rothkirch & Hesselmann, 2018"` 
