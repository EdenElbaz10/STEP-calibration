# Tal & Mudrik, 2024 – Implementation Notes

This calibration method is equivalent to the implementation used in **Biderman & Mudrik, 2018**. The only differences are:
- **Number of trials per run**:  
  Set to **100**

- **Threshold estimation**:  
  Defined as the **second-lowest ISI** among the **last 40 trials**

To reproduce this method
1. In `Biderman & Mudrik, 2018.R` file, modify the following parameters:
```r
max_trials <- 100
threshold_calculation_method <- "Tal & Mudrik, 2024"
```
2. Then, run the `main_calibration.r` script using `METHOD_NAME <- "Biderman & Mudrik, 2018"` 
