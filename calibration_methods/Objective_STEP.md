# Objective STEP – Implementation Notes

The **Objective STEP** method is a variant of the STEP calibration procedure, tailored for use in experiments that rely solely on objective performance measures (e.g., X-AFC), without any subjective awareness reports.

To use this method
1. In `STEP.R` file, modify the following parameter:
```r
Subjective_test <- 0  # Set to 0 to disable integration with subjective reports
```

2. Then, run the `main_calibration.r` script using `METHOD_NAME <- "STEP"` 


## Differences from Standard STEP

The STEP procedure can also be applied in studies where subjective awareness measures are impractical or undesirable. To adapt STEP for objective-only use, two key modifications are made:

### A. No Subjective Integration

The subjective component is skipped entirely.

### B. Alternative Upper Boundary Calculation

When significant performance is detected, the upper boundary is updated using a simple average  
(rather than a weighted average based on subjective visibility):

$$
\text{newBoundary} = \frac{\text{Boundary} + \text{currentISI}}{2}
$$

This adjustment ensures that the threshold is still adaptively refined, but solely based on objective performance criteria.
