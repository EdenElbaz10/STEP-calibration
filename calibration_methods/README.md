# Running python scripts
I used python 3.8 with the following packages:
```
numpy==1.23.2
pandas==1.5.3
psychopy==2025.1.0
psychtoolbox==3.0.19.14
questplus==2023.1
```

## Note about questplus
In questplus package (`questplus==2023.1`), the objective psychometric functinon was changed to a modified Weibull function, adapted from (Rouder & Morey, 2009):

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

One can reproduce the modifications following: https://github.com/EdenElbaz10/questplus
