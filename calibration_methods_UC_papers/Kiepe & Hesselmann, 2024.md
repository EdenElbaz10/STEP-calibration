# Kiepe & Hesselmann, 2024 – Implementation Notes

This calibration method is implemented using the standard **QUEST** procedure, as described in the script:

**'QUEST.py'** (located in the 'calibration_methods/' folder)

The only modification required to match the parameters reported in *Kiepe & Hesselmann (2024)* is:

- **Number of trials**:  
  Set to **50**

Because the algorithmic implementation is identical to the general QUEST procedure, no separate Python script is provided.  
To reproduce *Kiepe & Hesselmann, 2024*, simply run `QUEST.py` with the following configuration:

```python
N_TRIALS = 50
```