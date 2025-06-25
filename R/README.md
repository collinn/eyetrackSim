## Piecewise_power.R

### `createPlineData()`

Generates data for the purpose of testing whether there's a difference between two groups using a given method.

Specifically, the function simulates two groups:

1. **Group A**: A treatment group that shows a fixation reaction which increases over time. The response remains at zero when `time < 0`, and begins increasing linearly when `time ≥ 0`.
2. **Group B**: A control group where there's no change over time — the fixation signal remains flat regardless of time.

The function first creates the two groups with these characteristics **without noise**. Then, random noise is added to introduce realistic variability into the datasets.

This simulated data can be used in statistical power analyses to evaluate the effectiveness of methods such as those provided by the `bdots` package.
