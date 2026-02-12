// simple_model.stan
// A basic Bayesian linear regression model to analyze pedicle width differences across vertebral levels.
// This model estimates a mean pedicle width difference (concave_width - convex_width) for each vertebral level (T1-T6).

data {
  int<lower=0> N;                                     // Number of observations (rows in the dataset)
  array[N] int<lower=1, upper=6> vertebra_level_numeric; // Vertebral level for each observation (1=T1, 2=T2, ..., 6=T6)
  array[N] real pedicle_width_diff;                   // Pedicle width difference (concave_mm - convex_mm) for each observation
}

parameters {
  array[6] real mu_diff;                             // Mean pedicle width difference for each vertebral level (T1-T6)
  real<lower=0> sigma;                               // Standard deviation of the pedicle width differences
}

model {
  // Priors:
  // Informative priors can be chosen based on previous research or clinical expertise.
  // Here, we use relatively diffuse priors suitable for initial exploration.
  mu_diff ~ normal(0, 5);                             // Prior for mean differences, centered at 0 with a standard deviation of 5mm.
                                                      // Assuming differences typically fall within +/- 10-15mm.
  sigma ~ exponential(1);                             // Prior for the standard deviation, encouraging positive values.
                                                      // An exponential prior with rate 1 implies a mean of 1mm, which is reasonable for variation in pedicle widths.

  // Likelihood:
  // The pedicle width difference for each observation is modeled as normally distributed
  // with a mean specific to its vertebral level and a common standard deviation 'sigma'.
  for (i in 1:N) {
    pedicle_width_diff[i] ~ normal(mu_diff[vertebra_level_numeric[i]], sigma);
  }
}

generated quantities {
  // This block can be used to compute quantities of interest based on the posterior samples.
  // For example, posterior predictive distributions, or differences between specific level means.

  // Example: Difference between mean pedicle width differences at T3 and T2
  // real diff_T3_T2 = mu_diff[3] - mu_diff[2];
  // More complex comparisons or transformations can be added here as needed for analysis.
}
