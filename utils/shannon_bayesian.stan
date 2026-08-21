data {
  int<lower=1> N;
  int<lower=1> R;
  vector[N] y;
  array[N] int<lower=1, upper=R> region;
}

parameters {
  vector[R] mu_region;
  real<lower=0> sigma;
}

model {
  // Priors
  mu_region ~ normal(0, 1);
  sigma ~ exponential(1);

  // Likelihood
  y ~ normal(mu_region[region], sigma);
}

generated quantities {
  vector[N] y_rep;

  for (n in 1:N)
    y_rep[n] = normal_rng(mu_region[region[n]], sigma);
}