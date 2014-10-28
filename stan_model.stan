data {
  int<lower=0> I; // number of individuals
  int<lower=0> J; // number of travels
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates
  int<lower=0, upper=7> regions[I][J];
  int<lower=0, upper=50> area_code[I][J];

}

parameters {
  real mu;
  real<lower=0> tau;
  real eta[J];
}

transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] <- mu + tau * eta[j];
}

model {
  eta ~ normal(0, 1);
  y ~ normal(theta, sigma);
}
