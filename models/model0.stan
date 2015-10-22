# model0.stan
# This model treats each unit as having a single firing rate for each outcome.
# Firing rates for different units and different outcomes are completely independent.
# This is the model of Figure 3 in the main text.

data {
  int<lower=0> U; // number of units
  int<lower=0> N; // number of trials
  int<lower=0> T; // number of trial types
  int c[N]; // counts for each trial
  int type[N]; // type of each trial
  int unit[N]; // unit observed on each trial
  real reward[N]; // relative reward on each trial
}
parameters {
  real beta[U, T];  // baseline for each unit
  real sens[U, T];  // sensitivity to reward size
}
transformed parameters {
  real lp[N];
  for (n in 1:N) {
    lp[n] <- beta[unit[n], type[n]] + reward[n] * sens[unit[n], type[n]];
  }
}
model {
  for (u in 1:U) {
    for (t in 1:T) {
      beta[u, t] ~ normal(0, 3);
      sens[u, t] ~ normal(0, 3);
    }
  }
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}