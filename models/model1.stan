# model1.stan
# This model treats each unit as having a single firing rate for each outcome.
# Firing rates for different outcomes are completely independent.
# Firing rates for units are tied via a hierarchical prior.

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
  real mu_beta[T];  // mean of population
  real mu_sens[T];  // mean of population
  real<lower=0> sig_beta[T];  // standard deviation of population
  real<lower=0> sig_sens[T];  // standard deviation of population
}
transformed parameters {
  real lp[N];
  for (n in 1:N) {
    lp[n] <- beta[unit[n], type[n]] + reward[n] * sens[unit[n], type[n]];
  }
}
model {
  for (t in 1:T) {
    mu_beta[t] ~ normal(0, 3);
    mu_sens[t] ~ normal(0, 3);
    sig_beta[t] ~ cauchy(0, 2.5);
    sig_sens[t] ~ cauchy(0, 2.5);
  }
  for (u in 1:U) {
    for (t in 1:T) {
      beta[u, t] ~ normal(mu_beta[t], sig_beta[t]);
      sens[u, t] ~ normal(mu_sens[t], sig_sens[t]);
    }
  }
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}