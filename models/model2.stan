# model2.stan
# This model treats each unit as having a firing rate response proportional to reward.
# Firing rates for different outcomes are multivariate normally distributed.
# Firing rates for units are tied via a hierarchical prior.
# Modeling is similar to section 5.12 of the Stan reference manual 2.8.0.

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
  vector[T] beta[U];  // baseline for each unit
  vector[T] sens[U];  // sensitivity to reward size
  vector[T] mu_beta;  // mean of population
  vector[T] mu_sens;  // mean of population
  vector<lower=0>[T] tau_beta;  // standard deviation of population
  vector<lower=0>[T] tau_sens;  // standard deviation of population
  cholesky_factor_corr[T] L_beta;  // Cholesky factor for covariance matrix
  cholesky_factor_corr[T] L_sens;  // Cholesky factor for covariance matrix
}
transformed parameters {
  real lp[N];
  corr_matrix[T] Sigma_beta;  // correlation of betas
  corr_matrix[T] Sigma_sens;  // correlation matrix of sensitivities

  Sigma_beta <- L_beta * L_beta';
  Sigma_sens <- L_sens * L_sens';

  for (n in 1:N) {
    lp[n] <- beta[unit[n]][type[n]] + reward[n] * sens[unit[n]][type[n]];
  }
}
model {
  mu_beta ~ normal(0, 3);
  mu_sens ~ normal(0, 3);
  tau_beta ~ cauchy(0, 2.5);
  tau_sens ~ cauchy(0, 2.5);
  L_beta ~ lkj_corr_cholesky(2);
  L_sens ~ lkj_corr_cholesky(2);

  for (u in 1:U) {
    beta[u] ~ multi_normal(mu_beta, quad_form_diag(Sigma_beta, tau_beta));
    sens[u] ~ multi_normal(mu_sens, quad_form_diag(Sigma_sens, tau_sens));
  }

  // observations
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}