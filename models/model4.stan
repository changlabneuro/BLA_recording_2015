# model4.stan
# This model treats each unit as having a firing rate response proportional to reward.
# Reward sensitivities and baseline firing are drawn from a mixture containing two components:
#   1) A noise mode with mean 0 and isotropic covariance.
#   2) A "signal" distribution with multivariate normal statistics.
# Sensitivity parameters for units are tied via a hierarchical prior.
# Modeling is similar to section 5.12 of the Stan reference manual 2.8.0.
# Mixture modeling is based on the treatment in Ch9.

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
  vector[T] mu_beta;  // mean of population baseline
  vector[T] mu_sens;  // mean of population sensitivity
  vector<lower=0>[T] tau_beta;  // scale of population baseline
  vector<lower=0>[T] tau_sens;  // scale of population sensitivity
  cholesky_factor_corr[T] L_beta;  // Cholesky factor for covariance matrix
  cholesky_factor_corr[T] L_sens;  // Cholesky factor for covariance matrix
  real<lower=0, upper=1> theta;  // mixture probability (p(signal))
  real<lower=0> sig_noise;  // standard deviation of baseline in non-sensitive condition
}
transformed parameters {
  corr_matrix[T] Sigma_beta;  // correlation matrix of sensitivities
  corr_matrix[T] Sigma_sens;  // correlation matrix of sensitivities

  Sigma_beta <- L_beta * L_beta';
  Sigma_sens <- L_sens * L_sens';
}
model {
  real lp[N];  // linear predictor for firing rate
  real bsens;  // mixture term for beta_sens

  mu_beta ~ normal(0, 3);
  mu_sens ~ normal(0, 3);
  tau_beta ~ cauchy(0, 2.5);
  tau_sens ~ cauchy(0, 2.5);
  sig_noise ~ cauchy(0, 2.5);
  L_beta ~ lkj_corr_cholesky(2);
  L_sens ~ lkj_corr_cholesky(2);
  theta ~ uniform(0, 1);

  for (u in 1:U) {
    bsens <- log_mix(theta,
      multi_normal_log(beta[u], mu_beta, quad_form_diag(Sigma_beta, tau_beta)) +
      multi_normal_log(sens[u], mu_sens, quad_form_diag(Sigma_sens, tau_sens)),
      normal_log(beta[u], 0, sig_noise));
    increment_log_prob(bsens);
    #beta[u] ~ multi_normal(mu_beta, quad_form_diag(Sigma_beta, tau_beta));
    #sens[u] ~ multi_normal(mu_sens, quad_form_diag(Sigma_sens, tau_sens));
  }

  for (n in 1:N) {
    lp[n] <- beta[unit[n]][type[n]] + reward[n] * sens[unit[n]][type[n]];
  }

  // observations
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}