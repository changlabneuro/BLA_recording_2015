# model7.stan
# This model treats each unit as having a firing rate response proportional to reward.
# Regression parameters are tied via a hierarchical prior.
# Parameters within each outcome type are assumed to be drawn from a multivariate t distribution
# Parameters across outcomes are drawn from a product of these t-distributions, corresponding to
# a sparse code (see exploratory glm analysis for an empirical justification of this)
# Modeling is similar to section 5.12 of the Stan reference manual 2.8.0.
# Mixture modeling is based on the treatment in Ch9.

data {
  int<lower=0> U; // number of units
  int<lower=0> N; // number of trials
  int<lower=0> T; // number of trial outcomes
  int<lower=0> P; // number of regressors (including baseline)
  int c[N]; // counts for each trial
  int type[N]; // type of each trial
  int unit[N]; // unit observed on each trial
  int outcome[N];  // outcome type for each trial
  row_vector[P] X[N]; // design vector for each trial
}

parameters {
  vector[P] beta_raw[U, T];  // N(0, 1) versions of params
  vector[P] mu[T];  // mean of population baseline
  vector<lower=0>[P] tau[T];  // scale of population baseline
  cholesky_factor_corr[P] L[T];  // Cholesky factor for covariance matrix
  real<lower=1> nu[T];  // degrees of freedom for parameter t
}
transformed parameters {
  corr_matrix[P] Sigma[T];  // correlation matrix of sensitivities
  vector[P] beta[U, T];  // regression coefficients for each unit

  for (t in 1:T){
    Sigma[t] <- L[t] * L[t]';
  }

  for (u in 1:U) {
    for (t in 1:T) {
      beta[u, t] <- mu[t] + diag_pre_multiply(tau[t], L[t]) * beta_raw[u, t];
    }
  }

}
model {
  real lp[N];  // linear predictor for firing rate

  for (t in 1:T) {
    mu[t] ~ normal(0, 2);
    tau[t] ~ cauchy(0, 2.5);
    L[t] ~ lkj_corr_cholesky(2);
    nu[t] ~ cauchy(0, 25);
  }

  for (u in 1:U) {
    for (t in 1:T) {
      beta_raw[u, t] ~ student_t(nu[t], 0, 1);
    }
  }

  for (n in 1:N) {
    lp[n] <- X[n] * beta[unit[n], outcome[n]];
  }

  // observations
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}