# model5.stan
# This model treats each unit as having a firing rate response proportional to reward.
# Regression parameters are tied via a hierarchical prior.
# Parameters are assumed to be drawn from a mixture of two distributions:
#   1) A noise distribution that is isotropic and has only a baseline firing rate
#   2) A distribution of responding cells with a more general distribution.
# Modeling is similar to section 5.12 of the Stan reference manual 2.8.0.
# Mixture modeling is based on the treatment in Ch9.

data {
  int<lower=0> U; // number of units
  int<lower=0> N; // number of trials
  int<lower=0> T; // number of trial types
  int<lower=0> P; // number of regressors (including baseline)
  int c[N]; // counts for each trial
  int type[N]; // type of each trial
  int unit[N]; // unit observed on each trial
  row_vector[P] X[N]; // design vector for each trial
}
parameters {
  vector[P] beta[U];  // baseline for each unit
  vector[P] mu;  // mean of population baseline
  vector<lower=0>[P] tau;  // scale of population baseline
  cholesky_factor_corr[P] L;  // Cholesky factor for covariance matrix
  real<lower=0, upper=1> theta;  // mixture probability (p(signal))
  vector<lower=0>[P] sig_noise;  // standard deviation of baseline in non-sensitive condition
}
transformed parameters {
  corr_matrix[P] Sigma;  // correlation matrix of sensitivities

  for (p in 1:P) {
    Sigma <- L * L';
  }
}
model {
  real lp[N];  // linear predictor for firing rate
  real LL0;  // log likelihood for null population
  real LL1;  // log likelihood for responsive population

  mu ~ normal(0, 3);
  tau ~ cauchy(0, 2.5);
  L ~ lkj_corr_cholesky(2);
  theta ~ uniform(0, 1);
  sig_noise[1] ~ cauchy(0, 2.5);
  for (p in 2:P) {
    sig_noise ~ cauchy(0, 0.01);
  }

  for (u in 1:U) {
    LL1 <- multi_normal_log(beta[u], mu, quad_form_diag(Sigma, tau));
    LL0 <- normal_log(beta[u], 0, sig_noise);
    increment_log_prob(log_mix(theta, LL1, LL0));
  }

  for (n in 1:N) {
    lp[n] <- X[n] * beta[unit[n]];
  }

  // observations
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}