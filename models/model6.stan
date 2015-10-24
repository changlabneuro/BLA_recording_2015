# model6.stan
# This model treats each unit as having a firing rate response proportional to reward.
# Regression parameters are tied via a hierarchical prior.
# Parameters are assumed to be drawn from a multivariate t distribution
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
  vector[P] beta[U];  // regression coefficients for each unit
  vector[P] mu;  // mean of population baseline
  vector<lower=0>[P] tau;  // scale of population baseline
  cholesky_factor_corr[P] L;  // Cholesky factor for covariance matrix
  real<lower=1> nu;  // degrees of freedom for parameter t
}
transformed parameters {
  corr_matrix[P] Sigma;  // correlation matrix of sensitivities
  
  Sigma <- L * L';
  
}
model {
  real lp[N];  // linear predictor for firing rate

  mu ~ normal(0, 2);
  tau ~ cauchy(0, 2.5);
  L ~ lkj_corr_cholesky(2);
  nu ~ cauchy(0, 25);

  for (u in 1:U) {
    beta[u] ~ multi_student_t(nu, mu, quad_form_diag(Sigma, tau));
  }

  for (n in 1:N) {
    lp[n] <- X[n] * beta[unit[n]];
  }

  // observations
  for (n in 1:N) {
    c[n] ~ poisson(exp(lp[n]));
  }
}