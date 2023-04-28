data {
  int N_m;
  int N_f;
  int N_m_cens;
  int N_f_cens;
  vector<lower=0>[N_m] days_m;
  vector<lower=0>[N_f] days_f;
  vector<lower=0>[N_m_cens] days_m_cens;
  real days_f_cens;
}

parameters {
  real<lower=0> mu_m;
  real<lower=0> sigma_m;
  real<lower=0> mu_f;
  real<lower=0> sigma_f;
}

model {
  // prior
  mu_m ~ normal(3.66, 2);
  mu_f ~ normal(3.66, 2);

  // male model
  target += lognormal_lpdf(days_m | mu_m, sigma_m);
  target += lognormal_lccdf(days_m_cens | mu_m, sigma_m);

  // female model
  target += lognormal_lpdf(days_f | mu_f, sigma_f);
  target += lognormal_lccdf(days_f_cens | mu_f, sigma_f);
}

generated quantities {
  real y_m;
  real y_f;
  y_m = lognormal_rng(mu_m, sigma_m);
  y_f = lognormal_rng(mu_f, sigma_f);
}
