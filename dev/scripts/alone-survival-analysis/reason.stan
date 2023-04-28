data {
  int N_m;
  int N_f;
  vector[N_m] days_m;
  vector[N_f] days_f;
  real df;
}

parameters {
  real<lower=0> mu_days_m;
  real<lower=0> sigma_m;
  real<lower=0> mu_days_f;
  real<lower=0> sigma_f;
}

model {
  // prior
  mu_days_m ~ normal(39, 14);
  mu_days_f ~ normal(39, 14);

  // liklihood
  // target += normal_lpdf(days_m | mu_days_m, sigma_m);
  // target += normal_lpdf(days_f | mu_days_f, sigma_f);
  target += student_t_lpdf(days_m | df, mu_days_m, sigma_m);
  target += student_t_lpdf(days_f | df, mu_days_f, sigma_f);
}
