data {
  int<lower=0> K;
  int<lower=0> N;
  matrix[N, 2] x;
  vector[K] alpha;
}

parameters {
  simplex[K] pi;
  matrix[K, 2] mu;
  matrix<lower=0>[K, 2] sigmas;
}

model {
  pi ~ dirichlet(alpha);
  for (k in 1:K) {
    mu[k] ~ normal(0, 5);
    sigmas[k] ~ gamma(5, 5);
  }
  
  vector[K] log_prob;
  for (n in 1:N) {
    for (k in 1:K) {
      log_prob[k] = log(pi[k]) + multi_normal_lpdf(x[n] | mu[k], diag_matrix(to_vector(sigmas[k])));
    }
    target += log_sum_exp(log_prob);
  }
}

generated quantities {
  matrix[N, 2] x_sim;
  for (n in 1:N) {
    int z;
    z = categorical_rng(pi);
    x_sim[n] = to_row_vector(multi_normal_rng(mu[z], diag_matrix(to_vector(sigmas[z]))));
  }
}
