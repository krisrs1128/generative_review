data {
  int<lower=0> K;
  int<lower=0> N;
  matrix[N, 2] x;
}

parameters {
  simplex[K] pi;
  matrix[K, 2] mu;
  real<lower=0> sigma;
}

model {
  vector[K] log_prob;
  for (n in 1:N) {
    for (k in 1:K) {
      log_prob[k] = log(pi[k]) + normal_lpdf(x[n] | mu[k], sigma);
    }
    target += log_sum_exp(log_prob);
  }
}


generated quantities {
  matrix[N, 2] x_sim;
  for (n in 1:N) {
    int z;
    z = categorical_rng(pi);
    x_sim[n] = to_row_vector(normal_rng(mu[z], sigma));
  }
}