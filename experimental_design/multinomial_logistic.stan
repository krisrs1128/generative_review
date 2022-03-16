data {
  int<lower=0> N;
  int<lower=0> D;
  int<lower=0> T;
  int<lower=0> S;
 
  array[N] int t_ix;
  array[N] int s_ix;
  array[N, D] int x;
}

parameters {
  array[S] vector[D] subject;
  array[T] vector[D] treatment;
  vector[D] baseline;
}

model {
  for (n in 1:N) {
    x[n] ~ multinomial_logit(baseline + subject[s_ix[n]] + treatment[t_ix[n]]);
  }
}