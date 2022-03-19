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
}

model {
  for (t in 1:T) {
    treatment[t] ~ normal(0, 2);
  }
  for (s in 1:S) {
    subject[s] ~ normal(0, 1);
  }
  
  for (n in 1:N) {
    x[n] ~ multinomial(softmax(subject[s_ix[n]] + treatment[t_ix[n]]));
  }
}
