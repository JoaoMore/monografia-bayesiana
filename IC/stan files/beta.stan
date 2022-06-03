data {
  int N;
  int y[N];
  real<lower=0> a;
  real<lower=0> b;
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  target += beta_lpdf(p |a, b);
}

