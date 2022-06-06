data {
  int N; //N=1000
  int y; // Y[1]
  real<lower=0> a;
  real<lower=0> b;
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  y ~ binomial(N, p); //binomial(1000, p)
  p ~ beta(a, b);
}

