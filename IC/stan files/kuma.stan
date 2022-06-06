// The input data is a vector 'y' of length 'N'.
data {
  int N;
  int y;
  real<lower=0> a;
  real<lower=0> b;
}

parameters {
  real<lower=0, upper=1> p;
}


model {
  // Prior on p:
  target += log(a*b)+(a-1)*log(p)+(b-1)*log(1-pow(p,a)); 
  // Likelihood:
  target += binomial_lpmf(y | N, p); 
}
