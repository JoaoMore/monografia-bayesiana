// The input data is a vector 'y' of length 'N'.
data {
  int N;
  int y[N];
  real<lower=0> a;
  real<lower=0> b;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper=1> p;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += (a-1)*log(a*b*p)+(b-1)*log(1-p);
}
