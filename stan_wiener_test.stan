data {
  int<lower=0> Nu; // number of upper boundary responses
  int<lower=0> Nl; // number of lower boundary responses
  real RTu[Nu]; // upper boundart responde times
  real RTl[Nl]; // upper boundart responde times
}
parameters {
  real delta;
  real<lower=0, upper=10> alpha;
  real<lower=0, upper=1> beta;
  real<lower=0, upper=1> tau;
}

model {
  RTu ~ wiener(alpha, tau, beta, delta);
  RTl ~ wiener(alpha, tau, 1-beta, -delta);
}