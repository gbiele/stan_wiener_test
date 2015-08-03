data {
  int<lower=0> Nu; // # of upper boundary responses
  int<lower=0> Nl; // # of lower boundary responses
  real RTu[Nu]; // upper boundary response times
  real RTl[Nl]; // upper boundary response times
  int<lower=0> K; // # of groups
  int su[K]; // group sizes upper boundary responses
  int sl[K]; // group sizes lower boundary responses
}

parameters {
  real delta[K];
  real<lower=0, upper=10> alpha[K];
  real<lower=0, upper=1> beta[K];
  real<lower=.1, upper=1> tau[K]; # upper boundary of tau should be smaller than minimum RT
}

model {
  int posu;
  int posl;
  posu <- 1;
  posl <- 1;
  //tau ~ cauchy(0,.1);
  delta ~ cauchy(0,.5);
  alpha ~ cauchy(0,.5);
  beta ~ beta(1.1,1.1);
   for (k in 1:K){
    segment(RTu, posu, su[k]) ~ wiener(alpha[k], tau[k], beta[k], delta[k]);
    posu <- posu + su[k];
    segment(RTl, posl, sl[k]) ~ wiener(alpha[k], tau[k], 1-beta[k], -delta[k]);
    posl <- posl + sl[k];
  }
}
