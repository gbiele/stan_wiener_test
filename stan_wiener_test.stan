data {
  int<lower=0> Nu; // # of upper boundary responses
  int<lower=0> Nl; // # of lower boundary responses
  real RTu[Nu]; // upper boundary response times
  real RTl[Nl]; // lower boundary response times
  int<lower=0> K; // # of groups
  int su[K]; // group sizes upper boundary responses
  int sl[K]; // group sizes lower boundary responses
  real minRT;
}

parameters {
  real<lower=-2, upper=1> delta[K];
  real<lower=0, upper=5> alpha[K];
  real<lower=0, upper=1> beta[K];
  /* upper boundary of tau must be smaller than minimum RT
  to avoid zero likelihood for fast responses.
  tau can for physiological reasone not be faster than 0.1 s.*/
  real<lower=.1, upper=minRT> tau[K]; 
}

model {
  int posu;
  int posl;
  posu <- 1;
  posl <- 1;
  /* beta and tau have a very restricted ranges
  so uniform prior should be OK
  tau ~ cauchy(0,.1); 
  beta ~ beta(1.1,1.1);*/
  
  /*this are not great priors for delta and alpha
  they avoid impossible high parameter values
  but they have the largest mass at/around 0
  which are also unlike parameter values*/ 
  delta ~ normal(0,2); 
  alpha ~ normal(0,2);

   for (k in 1:K){
    segment(RTu, posu, su[k]) ~ wiener(alpha[k], tau[k], beta[k], delta[k]);
    posu <- posu + su[k];
    segment(RTl, posl, sl[k]) ~ wiener(alpha[k], tau[k], 1-beta[k], -delta[k]);
    posl <- posl + sl[k];
  }
}
