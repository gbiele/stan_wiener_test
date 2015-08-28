library(RWiener)
library(rstan)


# parameters of the DDM (parameter names in Ratcliffs DDM)
# alpha (a): Boundary separation or Speed-accuracy trade-off (high alpha means high accuracy). alpha > 0
# beta (b): Initial bias Bias for either response (beta > 0.5 means bias towards "upper" response 'A'). 0 < beta < 1
# delta (v): Drift rate Quality of the stimulus (delta close to 0 means ambiguous stimulus or weak ability). 0 < delta
# tau (ter): Nondecision time + Motor response time + encoding time (high means slow encoding, execution). 0 < ter (in seconds)
n_per_condition = 250

resp = rbind(rwiener(n_per_condition,delta = .5, alpha = 1.25, beta = .5, tau = .15),
             rwiener(n_per_condition,delta = .5, alpha = 1.25, beta = .4, tau = .15),
             rwiener(n_per_condition,delta = .7, alpha = 1.25, beta = .6, tau = .15),
             rwiener(n_per_condition,delta = .5, alpha = 1.5, beta = .5, tau = .2))
resp$condition  = sort(rep(1:4,n_per_condition))


stan_data = list(Nu = sum(resp$resp == "upper"),
                  Nl = sum(resp$resp == "lower"),
                  RTu = resp$q[resp$resp == "upper"],
                  RTl = resp$q[resp$resp == "lower"],
                  K = length(unique(resp$condition)),
                  su = as.vector(table(resp$condition[resp$resp == "upper"])),
                  sl = as.vector(table(resp$condition[resp$resp == "lower"])),
                  minRT = min(resp[,1]))

m = stan_model(file = "stan_wiener_test.stan")
fit = sampling(m,data = stan_data,iter = 400,seed = 1234)

