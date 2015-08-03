library(RWiener)
library(rstan)


# parameters of the DDM (parameter names in Ratcliffs DDM)
# alpha (a): Boundary separation or Speed-accuracy trade-off (high alpha means high accuracy). alpha > 0
# beta (b): Initial bias Bias for either response (beta > 0.5 means bias towards "upper" response 'A'). 0 < beta < 1
# delta (v): Drift rate Quality of the stimulus (delta close to 0 means ambiguous stimulus or weak ability). 0 < delta
# tau (ter): Nondecision time + Motor response time + encoding time (high means slow encoding, execution). 0 < ter (in seconds)
n_per_condition = 500

resp = rbind(rwiener(n_per_condition,delta = .2, alpha = 1.25, beta = .5, tau = .2),
             rwiener(n_per_condition,delta = .2, alpha = 1.25, beta = .4, tau = .2),
             rwiener(n_per_condition,delta = .3, alpha = 1.25, beta = .6, tau = .2),
             rwiener(n_per_condition,delta = .2, alpha = 1.5, beta = .5, tau = .3))
resp$condition  = sort(rep(1:4,n_per_condition))


stan_data2 = list(Nu = sum(resp$resp == "upper"),
                  Nl = sum(resp$resp == "lower"),
                  RTu = resp$q[resp$resp == "upper"],
                  RTl = resp$q[resp$resp == "lower"],
                  K = length(unique(resp$condition)),
                  su = table(resp$condition[resp$resp == "upper"]),
                  sl = table(resp$condition[resp$resp == "lower"]),
                  minRT = min(resp[,1]))

inits = list(alpha = c(1,1,1,1),
             beta = c(.5,.5,.5,.5),
             delta = c(0,0,0,0),
             tau = c(.1,.1,.1,.1))

fit = stan(file = "stan_wiener_test2.stan",data = stan_data2,iter = 1000)
fit = stan(fit = fit,data = stan_data2,init = list(inits,inits,inits,inits),iter = 1000)

fit = stan(fit = fit,
           data = stan_data2,
           chains = 4,
           init = list(inits,inits,inits,inits),
           iter = 1000)



RTs = resp[,1]
RTs[resp[,2] == "lower"] = -RTs[resp[,2] == "lower"]
breaks = seq(from = -5,to = 5,by = .2)
breaks = breaks[(which(findInterval(breaks,sort(RTs)) == 1)[1]-1):
                  which(findInterval(breaks,sort(RTs)) == length(RTs))[1]]
hist(RTs,breaks = breaks)