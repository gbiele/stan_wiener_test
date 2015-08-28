library(RWiener)
library(rstan)


# parameters of the DDM (parameter names in Ratcliffs DDM)
# alpha (a): Boundary separation or Speed-accuracy trade-off (high alpha means high accuracy). alpha > 0
# beta (b): Initial bias Bias for either response (beta > 0.5 means bias towards "upper" response 'A'). 0 < beta < 1
# delta (v): Drift rate Quality of the stimulus (delta close to 0 means ambiguous stimulus or weak ability). 0 < delta
# tau (ter): Nondecision time + Motor response time + encoding time (high means slow encoding, execution). 0 < ter (in seconds)
n_per_condition = 250

true_paras = data.frame(delta = c(.5, .7, .5, .5, .5),
                        alpha = c(1.25, 1.25, 1.5, 1.25, 1.25),
                        beta = c(.5, .5, .5, .4, .5),
                        tau = c(.15,.15,.15,.15,.2))

resp = c()
for (k in 1:5) resp = rbind(resp, rwiener(n_per_condition,
                                          true_paras$alpha[k],
                                          true_paras$tau[k],
                                          true_paras$beta[k],
                                          true_paras$delta[k]))
resp$condition  = sort(rep(1:5,n_per_condition))

stan_data = list(Nu = sum(resp$resp == "upper"),
                  Nl = sum(resp$resp == "lower"),
                  RTu = resp$q[resp$resp == "upper"],
                  RTl = resp$q[resp$resp == "lower"],
                  K = length(unique(resp$condition)),
                  su = as.vector(table(resp$condition[resp$resp == "upper"])),
                  sl = as.vector(table(resp$condition[resp$resp == "lower"])),
                  minRT = min(resp[,1]))

m = stan_model(file = "stan_wiener_test.stan")
fit = sampling(m,data = stan_data,iter = 1000,seed = 1234)


s = summary(fit)[[1]]
par(mfrow = c(2,2), mar = c(2.5,2.5,2,2))
for (p in c("delta","alpha","beta","tau")) {
  mhat = s[grep(p,rownames(s)),"mean"]
  CI = s[grep(p,rownames(s)),c("2.5%","97.5%")]
  plot(1:5,true_paras[,p],col = "red", pch = 19,
       ylim = range(cbind(CI,true_paras[,p])),main = p,ylab = "",xlab = "")
  points(1:5,mhat,pch = 8)
  segments(1:5,CI[,1],1:5,CI[,2])
}