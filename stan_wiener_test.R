library(RWiener)
library(rstan)


# parameters of the DDM (parameter names in Ratcliffs DDM)
# alpha (a): Boundary separation or Speed-accuracy trade-off (high alpha means high accuracy). alpha > 0
# beta (b): Initial bias Bias for either response (beta > 0.5 means bias towards "upper" response 'A'). 0 < beta < 1
# delta (v): Drift rate Quality of the stimulus (delta close to 0 means ambiguous stimulus or weak ability). 0 < delta
# tau (ter): Nondecision time + Motor response time + encoding time (high means slow encoding, execution). 0 < ter (in seconds)
n_per_condition = 1000

true_paras = data.frame(delta = c(.5, .9, .5, .5, .5),
                        alpha = c(1.25, 1.25, 1.5, 1.25, 1.25),
                        beta = c(.5, .5, .5, .4, .5),
                        tau = c(.15,.15,.15,.15,.2))
for (s in 1:5) {
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
  
  list2env(stan_data,envir = .GlobalEnv)
  stan_rdump(as.list(names(stan_data)),file = paste0("wiener_test_data",s,".rdump"))
  
}

m = stan_model(file = "stan_wiener_test.stan")
fit = sampling(m,data = stan_data,iter = 1000,seed = 1234)

shift = seq(-.25,.25,length = 5)



fits = vector(mode = "list",length = 5) 
for (s in 1:5) {fits[[s]] = read_stan_csv(paste0("samples",s,".csv"))}



par(mfrow = c(2,2), mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)

for (p in c("delta","alpha","beta","tau")) {
  
  plot(1:5,true_paras[,p],col = "red", pch = 19, xlim = c(.5,5.5),
       ylim = range(c(true_paras[,p],range(sapply(fits,function(x) range(hdi(as.matrix(x,p))))))),
       main = p,ylab = "",xlab = "")
  for (j in 1:5) {
    x = 1:5+shift[j]
    s = summary(fits[[j]])[[1]]
    mhat = s[grep(p,rownames(s)),"mean"]
    CI = s[grep(p,rownames(s)),c("2.5%","97.5%")]
    CI2 = s[grep(p,rownames(s)),c("25%","75%")]
    segments(x,CI[,1],x,CI[,2])
    segments(x,CI2[,1],x,CI2[,2],lwd = 2)
    points(x,true_paras[,p],pch = 19,col = "red")
    points(x,mhat,pch = 8)
  }
  
}

legend("topleft",
       legend = c("true","est, 50% & 95% CI"),
       pch = c(19,8),
       col= c("red","black"),
       bty = "n")

dev.copy(png,width = 20, height = 15, unit = "cm", res = 300, filename = "wiener_test.png")
dev.off()
