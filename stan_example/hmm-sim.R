library("rstan")
source("hmm-semisup.data.R")
fit <- stan('hmm-semisup.stan', # 'hmm-fit-semisup.stan',
            data=list(K=K,V=V,T_unsup=T_unsup,u=u,alpha=alpha,beta=beta),
            iter=3000, chains=2, init=0);  # fit = fit // reuse model
