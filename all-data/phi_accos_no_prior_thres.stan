data {
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> I;  // num inds
  int<lower=5> T[I]; // num of travel occasions for each individuals
  int<lower=1> T_unsup;  // num max unsupervised items
  int<lower=1,upper=V+1> u[I,T_unsup]; // unsup words

  real age[I,T_unsup]; // age covariates
  real accos[I,T_unsup]; // accos covariates

  real tl[I,T_unsup]; // accos covariates
  real nth[I,T_unsup]; // accos covariates
  real pkg_amt[I,T_unsup]; // accos covariates
}

parameters {
  vector[V] phi_alpha[K];
  vector[V] phi_beta_accos;
  real theta_alpha;
  real theta_beta_age;
  real theta_beta_tl;
  real theta_beta_nth;
  ordered[K] thres;
}

model {

  for(k in 1:K) {
    phi_alpha[k] ~ normal(0,0.1);
  }
  theta_alpha ~ normal(0,0.1);
  theta_beta_age ~ normal(0,0.1);
  theta_beta_tl ~ normal(0,0.1);
  theta_beta_nth ~ normal(0,0.1);
  phi_beta_accos ~ normal(0,0.1);
  thres ~ normal (0,0.1);


  for (i in 1:I){
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T[i],K];
    real util;

    vector[K] theta[K];
    vector[K] thetareg[K];
    vector[V] phi[K];
    vector[V] phireg[K];

    for (k in 1:K) {
      phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,1];
      phi[k] <- softmax(phireg[k]);
    }

    for (k in 1:K)
      gamma[1,k] <- log(phi[k,u[i,1]]);

    for (t in 2:T[i]) {
      for (k in 1:K) {
        util <- theta_alpha 
                + theta_beta_age * age[i,t] 
                + theta_beta_nth * nth[i,t] 
                + theta_beta_tl * tl[i,t];
        thetareg[k] <- exp(thres - util);

        theta[k,1] <- thetareg[k,1]/(1+thetareg[k,1]);
        for (k2 in 2:(K-1))
          theta[k,k2] <- thetareg[k,k2]/(1+thetareg[k,k2]) - thetareg[k,k2-1]/(1+thetareg[k,k2-1]);
        theta[k,K] <- 1 - thetareg[k,K-1]/(1+thetareg[k,K-1]);

        phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,t];
        phi[k] <- softmax(phireg[k]);
      }
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    increment_log_prob(log_sum_exp(gamma[T[i]]));
  }
}

generated quantities {
  real log_prob;
  real aic;
  real bic;

  for (i in 1:I){
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T[i],K];
    int numparam;

    vector[K] theta[K];
    vector[K] thetareg[K];
    vector[V] phi[K];
    vector[V] phireg[K];
    real util;

    for (k in 1:K) {
      phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,1];
      phi[k] <- softmax(phireg[k]);
    }

    for (k in 1:K)
      gamma[1,k] <- log(phi[k,u[i,1]]);

    for (t in 2:T[i]) {
      for (k in 1:K) {
        util <- theta_alpha
                + theta_beta_age * age[i,t] 
                + theta_beta_nth * nth[i,t] 
                + theta_beta_tl * tl[i,t];
        thetareg[k] <- exp(thres - util);

        theta[k,1] <- thetareg[k,1]/(1+thetareg[k,1]);
        for (k2 in 2:(K-1))
          theta[k,k2] <- thetareg[k,k2]/(1+thetareg[k,k2]) - thetareg[k,k2-1]/(1+thetareg[k,k2-1]);
        theta[k,K] <- 1 - thetareg[k,K-1]/(1+thetareg[k,K-1]);

        phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,t];
        phi[k] <- softmax(phireg[k]);
      }
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    log_prob <- log_prob + log_sum_exp(gamma[T[i]]);
    numparam <- (V*3 + K*4 + K*K + K*V);
    aic <- 2 * numparam - 2* log_prob;
    bic <- -2 * log_prob + numparam * log(I);
  }
}
