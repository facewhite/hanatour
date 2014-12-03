data {
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> I;  // num inds
  int<lower=5> T[I]; // num of travel occasions for each individuals
  int<lower=1> T_unsup;  // num max unsupervised items
  int<lower=1,upper=V+1> u[I,T_unsup]; // unsup words
  real age[I,T_unsup]; // age covariates
  real accos[I,T_unsup]; // accos covariates
}

parameters {
  vector[V] phi_alpha[K];
  vector[V] phi_beta;
  vector[K] prior_alpha;
  vector[K] prior_beta;
  vector[K] theta_alpha[K];
  vector[K] theta_beta;
}

model {
  for(k in 1:K) {
    prior_alpha ~ normal(0,0.1);
  }
  prior_beta ~ normal(0,0.1);

  for(k in 1:K) {
    theta_alpha[k] ~ normal(0,0.1);
    phi_alpha[k] ~ normal(0,0.1);
  }
  theta_beta ~ normal(0,0.1);
  phi_beta ~ normal(0,0.1);


  for (i in 1:I){
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T[i],K];

    vector[K] prior;
    vector[K] priorreg;
    vector[K] theta[K];
    vector[K] thetareg[K];
    vector[V] phi[K];
    vector[V] phireg[K];

    priorreg <- prior_alpha + prior_beta * age[i,1];
    prior <- softmax(priorreg);
    for (k in 1:K) {
      phireg[k] <- phi_alpha[k] + phi_beta * accos[i,1];
      phi[k] <- softmax(phireg[k]);
    }

    for (k in 1:K)
      gamma[1,k] <- log(prior[k]) + log(phi[k,u[i,1]]);

    for (t in 2:T[i]) {
      for (k in 1:K) {
        thetareg[k] <- theta_alpha[k] + theta_beta * age[i,t];
        theta[k] <- softmax(thetareg[k]);

        phireg[k] <- phi_alpha[k] + phi_beta * accos[i,t];
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
