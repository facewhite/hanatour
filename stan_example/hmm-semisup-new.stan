data {
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> I;  // num inds
  int<lower=1> T_unsup;  // num unsupervised items
  int<lower=1,upper=V> u[I,T_unsup]; // unsup words
  vector<lower=0>[K] alpha;  // transit prior
  vector<lower=0>[K] aprior;  // prior prob.
  vector<lower=0>[V] beta;   // emit prior
}

parameters {
  simplex[K] theta[K];  // transit probs
  simplex[V] phi[K];    // emit probs
  simplex[K] prior;
}

model {
  prior ~ dirichlet(aprior);
  for (k in 1:K) 
    theta[k] ~ dirichlet(alpha);
  for (k in 1:K)
    phi[k] ~ dirichlet(beta);

  for (i in 1:I){ 
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T_unsup,K];
    real priorflag;

    for (k in 1:K)
      gamma[1,k] <- log(prior[k]) + log(phi[k,u[i,1]]);
    for (t in 2:T_unsup) {
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    priorflag <- 0;
    for (k in 2:K) {
      if(prior[k] > prior[k-1]) {
        priorflag <- 1;
      }
    }
    if (priorflag == 1) {
      increment_log_prob(negative_infinity());
    } else {
      increment_log_prob(log_sum_exp(gamma[T_unsup]));
    }
  }
}
