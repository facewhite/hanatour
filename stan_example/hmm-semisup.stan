data {
  int<lower=1> I;  // num individuals
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> T_unsup;  // num unsupervised items
  int<lower=1> tnum[I];  // num unsupervised items
  int<lower=1> u[I, T_unsup]; // unsup words
  vector<lower=0>[K] alpha;  // transit prior
  vector<lower=0>[V] beta;   // emit prior
}
parameters {
  simplex[K] theta[K];  // transit probs
  simplex[V] phi[K];    // emit probs
}

model {
  for (k in 1:K) 
    theta[k] ~ dirichlet(alpha);
  for (k in 1:K)
    phi[k] ~ dirichlet(beta);

  for (i in 1:I) { 
    real acc[K];
    real gamma[tnum[i],K];
    // forward algorithm computes log p(u|...)
    for (k in 1:K)
      gamma[1,k] <- log(phi[k,u[i,1]]);
    for (t in 2:tnum[i]) {
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    increment_log_prob(log_sum_exp(gamma[tnum[i]]));
  }
}
generated quantities {
  int<lower=1,upper=K> y_star[I,T_unsup];
  real log_p_y_star;
  for (i in 1:I) { 
    // Viterbi algorithm
    int back_ptr[tnum[i],K];
    real best_logp[tnum[i],K];
    real best_total_logp;
    for (k in 1:K)
      best_logp[1,K] <- log(phi[k,u[i,1]]);
    for (t in 2:tnum[i]) {
      for (k in 1:K) {
        best_logp[t,k] <- negative_infinity();
        for (j in 1:K) {
          real logp;
          logp <- best_logp[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
          if (logp > best_logp[t,k]) {
            back_ptr[t,k] <- j;
            best_logp[t,k] <- logp;
          }
        }
      }
    }
    log_p_y_star <- max(best_logp[tnum[i]]);
    for (k in 1:K)
      if (best_logp[tnum[i],k] == log_p_y_star)
        y_star[i,tnum[i]] <- k;
    for (t in 1:(tnum[i] - 1))
      y_star[i,tnum[i] - t] <- back_ptr[tnum[i] - t + 1, 
                                      y_star[i,tnum[i] - t + 1]];
  }
}
