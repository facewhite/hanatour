data {
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> I;  // num inds
  int<lower=1> T_unsup;  // num unsupervised items
  int<lower=1,upper=V> u[I,T_unsup]; // unsup words
  vector<lower=0>[K] alpha;  // transit prior
  vector<lower=0>[V] beta;  // emit prior
  int<lower=0> age[I,T_unsup]; // unsup words
}

parameters {
  simplex[V] phi[K];    // emit probs
  ordered[K] prior_alpha;
  vector[K] prior_beta;
  vector[K] theta_alpha[K];
  vector[K] theta_beta;
}

model {
  prior_beta ~ normal(0,0.1);

  for(k in 1:K) {
    theta_alpha[k] ~ normal(0.3*k,0.1);
  }
  theta_beta ~ normal(0,0.1);

  for (k in 1:K)
    phi[k] ~ dirichlet(beta);

  for (i in 1:I){ 
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T_unsup,K];

    vector[K] prior;
    vector[K] priorreg;
    vector[K] theta[K];
    vector[K] thetareg[K];

    priorreg <- prior_alpha + prior_beta * age[i,1];
    prior <- softmax(priorreg);

    for (k in 1:K)
      gamma[1,k] <- log(prior[k]) + log(phi[k,u[i,1]]);
    for (t in 2:T_unsup) {
      for (k in 1:K) {
        thetareg[k] <- theta_alpha[k] + theta_beta * age[i,t];
        theta[k] <- softmax(thetareg[k]);
      }
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    increment_log_prob(log_sum_exp(gamma[T_unsup]));
  }
}
