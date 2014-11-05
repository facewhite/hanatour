data {
  int<lower=1> S1;  // num categories
  int<lower=1> S2;  // num words
  int<lower=1> J;  // num unsupervised items
  int<lower=1,upper=S2> regions[J]; // unsup words
  vector<lower=0>[S1] alpha;  // transit prior
  vector<lower=0>[S2] beta;   // emit prior
}
parameters {
  simplex[S1] theta[S1];  // transit probs
  simplex[S2] phi[S1];    // emit probs
}

model {
  for (k in 1:S1) 
    theta[k] ~ dirichlet(alpha);
  for (k in 1:S1)
    phi[k] ~ dirichlet(beta);

  { 
    // forward algorithm computes log p(u|...)
    real acc[S1];
    real gamma[J,S1];
    for (k in 1:S1)
      gamma[1,k] <- log(phi[k,regions[1]]);
    for (t in 2:J) {
      for (k in 1:S1) {
        for (j in 1:S1)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,regions[t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    increment_log_prob(log_sum_exp(gamma[J]));
  }
}

//generated quantities {
//  int<lower=1,upper=S1> y_star[J];
//  real log_p_y_star;
//  { 
//    // S2iterbi algorithm
//    int back_ptr[J,S1];
//    real best_logp[J,S1];
//    real best_total_logp;
//    for (k in 1:S1)
//      best_logp[1,S1] <- log(phi[k,regions[1]]);
//    for (t in 2:J) {
//      for (k in 1:S1) {
//        best_logp[t,k] <- negative_infinity();
//        for (j in 1:S1) {
//          real logp;
//          logp <- best_logp[t-1,j] + log(theta[j,k]) + log(phi[k,regions[t]]);
//          if (logp > best_logp[t,k]) {
//            back_ptr[t,k] <- j;
//            best_logp[t,k] <- logp;
//          }
//        }
//      }
//    }
//    log_p_y_star <- max(best_logp[J]);
//    for (k in 1:S1)
//      if (best_logp[J,k] == log_p_y_star)
//        y_star[J] <- k;
//    for (t in 1:(J - 1))
//      y_star[J - t] <- back_ptr[J - t + 1, 
//                                      y_star[J - t + 1]];
//  }
//}
