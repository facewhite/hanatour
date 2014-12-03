data {
  int<lower=1> I;  // num unsupervised items
  int<lower=1> S1;  // num categories
  int<lower=1> S2;  // num words
  int<lower=1> J;  // num unsupervised items
  int<lower=1,upper=S2> regions[I,J]; // unsup words
  //int<lower=1, upper=2> gender[I];
  int<lower=0> accos[I,J]; // the number of accompanies of a customer at each travel
  int<lower=0> age[I,J]; // the age of a customer at each travel
  int<lower=1> tnum[I]; // the number of travel for each individuals
  //vector<lower=0>[S2] beta;
}
parameters {
  vector[S1] a_prior; // a constant in prior state distribution
  vector[S1] a2; // beta age for prior distribution
  vector[S1] a_trans[S1]; // a constant in state transition matrix
  vector[S1] a4; // beta age for state transition matrix
  vector[S2] a_outcome[S1]; // a constant in outcome probability distribution
  vector[S2] b_accos_out; // beta accompanies in outcome probability distribution
  //simplex[S2] phi[S1];    // emit probs
}

model {
  a_prior ~ normal(0,0.01);
  a2 ~ normal(0,0.01);
  a4 ~ normal(0,0.01);
  b_accos_out ~ normal(0,0.01);
  for (s1 in 1:S1) {
    a_trans[s1] ~ normal(0,0.01);
    a_outcome[s1] ~ normal(0,0.01);
  }

  //for (s1 in 1:S1) {
  //  phi[s1] ~ dirichlet(beta);
  //}

  {
    vector[S1] alpha[I];      // prior state probs
    vector[S1] theta[I,S1];  // transit probs
    vector[S2] phi[I,S1];    // emit probs
    real gamma[I,J,S1];
    vector[S1] alphareg[I];
    vector[S1] thetareg[I,S1];
    vector[S2] phireg[I,S1];

    for (i in 1:I) {
      // forward algorithm computes log p(u|...)
      real acc[S1];

      alphareg <- a_prior + a2 * age[i,1];

      alpha <- softmax(alphareg);

      for (s1 in 1:S1) {
        phireg[i,s1] <- a_outcome[s1] + b_accos_out * accos[i,1];

        phi[i,s1] <- softmax(phireg[i,s1]);

        gamma[i,1,s1] <- log(alpha[s1]) + log(phi[i,s1,regions[i,1]]);
      }

      for (t in 2:tnum[i]) {
        for (k in 1:S1) {
          thetareg[k] <- a_trans[k] + a4 * age[i,t];
          theta[k] <- softmax(thetareg[k]);
          phireg[i,k] <- a_outcome[k] + b_accos_out * accos[i,t];
          phi[i,k] <- softmax(phireg[i,k]);
          for (j in 1:S1) {
            acc[j] <- gamma[i,t-1,j] + log(theta[j,k]) + log(phi[i,k,regions[i,t]]);
          }
          gamma[i,t,k] <- log_sum_exp(acc);
        }
      }
      increment_log_prob(log_sum_exp(gamma[i,tnum[i]]));
    }
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
