data {
  int<lower=1> S1;  // num categories
  int<lower=1> S2;  // num words
  int<lower=1> J;  // num unsupervised items
  int<lower=1,upper=S2> regions[J]; // unsup words
  int<lower=1, upper=2> gender;
  int<lower=0> accos[J];
  vector<lower=0>[S2] beta;
}
parameters {
  vector[S1] a1;
  vector[S1] a2;
  vector[S1] a3[S1];
  vector[S1] a4;
  vector[S2] b1[S1];
  vector[S2] b2;
  //simplex[S2] phi[S1];    // emit probs
}

transformed parameters {
  simplex[S1] alpha;      // prior state probs
  simplex[S1] theta[S1];  // transit probs
  simplex[S2] phi[S1];    // emit probs
  real gamma[J,S1];



  { 
    // forward algorithm computes log p(u|...)
    vector[S1] alphareg;
    vector[S1] thetareg[S1];
    vector[S2] phireg[S1];
    real acc[S1];

    alphareg <- a1 + a2 * gender;

    alpha <- softmax(alphareg);

    for (s1 in 1:S1) {
      thetareg[s1] <- a3[s1] + a4 * gender;
      phireg[s1] <- b1[s1] + b2 * accos[1];
    }

    for (s1 in 1:S1) {
      theta[s1] <- softmax(thetareg[s1]);
      phi[s1] <- softmax(phireg[s1]);
    }


    for (k in 1:S1)
      gamma[1,k] <- log(alpha[k]) + log(phi[k,regions[1]]);

    for (t in 2:J) {
      for (k in 1:S1) {
        phireg[k] <- b1[k] + b2 * accos[t];
        phi[k] <- softmax(phireg[k]);
        for (j in 1:S1) {
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,regions[t]]);
        }
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
  }
}

model {
  a1 ~ normal(0,0.01);
  a2 ~ normal(0,0.01);
  a4 ~ normal(0,0.01);
  b2 ~ normal(0,0.01);
  for (s1 in 1:S1) {
    a3[s1] ~ normal(0,0.01);
    b1[s1] ~ normal(0,0.01);
  }

  //for (s1 in 1:S1) {
  //  phi[s1] ~ dirichlet(beta);
  //}

  increment_log_prob(log_sum_exp(gamma[J]));
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
