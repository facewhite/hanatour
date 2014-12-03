data {
  int<lower=1> I;  // num unsupervised items
  int<lower=1> S1;  // num categories
  int<lower=1> S2;  // num words
  int<lower=1> J;  // num unsupervised items
  int<lower=1,upper=S2+1> regions[I,J]; // unsup words
  //int<lower=1, upper=2> gender[I];
  int<lower=0> accos[I,J]; // the number of accompanies of a customer at each travel
  //int<lower=1> tnum; // the number of travel for each individuals
  //vector<lower=0>[S2] beta;
}

parameters {
  vector[S1] a_prior; // a constant in prior state distribution
  vector[S1] a_trans[S1]; // a constant in state transition matrix
  vector[S2] a_outcome[S1]; // a constant in outcome probability distribution
  vector[S2] b_accos_out; // beta accompanies in outcome probability distribution
  //simplex[S2] phi[S1];    // emit probs
}

model {
  a_prior ~ normal(0,0.01);
  b_accos_out ~ normal(0,0.01);
  for (s1 in 1:S1) {
    a_trans[s1] ~ normal(0,0.01);
    a_outcome[s1] ~ normal(0,0.01);
  }

  //for (s1 in 1:S1) {
  //  phi[s1] ~ dirichlet(beta);
  //}

  {
    vector[S1] alpha;      // prior state probs
    vector[S1] alphareg;
    vector[S1] theta[S1];  // transit probs
    vector[S1] thetareg[S1];
    vector[S2] phi[S1];    // emit probs
    vector[S2] phireg[S1];

    real gamma[I,J,S1];
    real acc[S1];

    for (i in 1:I) {
      alphareg <- a_prior;

      alpha <- softmax(alphareg);

      for (s1 in 1:S1) {
        phireg[s1] <- a_outcome[s1];

        phi[s1] <- softmax(phireg[s1]);
      }

      for (k in 1:S1)
        gamma[i,1,k] <- log(alpha[k]) + log(phi[k,regions[i,1]]);

      for (t in 2:J) {
        for (k in 1:S1) {
          thetareg[k] <- a_trans[k];
          theta[k] <- softmax(thetareg[k]);
          phireg[k] <- a_outcome[k] + b_accos_out * accos[i,t];
          phi[k] <- softmax(phireg[k]);
        }
 
        for (k in 1:S1) {
          for (j in 1:S1) {
            acc[j] <- gamma[i,t-1,j] + log(theta[j,k]) + log(phi[k,regions[i,t]]);
          }
          gamma[i,t,k] <- log_sum_exp(acc);
        }
      }
      increment_log_prob(log_sum_exp(gamma[i,J]));
    }
  }
}

generated quantities {
  int<lower=1,upper=S1> y_star[J];
  real log_p_y_star;
  for (i in 1:I){ 
    vector[S1] alpha;      // prior state probs
    vector[S1] alphareg;
    vector[S1] theta[S1];  // transit probs
    vector[S1] thetareg[S1];
    vector[S2] phi[S1];    // emit probs
    vector[S2] phireg[S1];

    // Viterbi algorithm
    int back_ptr[J,S1];
    real best_logp[J,S1];
    real best_total_logp;

    alphareg <- a_prior;

    alpha <- softmax(alphareg);

    for (s1 in 1:S1) {
      phireg[s1] <- a_outcome[s1];

      phi[s1] <- softmax(phireg[s1]);
    }

    for (k in 1:S1)
      best_logp[1,S1] <- log(alpha[k]) + log(phi[k,regions[i,1]]);
    for (t in 2:J) {
      for (k in 1:S1) {
        thetareg[k] <- a_trans[k];
        theta[k] <- softmax(thetareg[k]);
        phireg[k] <- a_outcome[k] + b_accos_out * accos[i,t];
        phi[k] <- softmax(phireg[k]);
      }

      for (k in 1:S1) {
        best_logp[t,k] <- negative_infinity();
        for (j in 1:S1) {
          real logp;
          logp <- best_logp[t-1,j] + log(theta[j,k]) + log(phi[k,regions[i,t]]);
          if (logp > best_logp[t,k]) {
            back_ptr[t,k] <- j;
            best_logp[t,k] <- logp;
          }
        }
      }
    }
    log_p_y_star <- max(best_logp[J]);
    for (k in 1:S1)
      if (best_logp[J,k] == log_p_y_star)
        y_star[J] <- k;
    for (t in 1:(J - 1))
      y_star[J - t] <- back_ptr[J - t + 1, 
                                      y_star[J - t + 1]];
  }
}
