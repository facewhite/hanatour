data {
  int<lower=1> K;  // num categories
  int<lower=1> V;  // num words
  int<lower=1> I;  // num inds
  int<lower=1> T[I]; // num of travel occasions for each individuals
  int<lower=1> T_unsup;  // num max unsupervised items
  int<lower=1,upper=V+1> u[I,T_unsup]; // unsup words

  real age[I,T_unsup]; // age covariates
  real accos[I,T_unsup]; // accos covariates

  real tl[I,T_unsup]; // travel length covariates
  real nth[I,T_unsup]; // nth travel covariates
  real pkg_amt[I,T_unsup]; // package price covariates

  // freq area matrix with indicator vector (e.g. (0.1,0.3,0.5,0,0,0.1,0))
  vector[V] freq[I,T_unsup];
}

parameters {
  // Phi: state에서 output이 나오는 probability distribution
  // Phi[k,v] = Prob(output=v|state=k)
  vector[V] phi_alpha[K]; // 상수
  vector[V] phi_beta_accos; // covariate for accompany number
  vector[V] phi_beta_pkg_amt; //covariate for package_amt
  matrix[V,V] phi_beta_freq[K]; // covariate for region frequency

  // Theta: state간 transition을 modelling한 probability distribution
  // Theta[k,k2] = Prob(current state = k2|previous state = k1)
  // utility가 threshold를 넘어가면 변화의 가능성이 올라감
  // 아래 상수는 utility 계산에 들어가는 상수
  real theta_alpha; //상수
  real theta_beta_age; // covariate for age
  real theta_beta_tl; // covariate for travel length
  real theta_beta_nth; // covariate for nth
  ordered[K] thres; // threshold constant k가 클수록 threshold가 높아짐
}

model {

  for(k in 1:K) {
    phi_alpha[k] ~ normal(0,0.1);
    for(v in 1:V)
      phi_beta_freq[k,v] ~normal(0,0.1);
  }
  phi_beta_accos ~ normal(0,0.1);
  phi_beta_pkg_amt ~ normal(0,0.1);
  phi_beta_age ~ normal(0,0.1);

  theta_alpha ~ normal(0,0.1);
  theta_beta_age ~ normal(0,0.1);
  theta_beta_tl ~ normal(0,0.1);
  theta_beta_nth ~ normal(0,0.1);

  for (i in 1:I){
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T[i],K];
    real util;

    vector[K] theta[K];
    vector[K] thetareg[K];
    vector[V] phi[K];
    vector[V] phireg[K];

    // output probability calculation for time 1
    for (k in 1:K) {
      phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,1]
                    + phi_beta_pkg_amt * pkg_amt[i,1]
                    + phi_beta_freq[k] * freq[i,1];
      phi[k] <- softmax(phireg[k]); // softmax(x,y) = (exp(x)/(exp(x)+exp(y)),exp(y)/(exp(x)+exp(y)))
    }

    for (k in 1:K)
      gamma[1,k] <- log(phi[k,u[i,1]]); // log likelihood of observing result u[i,1] at time 1 in state k

    for (t in 2:T[i]) {
      //utility is calculated through regression
      util <- theta_alpha
              + theta_beta_age * age[i,t] 
              + theta_beta_nth * nth[i,t] 
              + theta_beta_tl * tl[i,t];

      for (k in 1:K) {
        thetareg[k] <- exp(thres - util); // threshold - utility 가 클수록 exp가 inf에 가까워지고 따라서 logit이 1에 가까워짐
        theta[k,1] <- thetareg[k,1]/(1+thetareg[k,1]); // utility가 작으면 가장 커짐
        for (k2 in 2:(K-1))
          theta[k,k2] <- thetareg[k,k2]/(1+thetareg[k,k2]) - thetareg[k,k2-1]/(1+thetareg[k,k2-1]); // 각 threshold사이에서 균형을 잡음
        theta[k,K] <- 1 - thetareg[k,K-1]/(1+thetareg[k,K-1]); // utilty가 작으면 가장 작아짐

        // output probability distribution 계산은 앞과 같음
        phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,t]
                    + phi_beta_pkg_amt * pkg_amt[i,t]
                    + phi_beta_freq[k] * freq[i,t];
        phi[k] <- softmax(phireg[k]);
      }
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]); // 이전 state 가 j일때 현재 state 가 k 일 log likelihood는
                                                                         // 이전 state가 j일 log likelihood + 이전 state가 j일때 현재 state가 k일 log likelihood + 현재 state가 k일때 output이 u[i,t]일 log likelihood
        gamma[t,k] <- log_sum_exp(acc); // 현재 state가 k일 log likelihood는 이전 log likelihood의 log_sum_exp
                                        // log_sum_exp(x,y) = log(exp(x)+exp(y))
      }
    }
    increment_log_prob(log_sum_exp(gamma[T[i]])); // estimation의 log likelihood를 최종 log likelihood의 log_sum_exp로 더한다
  }
}

generated quantities {
  real log_prob;
  real aic;
  real bic;
  int numparam;
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
      phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,1]
                    + phi_beta_pkg_amt * pkg_amt[i,1]
                    + phi_beta_freq[k] * freq[i,1];
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

        phireg[k] <- phi_alpha[k] + phi_beta_accos * accos[i,t]
                    + phi_beta_pkg_amt * pkg_amt[i,t]
                    + phi_beta_freq[k] * freq[i,t];
        phi[k] <- softmax(phireg[k]);
      }
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]]);
        gamma[t,k] <- log_sum_exp(acc);
      }
    }
    log_prob <- log_prob + log_sum_exp(gamma[T[i]]);
  }
  numparam <- (V*4 + K*5 + K*K + K*V + K*V*V);
  aic <- 2 * numparam - 2* log_prob;
  bic <- -2 * log_prob + numparam * log(I);
}
