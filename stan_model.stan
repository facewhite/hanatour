data {
    int<lower=0> I; // number of individuals
    int<lower=0> J; // number of travels
    int<lower=0> S1; // number of latent state
    int<lower=0> S2; // number of observed state
    int<lower=0, upper=7> regions[I,J]; // Big regions, Asia, China, Europe, Africa etc.
    int<lower=0, upper=50> area_code[I,J];
    int<lower=0> accos[I,J];
}

parameters {
    // latent state prior probability
    real priorprob[S1];
    real a1[S1]; // the constant
    real alpha1[S1]; // the coefficient for covariates
    real U1; // the error term

    //the hidden state transition probability
    real conditionalprob[J,S1];
    real a2[S1]; // the constant
    // real a3[S1,S1]; // the coefficient for possible latent states in the time before [given,conditional
    real alpha2[S1]; // the coefficient for covariates
    // no interaction term right now

    // observed state probability from latent state
    real outprob[I,J,S2]; // [given,conditional]
    real b1[S2]; // the constant
    // real b2[S1,S2]; // the coefficient for possible latent states [given,conditional]
    real gamma[S2]; // the coefficient for covariates
    // no interaction term
    real U2; // the error term

    real<lower=0> inv_phi1;
    real<lower=0> inv_phi2;
}

transformed parameters {
    // latent state prior probability
    real priorprob[S1];
    real a1[S1]; // the constant
    real alpha1[S1]; // the coefficient for covariates

    //the hidden state transition probability
    real conditionalprob[J,S1];
    real a2[S1]; // the constant
    // real a3[S1,S1]; // the coefficient for possible latent states in the time before [given,conditional
    // real alpha2[S1]; // the coefficient for covariates
    // no interaction term right now

    // observed state probability from latent state
    real outprob[I,J,S2]; // [given,conditional]
    real b1[S2]; // the constant
    // real b2[S1,S2]; // the coefficient for possible latent states [given,conditional]
    real gamma[S2]; // the coefficient for covariates
    // no interaction term

    real<lower=0> phi1;
    real<lower=0> phi2;
    
    phi1 <- 1 / inv_phi1;
    phi2 <- 1 / inv_phi2;

    for (s1 in 1:(S1-1)) {
        priorprob[s1] <- a1[s1] + alpha1[s1] * accos[i,1] + U1;
    }

    for (j in 2:J) {
        for (s1 in 1:(S1-1)) {
            for (s1b in 1:S1) {
                conditionalprob[s1b,s1] <- a2[s1] + U1;
            }
        }
        for (s2 in 1:S2) {
            for (s1 in 1:(S1-1)) {
                reg_formula[s2,s1] <- b1[s2] + gamma[s2] * accos[i,j] + U2;
            }
            reg_formula[s2,S1] <- 0
        }
    }

    outprob[i,j,s2] <- outprob[s2] + conditionalprob(j,S1)*softmax(reg_formula[s2])
}

model {

    inv_phi1 ~ gamma(sigma, m);
    inv_phi2 ~ gamma(sigma, m);
    U1 ~ normal(0, phi1);
    U2 ~ normal(0, phi2);

    for (i in 1:I) {
        for (j in 1:J) {
            for (s1 in 1:S1)
                for (s2 in 1:S2)
                    outprob
            regions[I,J] ~ categorical([I,J])
        }
    }
}
