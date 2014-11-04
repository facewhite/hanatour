data {
    int<lower=0> I; // number of individuals
    int<lower=0> J; // number of travels
    int<lower=0> S1; // number of latent state
    int<lower=0> S2; // number of observed state
    int<lower=0, upper=1> gender[I];
    int<lower=0, upper=7> regions[I,J]; // Big regions, Asia, China, Europe, Africa etc.
    //int<lower=0, upper=50> area_code[I,J];
    int<lower=0> accos[I,J];
}

parameters {
    // latent state prior probability
    real a1[S1]; // the constant
    real alpha1[S1]; // the coefficient for covariates
    real U1; // the error term

    //the hidden state transition probability
    vector[S1-1] a2; // the constant
    // real a3[S1,S1]; // the coefficient for possible latent states in the time before [given,conditional
    vector[S1-1] alpha2; // the coefficient for covariates
    // no interaction term right now

    // observed state probability from latent state
    vector[S2-1] b1; // the constant
    // real b2[S1,S2]; // the coefficient for possible latent states [given,conditional]
    vector[S2-1] gamma; // the coefficient for covariates
    // no interaction term
    real U2; // the error term

    real<lower=0> inv_phi1;
    real<lower=0> inv_phi2;
}

transformed parameters {
    // latent state prior probability
    vector[S1] priorreg; // prior regression formula

    //the hidden state transition probability
    simplex[S1] condprob[I,J]; // conditional probability of S1 at J
    vector[S1] condreg[I,J,S1]; // conditional regression formula [conditional, given]
    matrix[S1,S1] transmat[I,J]; // transitional matrix [conditional, given]
    // real a3[S1,S1]; // the coefficient for possible latent states in the time before [conditional,conditional]
    // no interaction term right now

    // observed state probability from latent state
    matrix[S2,S1] outcondmat[I,J]; // outcome probability
    vector[S2] outreg[I,J,S1]; // outcome regression formula [conditional, given]
    simplex[S2] outprob[I,J];
    // real b2[S2,S1]; // the coefficient for possible latent states [conditional, given]
    // no interaction term

    real<lower=0> phi1;
    real<lower=0> phi2;
    
    phi1 <- 1 / inv_phi1;
    phi2 <- 1 / inv_phi2;

    for (i in 1:I) {
        for (s1 in 1:(S1-1)) {
            priorreg[s1] <- a1[s1] + alpha1[s1] * gender[i] + U1;
        }
        priorreg[S1] <- 0; // reference latent state
        condprob[i,1] <- softmax(priorreg);

        for (j in 2:J) {
            for (s1 in 1:S1) {
                head(condreg[i,j,s1],S1-1) <- a2 + alpha2 * gender[i] + U1;
                condreg[i,j,s1,S1] <- 0;
                col(transmat[i,j],s1) <- softmax(condreg[i,j,s1]); // transitional matrix

                head(outreg[i,j,s1],S2-1) <- b1 + gamma * accos[i,j] + U2;
                outreg[i,j,s1,S2] <- 0;
                col(outcondmat[i,j],s1) <- softmax(outreg[i,j,s1]); // output conditional matrix
            }

            condprob[i,j] <-  transmat[i,j] * condprob[i,j-1];
            outprob[i,j] <-  outcondmat[i,j] * condprob[i,j];
        }
    }
}

model {

    inv_phi1 ~ gamma(0.01, 0.01);
    inv_phi2 ~ gamma(0.01, 0.01);
    U1 ~ normal(0, phi1);
    U2 ~ normal(0, phi2);

    for (s1 in 1:S1) {
        a1[s1] ~ normal(0,1);
        a2[s1] ~ normal(0,1);
        alpha1[s1] ~ normal(0,1);
        alpha2[s1] ~ normal(0,1);
    }

    for (s2 in 1:S2) {
        b1[s2] ~ normal(0,1);
        gamma[s2] ~ normal(0,1);
    }

    for (i in 1:I) {
        for (j in 2:J) {
            regions[i,j] ~ categorical(outprob[i,j]);
        }
    }
}
