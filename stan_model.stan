data {
    int<lower=0> I; // number of individuals
    int<lower=0> J; // number of travels
    int<lower=0> S1; // number of latent state
    int<lower=0> S2; // number of observed state
    int<lower=0, upper=1> gender[I]
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
    real condprob[J,S1];
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
    real priorreg[S1]; // prior regression formula
    real a1[S1]; // the constant
    real alpha1[S1]; // the coefficient for covariates

    //the hidden state transition probability
    row_vector[S1] condprob[I,J] // conditional probability of S1 at J
    real condreg[I,J,S1,S1]; // conditional regression formula [conditional, given]
    matrix[S1,S1] transmat[I,J] // transitional matrix [given, conditional]
    real a2[S1]; // the constant 
    // real a3[S1,S1]; // the coefficient for possible latent states in the time before [conditional,conditional]
    real alpha2[S1]; // the coefficient for covariates
    // no interaction term right now

    // observed state probability from latent state
    matrix[S1,S2] outcondmat[I,J]; // outcome probability
    real outreg[I,j,S1,S2] // outcome regression formula [conditional, given]
    row_vector[S2] outprob[I,J]
    real b1[S2]; // the constant
    // real b2[S2,S1]; // the coefficient for possible latent states [conditional, given]
    real gamma[S2]; // the coefficient for covariates
    // no interaction term

    real<lower=0> phi1;
    real<lower=0> phi2;
    
    phi1 <- 1 / inv_phi1;
    phi2 <- 1 / inv_phi2;

}

model {

    inv_phi1 ~ gamma(sigma, m);
    inv_phi2 ~ gamma(sigma, m);
    U1 ~ normal(0, phi1);
    U2 ~ normal(0, phi2);
    a1 ~ 

    for (i in 1:I) {
        for (s1 in 1:(S1-1)) {
            priorreg[s1] <- a1[s1] + alpha1[s1] * gender[i] + U1;
        }
        priorreg[S1] <- 0 // reference latent state
        condreg[i,1] <- softmax(priorreg)

        for (j in 2:J) {
            for (s1 in 1:S1) {
                for (s1after in 1:(S1-1)) {
                    condreg[i,j,s1,s1after] <- a2[s1after] + alpha2[s1after] * gender[i] + U1;
                }
                condreg[i,j,s1,S1] <- 0 // reference latent state
                transmat[i,j,s1] <- softmax(condreg[i,j,s1]) // transitional matrix

                for (s2 in 1:(S2-1)) {
                    outreg[i,j,s1,s2] <- b1[s2] + gamma[s2] * accos[i,j] + U2;
                }
                outreg[i,j,s1,S2] <- 0 // reference observed state
                outcondmat[i,j,s1] <- softmax(outreg[i,j,s1]) // output conditional matrix
            }

            condprob[i,j] <- condprob[i,j-1] * transmat[i,j]
            outprob[i,j] <- condprob[i,j] * outcondmat[i,j]

            regions[i,j] ~ categorical(outprob[i,j])
        }
    }
}
