data {
    int K;
    int V;
    int T_unsup;
    int I;
    int T[I];
    int<lower=1,upper=V+1> u[I,T_unsup];
    vector[K] p_prior;

    real age[I,T_unsup];
    real accos[I,T_unsup];
    real tl[I,T_unsup];
    real pkg_amt[I,T_unsup];
    real nth[I,T_unsup];
}

parameters {
    simplex[K] p; // state prob
    vector[V] age_beta[K];
    vector[V] alpha[K];
}

model {
    p ~ dirichlet(p_prior);

    for (k in 1:K) {
        alpha[k] ~ normal(0,0.1);
        age_beta[k] ~ normal(0,0.1);
    }

    for (i in 1:I) {
        real gamma[T[i],K];
        real acc[K];

        vector[V] regs; // state * output
        row_vector[V] reg; // output prob

        for (k in 1:K) {
            regs <- exp(alpha[k]
                        + age[i,1] * age_beta[k]
                        );
            regs <- softmax(regs);
            gamma[1,k] <- log(regs[u[i,1]]);

            for (t in 2:T[i]) {
                regs <- exp(alpha[k]
                        + age[i,1] * age_beta[k]
                        );
                regs <- softmax(regs);
                gamma[t,k] <- gamma[t-1,k] + log(regs[u[i,t]]);
            }

            acc[k] <- gamma[T[i],k] + log(p[k]);
        }

        increment_log_prob(log_sum_exp(acc));
    }
}
