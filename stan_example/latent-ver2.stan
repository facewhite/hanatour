data {
    int K;
    int V;
    int T_unsup;
    int I;
    int T[I];
    real age[I,T_unsup];
    real accos[I,T_unsup];
    int<lower=1,upper=V+1> u[I,T_unsup];
    vector[K] p_prior;
}

parameters {
    simplex[K] p; // state prob
    vector[V] accos_beta[K];
    vector[V] age_beta[K];
    vector[V] alpha[K];
}

model {
    p ~ dirichlet(p_prior);
    for (k in 1:K) {
        accos_beta[k] ~ normal(0,0.1);
        age_beta[k] ~ normal(0,0.1);
        alpha[k] ~ normal(0,0.1);
    }
    for (i in 1:I) {
        real gamma[T[i],K];
        real acc[K];

        matrix[K,V] regs; // state * output
        row_vector[V] reg; // output prob

        for (k in 1:K) {
            regs[k] <- exp(accos[i,1] * accos_beta[k] + age[i,1] * age_beta[k] + alpha[k])';
            regs[k] <- softmax(regs[k]')';
            gamma[1,k] <- log(regs[k,u[i,1]]);

            for (t in 2:T[i]) {
                regs[k] <- exp(accos[i,t] * accos_beta[k] + age[i,t] * age_beta[k] + alpha[k])';
                regs[k] <- softmax(regs[k]')';
                gamma[t,k] <- gamma[t-1,k] + log(regs[k,u[i,t]]);
            }

            acc[k] <- gamma[T[i],k] + log(p[k]);
        }

        increment_log_prob(log_sum_exp(acc));
    }
}
