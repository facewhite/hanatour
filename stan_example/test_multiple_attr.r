library("data.table")
library("rstan")
library("reshape2")

setwd("C:/Users/Chanhee/Documents/GitHub/hanatour/stan_example")


ll_calc <- function(u, param,I,K,V,T_unsup,age) {
    gammal <- list()
    prior_alpha <- param$aPrior
    theta_alpha <- param$aTheta
    prior_beta <- param$priorBeta
    theta_beta <- param$thetaBeta
    phi_alpha <- param$aPhi
    phi_beta <- param$phiBeta

    theta <- matrix(rep(0,K*K),nrow=K)
    thetareg <- matrix(rep(-Inf,K*K),nrow=K)

    phi <- matrix(rep(0,K*V),nrow=K)
    phireg <- matrix(rep(-Inf,K*V),nrow=K)

    for (i in 1:I){ 
        # forward algorithm computes log p(u|...)
        gamma <- matrix(rep(-Inf,T_unsup[i]*K),nrow=T_unsup[i],byrow=TRUE)
        acc <- rep(-Inf,K)

        priorreg <- exp(prior_alpha + prior_beta * age[i,1])
        prior <- priorreg/sum(priorreg)
        for (k in 1:K) {
            phireg[k,] <- exp(phi_alpha[k,] + phi_beta * age[i,1])
            phi[k,] <- phireg[k,]/sum(phireg[k,])
        }

        for (k in 1:K)
            gamma[1,k] <- log(prior[k]) + log(phi[k,u[i,1]])
        for (t in 2:T_unsup[i]) {
            for (k in 1:K) {
                thetareg[k,] <- exp(theta_alpha[k,] + theta_beta * age[i,t])
                theta[k,] <- thetareg[k,]/sum(thetareg[k,])

                phireg[k,] <- exp(phi_alpha[k,] + phi_beta * age[i,t])
                phi[k,] <- phireg[k,]/sum(phireg[k,])
            }
            for (k in 1:K) {
                for (j in 1:K)
                    acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]])
                gamma[t,k] <- log(sum(exp(acc)))
            }
        }
        gammal <- c(gammal, log(sum(exp(gamma[T_unsup[i],]))))
    }

    return(gammal)
}

output_simulation <- function(data, S1, time, maxTravel) {
    #print(data)
    priorDist <- exp(data$aPrior + data$priorBeta * data$priorInput)
    #print(priorDist)
    priorDist <- priorDist/sum(priorDist)
    #print(priorDist)
    phi <- c()
    #print(priorDist)

    for (s1 in 1:S1) {
        phiRegT <- exp(data$aPhi[s1,] + data$phiBeta * data$phiInput[1])
        phiT <- phiRegT / sum(phiRegT)
        phi <- rbind(phi, phiT)
    }

    #print(phi)
    curDist <- priorDist
    #print(curDist)
    curState <- which(rmultinom(1,1,curDist)==1)
    #print(curState)
    output <- which(rmultinom(1,1,phi[curState,])==1)

    for (t in 2:time) {
        phi <- c()
        theta <- c()
        for (s1 in 1:S1) {
            phiRegT <- exp(data$aPhi[s1,] + data$phiBeta * data$phiInput[t])
            phiT <- phiRegT / sum(phiRegT)
            phi <- rbind(phi, phiT)

            thetaRegT <- exp(data$aTheta[s1,] + data$thetaBeta * data$thetaInput[t])
            thetaT <- thetaRegT / sum(thetaRegT)
            theta <- rbind(theta, thetaT)
        }
        #print(phi)
        #print(theta)
        curState <- which(rmultinom(1,1,theta[curState,])==1)
        output <- c(output,which(rmultinom(1,1,phi[curState,])==1))
    }



    if (length(output) < maxTravel) {
        output <- c(output,rep(3,maxTravel - length(output)))
    }

    return(output)
}

output_generation <- function(data, S1, time, maxTravel) {
    #print(data)
    priorDist <- matrix(exp(data$aPrior + data$priorBeta * data$priorInput),ncol=1)
    priorDist <- priorDist/sum(priorDist)
    phi <- c()
    #print(priorDist)

    for (s1 in 1:S1) {
        phiRegT <- data$aPhi[,s1] + data$phiBeta * data$phiInput[1]
        phiT <- exp(phiRegT)
        phiT <- phiT / sum(phiT)
        phi <- cbind(phi, phiT)
    }

    #print(phi)
    curDist <- priorDist
    output <- which(rmultinom(1,1,phi %*% curDist)==1)

    for (t in 2:time) {
        phi <- c()
        theta <- c()
        for (s1 in 1:S1) {
            phiRegT <- data$aPhi[,s1] + data$phiBeta * data$phiInput[t]
            phiT <- exp(phiRegT)
            phiT <- phiT / sum(phiT)
            phi <- cbind(phi, phiT)

            thetaRegT <- data$aTheta[,s1] + data$thetaBeta * data$thetaInput[t]
            thetaT <- exp(thetaRegT)
            thetaT <- thetaT / sum(thetaT)
            theta <- cbind(theta, thetaT)
        }
        print(phi)
        print(theta)
        curDist <- theta %*% curDist
        print(curDist)
        output <- c(output,which(rmultinom(1,1,phi %*% curDist)==1))
    }



    if (length(output) < maxTravel) {
        output <- c(output,rep(9,maxTravel - length(output)))
    }

    return(output)
}

remove_na <- function(mat, naval) {
    newmat <- c()
    numcol <- ncol(mat)
    ncolafter <- 0
    for (i in 1:nrow(mat)) {
        nomit <- na.omit(mat[i,])
        if (length(nomit) > ncolafter) {
            ncolafter = length(nomit)
        }
        #print(nomit)
        #print(numcol)
        #print(mat[i,])
        newmat <- rbind(newmat,c(nomit,rep(naval,numcol-length(nomit))))
        #print(newmat)
    }
    return(newmat[,1:ncolafter])
}

maxTravel <- 20


code_region <- function(regionname) {
    regionlist <- list("A","C","E","F","H","J","P","S")
    return(grep(regionname, regionlist))
}

# MAC
#t1 <- data.table(read.csv("~/Documents/hanatour/hanatour/over10.csv")) # 40943 records

# Windows
t1 <- data.table(read.csv("over10.csv")) # 40943 records
t1Tab <- table(t1$cust_no)
t2 <- t1[cust_no %in% names(t1Tab[t1Tab>10])] # 18603 records
t2 <- t2[cust_no %in% names(t1Tab[t1Tab<21])] # 18603 records
t2 <- t2[!is.na(gender)] # 18367 records


#t2 <- t1[total_n <= maxTravel] # 39994 records
#tt <- t2[cust_no %in% sample(unique(t2$cust_no), 1000)] # sample 1000 customers

t2$booking_type <- factor(t2$booking_type)
t2$package_code <- factor(t2$package_code)
t2$area_code <- factor(t2$area_code)
#t2$accos <- factor(t2$accos)
t2$province <- factor(t2$province)
t2$gender <- factor(t2$gender)
t2$booking_path <- factor(t2$booking_path)
#t2$cust_no <- factor(t2$cust_no)
t2$seq <- factor(t2$seq)

t2$regions <- sapply(t2$regions,code_region)
t2$next_regions <- sapply(t2$next_regions,code_region)

# for (i in inds){
#     records <- t2[cust_no == i]
#     records <- records[with(records, order(cust_no, nth))]
# }

t2 <- t2[with(t2, order(cust_no, nth))]


region_matrix <- remove_na(acast(t2,cust_no~nth,value.var="regions"),9)
age_matrix <- remove_na(acast(t2,cust_no~nth,value.var="age"),-1)
accos_matrix <- log(remove_na(acast(t2,cust_no~nth,value.var="accos"),-1))
next_regions_matrix <- remove_na(acast(t2,cust_no~nth,value.var="next_regions"),9)
tl_matrix <- remove_na(acast(t2,cust_no~nth,value.var="travel_length"),-1)
nth_matrix <- log(remove_na(acast(t2,cust_no~nth,value.var="nth"),-1))
package_amt_matrix <- remove_na(acast(t2,cust_no~nth,value.var="package_amt"),-1)

tnum <- table(t2$cust_no)

data2 <- list(aPrior = c(-0.02,0.07,-0.06), # K vector (prior state)
          aPhi = rbind(c(0.76,-0.75),c(0.98,-0.98),c(-1.58,1.58)), # K*V matrix (row-fromstate, col-tooutput)
          aTheta = rbind(c(0.47,0.03,-0.49),c(0.86,0.04,-0.89),c(-1.21,-0.20,1.40)), # K*K matrix (row-from prev_state, col-to cur_state)
          priorBeta = c(-0.17,0.65,-0.48), # K vector (prior state)
          phiBeta = c(0.11,-0.10), # V vector (to-output)
          thetaBeta = c(0.19,-0.54,0.35) # K vector (to-state)
          )
data <- list(aPrior = log(c(1,0,0)), # K vector (prior state)
          aPhi = log(rbind(c(1,0),c(0.5,0.5),c(0,1))), # K*V matrix (row-fromstate, col-tooutput)
          aTheta = log(rbind(c(0.7,0.3,0),c(0,0.9,0.1),c(0,0,1))), # K*K matrix (row-from prev_state, col-to cur_state)
          priorBeta = c(0,0.7,0.2), # K vector (prior state)
          phiBeta = c(0.3,0), # V vector (to-output)
          thetaBeta = c(0,0.2,0.7) # K vector (to-state)
         )
data_nobeta <- list(aPrior = log(c(1,0)),
          aPhi = cbind(log(c(1,0)),log(c(0,1))),
          aTheta = cbind(log(c(0.9,0.1)),log(c(0,1))),
          priorBeta = c(0,0),
          phiBeta = c(0,0),
          thetaBeta = c(0,0)
         )
S1 <- 3
S2 <- 8

I <- length(unique(t2$cust_no))

I_temp <- 200

samplelist <- sample(1:I,I_temp)
# going on

stan_semisup_region <- list (T_unsup = max(tnum), K = 3, V = 8,
                           u = region_matrix[samplelist,],
                           I=I_temp,
                           age=age_matrix[samplelist,],
                           accos=accos_matrix[samplelist,],
                           T=tnum[samplelist],
                           tl=tl_matrix[samplelist,],
                           pkg_amt=package_amt_matrix[samplelist,],
                           nth=nth_matrix[samplelist,]
                           )

result_theta_age <- stan('./theta_age.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_theta_age_nth <- stan('./theta_age_nth.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_phi_accos <- stan('./phi_accos.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_phi_accos_tl <- stan('./phi_accos_tl.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_phi_accos_tl_pkg_amt <- stan('./phi_accos_tl_pkg_amt.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_temp <- stan('./temp.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
