library("data.table")
library("rstan")

ll_calc <- function(u, param,I,K,T_unsup,age) {
    gammal <- list()
    prior_alpha <- param$aPrior
    theta_alpha <- param$aTheta
    prior_beta <- param$priorBeta
    theta_beta <- param$thetaBeta
    phi <- exp(param$aPhi)

    theta <- matrix(rep(0,K*K),nrow=K)
    thetareg <- matrix(rep(-Inf,K*K),nrow=K)

    for (i in 1:I){ 
        # forward algorithm computes log p(u|...)
        gamma <- matrix(rep(-Inf,T_unsup*K),nrow=T_unsup,byrow=TRUE)
        acc <- rep(-Inf,K)

        priorreg <- exp(prior_alpha + prior_beta * log(age[i,1]))
        prior <- priorreg/sum(priorreg)

        for (k in 1:K)
            gamma[1,k] <- log(prior[k]) + log(phi[k,u[i,1]])
        for (t in 2:T_unsup) {
            for (k in 1:K) {
                thetareg[k,] <- exp(theta_alpha[k,] + theta_beta * log(age[i,t]))
                theta[k,] <- thetareg[k,]/sum(thetareg[k,])
            }
            for (k in 1:K) {
                for (j in 1:K)
                    acc[j] <- gamma[t-1,j] + log(theta[j,k]) + log(phi[k,u[i,t]])
                gamma[t,k] <- log(sum(exp(acc)))
            }
        }
        gammal <- c(gammal, log(sum(exp(gamma[T_unsup,]))))
    }
    return(gammal)
}

output_simulation <- function(data, S1, time, maxTravel) {
    #print(data)
    priorDist <- matrix(exp(data$aPrior + data$priorBeta * log(data$priorInput)),ncol=1)
    priorDist <- priorDist/sum(priorDist)
    phi <- c()
    #print(priorDist)

    for (s1 in 1:S1) {
        phiRegT <- data$aPhi[s1,] + data$phiBeta * log(data$phiInput[1])
        phiT <- exp(phiRegT)
        phiT <- phiT / sum(phiT)
        phi <- cbind(phi, phiT)
    }

    #print(phi)
    curDist <- priorDist
    curState <- which(rmultinom(1,1,curDist)==1)
    output <- which(rmultinom(1,1,phi[curState,])==1)

    for (t in 2:time) {
        phi <- c()
        theta <- c()
        for (s1 in 1:S1) {
            phiRegT <- data$aPhi[s1,] + data$phiBeta * log(data$phiInput[t])
            phiT <- exp(phiRegT)
            phiT <- phiT / sum(phiT)
            phi <- rbind(phi, phiT)

            thetaRegT <- data$aTheta[s1,] + data$thetaBeta * log(data$thetaInput[t])
            thetaT <- exp(thetaRegT)
            thetaT <- thetaT / sum(thetaT)
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

maxTravel <- 20


code_region <- function(regionname) {
    regionlist <- list("A","C","E","F","H","J","P","S")
    return(grep(regionname, regionlist))
}

table_to_matrix <- function(table) {
    
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

# for (i in inds){
#     records <- t2[cust_no == i]
#     records <- records[with(records, order(cust_no, nth))]
# }

t2 <- t2[with(t2, order(cust_no, nth))]
custbuf <- 0
regionbuf <- vector()
accosbuf <- vector()
genders <- vector()
agebuf <- vector()
tnum <- vector()
cust_list <- list()
count = 0

for (rownum in 1:nrow(t2)) {
    row <- t2[rownum,]
    cno <- row$cust_no
    region <- row$regions
    accos <- row$accos
    age <- row$age
    if (custbuf != cno) {
        cust_list <- c(cust_list, cno)
        custbuf <- cno

        if (count < maxTravel) {
            regionbuf <- c(regionbuf, rep(9,maxTravel - count))
            accosbuf <- c(accosbuf, rep(0,maxTravel - count))
            agebuf <- c(agebuf, rep(0,maxTravel - count))
            tnum <- c(tnum,count)
        } else {
            tnum <- c(tnum, maxTravel)
        }
        genders <- c(genders, row$gender)
        count = 1
    } else {
        count <- count + 1
        if (count > maxTravel)
            next
    }

    regionbuf <- c(regionbuf, region)
    accosbuf <- c(accosbuf, accos)
    agebuf <- c(agebuf, age)

}

tl <- length(regionbuf) %% maxTravel
if (tl != 0) {
    regionbuf <- c(regionbuf, rep(9,maxTravel - tl))
    accosbuf <- c(accosbuf, rep(0,maxTravel - tl))
    agebuf <- c(agebuf, rep(0,maxTravel - tl))
}

if (count < maxTravel) {
    tnum <- c(tnum, count)
} else {
    tnum <- c(tnum, maxTravel)
}
tnum <- tail(tnum,-1)


regionbuf <- tail(regionbuf, -1 * maxTravel)
region_matrix <- matrix(regionbuf, ncol = maxTravel, byrow=TRUE)

accosbuf <- tail(accosbuf, -1 * maxTravel)
accos_matrix <- matrix(accosbuf, ncol = maxTravel, byrow=TRUE)

agebuf <- tail(agebuf, -1 * maxTravel)
age_matrix <- matrix(agebuf, ncol = maxTravel, byrow=TRUE)

genders <- c(tail(genders,-1), row$gender)
genders <- as.integer(genders)

data2 <- list(aPrior = c(-0.08,-0.05),
          aPhi = rbind(c(-0.54,0.75),c(0.66,-0.48)),
          aTheta = rbind(c(0.47,-0.41),c(-0.50,0.54)),
          priorBeta = c(0.01,0.22),
          phiBeta = c(-0.13,-0.11),
          thetaBeta = c(0.02,0.01)
         )
data <- list(aPrior = log(c(1,0)), # K vector (prior state)
          aPhi = rbind(log(c(1,0)),log(c(0,1))), # K*V matrix (row-fromstate, col-tooutput)
          aTheta = rbind(log(c(0.9,0.1)),log(c(0,1))), # K*K matrix (row-from prev_state, col-to cur_state)
          priorBeta = c(0,0.2), # K vector (prior state)
          phiBeta = c(0.3,0), # V vector (to-output)
          thetaBeta = c(0,0.2) # K vector (to-state)
         )
data_nobeta <- list(aPrior = log(c(1,0)),
          aPhi = cbind(log(c(1,0)),log(c(0,1))),
          aTheta = cbind(log(c(0.9,0.1)),log(c(0,1))),
          priorBeta = c(0,0),
          phiBeta = c(0,0),
          thetaBeta = c(0,0)
         )
S1 <- 2
S2 <- 8

o <- c()
one <- c()
two <- c()
I <- length(unique(t2$cust_no))

for (i in 1:I) {
    priorInput <- age_matrix[i]
    thetaInput <- age_matrix[i,]
    phiInput <- accos_matrix[i,]
    idata <- c(data,priorInput=priorInput,thetaInput=list(thetaInput),phiInput=list(phiInput))

    o <- c(o, output_simulation(idata,S1,tnum[i],maxTravel))
    one <- c(one, rep(1,tnum[i]),rep(9,maxTravel-tnum[i]))
    timing <- sample(1:tnum[i],1)
    two <- c(two, rep(1,timing),rep(2,tnum[i]-timing),rep(9,maxTravel-tnum[i]))
    #print(o)
}

o_mat <- matrix(o,ncol=maxTravel,byrow=TRUE)
one_mat <- matrix(one,ncol=maxTravel,byrow=TRUE)
two_mat <- matrix(two,ncol=maxTravel,byrow=TRUE)

twovec <- c(rep(1,50),rep(2,150))
twovect <- c(rep(c(1,2),50),rep(2,100))

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = maxTravel,
                    S1 = 2,
                    S2 = 8,
                    regions = two_mat,
                    accos = accos_matrix,
                    age = age_matrix,
                    tnum = tnum
                    )

I_temp <- 100
stan_no_ind <- list (I = I_temp, J = 10, S1 = 2, S2 = 2, regions = o_mat[1:I_temp,1:10], accos=accos_matrix[1:I_temp,1:10])
result_no_ind <- stan('hmm-simple-new.stan',data=stan_no_ind,iter=200,chains=2,init=0)

stan_semisup <- list (T_unsup = maxTravel, K = 2, V = 2, u = two_mat, alpha = rep(1,2), beta = rep(0.1,2),tnum=tnum,I=I)
stan_semisup_one <- list (T_unsup = 200, K = 2, V = 2, u = twovect, alpha = rep(1,2), beta = rep(0.1,2),aprior=c(10,1))

result_semisup <- stan('hmm-semisup.stan',data=stan_semisup,iter=200,chains=2,init=0)
result_onet <- stan('hmm-semisup-orig.stan',data=stan_semisup_one ,iter=2000,chains=2,init=0)


# going on
stan_semisup_fixT <- list (T_unsup = 10, K = 2, V = 2, u = o_mat[1:I_temp,1:10], alpha = rep(1,2), aprior= c(1,1), beta = rep(0.1,2),I=I_temp, age=age_matrix[1:I_temp,1:10],accos=accos_matrix[1:I_temp,1:10])
result_fixT_100 <- stan('hmm-semisup-orig.stan',data=stan_semisup_fixT,iter=200,chains=1,init=0)
result_fixT_10 <- stan('hmm-semisup-refstate.stan',data=stan_semisup_fixT,iter=2000,chains=1,init=0)
result_fixT_10 <- stan('hmm-semisup-transpar.stan',data=stan_semisup_fixT,iter=2000,chains=1,init=0)
result_temp <- stan('temp.stan',data=stan_semisup_fixT,iter=200,chains=1,init=0)
