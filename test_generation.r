library("data.table")
library("rstan")

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

            thetaRegT <- data$aTheta[,s1] + data$thetaBeta * data$thetaInput
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

data <- list(aPrior = c(log(0.9),log(0.1)),
          aPhi = cbind(c(100,0,0,0,0,0,0,0),c(0,100,0,0,0,0,0,0)),
          aTheta = cbind(c(log(0.8),log(0.2)),c(-200,1)),
          priorBeta = c(0,0),
          phiBeta = c(0,0,0,0,0,0,0,0),
          thetaBeta = c(0,0)
         )
S1 <- 2
S2 <- 8

o <- c()
one <- c()
two <- c()
I <- length(unique(t2$cust_no))

for (i in 1:I) {
    priorInput <- age_matrix2[i]
    thetaInput <- age_matrix2[i]
    phiInput <- accos_matrix[i,]
    idata <- c(data,priorInput=priorInput,thetaInput=thetaInput,phiInput=list(phiInput))

    o <- c(o, output_generation(idata,S1,tnum[i],maxTravel))
    one <- c(one, rep(1,tnum[i]),rep(9,maxTravel-tnum[i]))
    timing <- sample(1:tnum[i],1)
    two <- c(two, rep(1,timing),rep(2,tnum[i]-timing),rep(9,maxTravel-tnum[i]))
    #print(o)
}

o_mat <- matrix(o,ncol=maxTravel,byrow=TRUE)
one_mat <- matrix(one,ncol=maxTravel,byrow=TRUE)
two_mat <- matrix(two,ncol=maxTravel,byrow=TRUE)

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = maxTravel,
                    S1 = 2,
                    S2 = 8,
                    regions = two_mat,
                    accos = accos_matrix,
                    age = age_matrix,
                    tnum = tnum
                    )

result_age <- stan('hmm_simple.stan',data=stan_table,iter=200,chains=2,init=0)
