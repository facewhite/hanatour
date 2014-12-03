library("data.table")
library("rstan")
library("reshape2")

setwd("C:/Users/Chanhee/Documents/GitHub/hanatour/stan_example")

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
t2 <- t2[travel_length<30]


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
accos_matrix <- remove_na(log(acast(t2,cust_no~nth,value.var="accos")),-1)
next_regions_matrix <- remove_na(acast(t2,cust_no~nth,value.var="next_regions"),9)
tl_matrix <- remove_na(acast(t2,cust_no~nth,value.var="travel_length"),-1)
nth_matrix <- remove_na(log(acast(t2,cust_no~nth,value.var="nth")),-1)
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

I_temp <- 1000

# going on

stan_semisup_region <- list (T_unsup = max(tnum), K = S1, V = S2,
                           u = region_matrix[1:I_temp,],
                           I=I_temp,
                           age=age_matrix[1:I_temp,],
                           accos=accos_matrix[1:I_temp,],
                           T=tnum[1:I_temp],
                           tl=tl_matrix[1:I_temp,],
                           pkg_amt=package_amt_matrix[1:I_temp,],
                           nth=nth_matrix[1:I_temp,],
                           p_prior=rep(1,S1)
                           )

oneortwo <- vector()

for (i in 1:I) {
    p <- sample(c(1,2),1,prob=c(0.7,0.3))
    if (p == 1) {
        newvect <- sample(c(1,2),tnum[i],replace=TRUE,prob=c(0.3,0.7))
    } else  {
        newvect <- sample(c(1,2),tnum[i],replace=TRUE,prob=c(0.5,0.5))
    }
    newvect <- c(newvect, rep(3,maxTravel-tnum[i]))
    oneortwo <- c(oneortwo, newvect)
}

oot_mat <- matrix(oneortwo,ncol=maxTravel,byrow=TRUE)

stan_semisup_latent <- list (T_unsup = max(tnum), K = 2, V = 2,
                           u = oot_mat[1:I_temp,],
                           I=I_temp,
                           age=age_matrix[1:I_temp,],
                           accos=accos_matrix[1:I_temp,],
                           T=tnum[1:I_temp],
                           tl=tl_matrix[1:I_temp,],
                           pkg_amt=package_amt_matrix[1:I_temp,],
                           nth=nth_matrix[1:I_temp,],
                           p_prior=rep(1,2)
                           )

result_latent3 <- stan('latent_temp.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
