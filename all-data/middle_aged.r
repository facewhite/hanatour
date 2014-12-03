library("data.table")
library("rstan")
library("reshape2")

setwd("C:/Users/Chanhee/Documents/GitHub/hanatour/all-data")

S1 <- 3
S2 <- 8

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

code_region <- function(regionname) {
    regionlist <- list("A","C","E","F","H","J","P","S","X")
    return(grep(regionname, regionlist))
}

# MAC
#t1 <- data.table(read.csv("~/Documents/hanatour/hanatour/over10.csv")) # 40943 records

# Windows
t1 <- data.table(read.csv("./over5.csv")) # 293310 records
t2 <- t1[attr_code=="P"] # 194277
t2Tab <- table(t2$cust_no)
t3 <- t2[cust_no %in% names(t2Tab[t2Tab>5])] # 18603 records

#travel_length <0 
t3[package_code=="AEP174051102PRF"]$travel_length <- 3
t3[package_code=="CSP840040804IN"]$travel_length <- 4
t3[package_code=="CSP583030319KE"]$travel_length <- 5
t3[package_code=="AEP108030702OZ"]$travel_length <- 5
t3[package_code=="JNP510110210KEU"]$travel_length <- 4
t3[package_code=="AEP151050603KEH"]$travel_length <- 4

#travel_length >= 30
t3[package_code=="JNP510110210KEU"]$travel_length <- 14
t3[package_code=="SSP286091109OZ"]$travel_length <- 14
t3[package_code=="AEP135010212CX"]$travel_length <- 6
t3[package_code=="CNP812050101CA"]$travel_length <- 5
t3[package_code=="AEP108030702OZ"]$travel_length <- 5
t3[package_code=="CNP812040421CA"]$travel_length <- 5
t3[package_code=="AEP172010812KE"]$travel_length <- 4
t3[package_code=="AEP172010805KE"]$travel_length <- 4
t3[package_code=="AEP172010810KE"]$travel_length <- 4
t3[package_code=="AEP110011207KE"]$travel_length <- 3
t3[package_code=="AEP144011212SQ"]$travel_length <- 4 # travel_length = 23

t3[age==100]$age <- 0

t3 <- t3[!is.na(gender)] # 135988 records
# samples middle aged groups
t4 <- t3[age>=40]
t4 <- t4[age<=60]

t5 <- t3[cust_no %in% unique(t4$cust_no)]

#t2 <- t1[total_n <= maxTravel] # 39994 records
#tt <- t2[cust_no %in% sample(unique(t2$cust_no), 1000)] # sample 1000 customers

levels(t5$next_area)[match("END",levels(t5$next_area))] <- "XX"
levels(t5$prev_area)[match("FIRST",levels(t5$prev_area))] <- "XX"

t5$booking_type <- factor(t5$booking_type)
t5$package_code <- factor(t5$package_code)
t5$area_code <- factor(t5$area_code)
#t5$accos <- factor(t5$accos)
t5$province <- factor(t5$province)
t5$gender <- factor(t5$gender)
t5$booking_path <- factor(t5$booking_path)
#t5$cust_no <- factor(t5$cust_no)
t5$seq <- factor(t5$seq)

t5$regions <- sapply(substring(t5$area_code,1,1),code_region)
t5$next_regions <- sapply(substring(t5$next_area,1,1),code_region)
t5$prev_regions <- sapply(substring(t5$prev_area,1,1),code_region)

# for (i in inds){
#     records <- t5[cust_no == i]
#     records <- records[with(records, order(cust_no, nth))]
# }

#t5 <- t5[with(t5, order(cust_no, nth))]


region_matrix <- remove_na(acast(t5,cust_no~nth,value.var="regions"),S2+1)
age_matrix <- remove_na(acast(t5,cust_no~nth,value.var="age"),-1)
accos_matrix <- remove_na(acast(t5,cust_no~nth,value.var="acco_no"),-1)
#next_regions_matrix <- remove_na(acast(t5,cust_no~nth,value.var="next_regions"),9)

tl_matrix <- remove_na(acast(t5,cust_no~nth,value.var="travel_length"),-1)
nth_matrix <- remove_na(acast(t5,cust_no~nth,value.var="nth"),-1)
package_amt_matrix <- remove_na(acast(t5,cust_no~nth,value.var="package_amt"),-1)

prev_regions_matrix <- array(dim=c(nrow(region_matrix),ncol(region_matrix),S2+2))
freq_matrix <- array(dim=c(nrow(region_matrix),ncol(region_matrix),S2))
#temp_prev <- remove_na(acast(t3,cust_no~nth,value.var="prev_regions"),10)

#next_regions_matrix <- array(dim=c(nrow(region_matrix),ncol(region_matrix),10))
#temp_next <- remove_na(acast(t3,cust_no~nth,value.var="next_regions"),10)

tnum <- table(t5$cust_no)

for (r in 1:nrow(region_matrix)) {
    for (c in 1:ncol(region_matrix)) {
        if (c == 1) {
            prev_regions_matrix[r,c,] <- c(rep(0,S2),1,0)
        } else if (c <= tnum[r]){
            prev_regions_matrix[r,c,] <- c(rep(0,region_matrix[r,c-1]-1),1,rep(0,S2+2-region_matrix[r,c-1]))
        } else {
            prev_regions_matrix[r,c,] <- c(rep(0,S2+1),1)
        }
    }
}

for (r in 1:nrow(region_matrix)) {
    for (c in 1:ncol(region_matrix)) {
        if (c == 1) {
            freq_matrix[r,c,] <- rep(0,S2)
        } else if (c <= tnum[r]){
            freq_matrix[r,c,] <- (freq_matrix[r,c-1,]*(c-2)
                                  + c(rep(0,region_matrix[r,c-1]-1),
                                      1,rep(0,S2-region_matrix[r,c-1])))/(c-1)
        } else {
            freq_matrix[r,c,] <- rep(0,S2)
        }
    }
}


I <- length(unique(t5$cust_no))

I_temp <- 200

samplelist <- sample(1:I,I_temp)

# going on

stan_semisup_region <- list (T_unsup = max(tnum), K = S1, V = S2,
                           u = region_matrix[samplelist,],
                           I=I_temp,
                           age=age_matrix[samplelist,],
                           accos=accos_matrix[samplelist,],
                           T=tnum[samplelist],
                           tl=tl_matrix[samplelist,],
                           pkg_amt=package_amt_matrix[samplelist,],
                           nth=nth_matrix[samplelist,],
                           prev=prev_regions_matrix[samplelist,,],
                           p_prior=rep(1,S1)
                           )

result_theta_age <- stan('./20141130-theta=age,tl,nth,prev-phi=accos,tl,pkg_amt,age.stan',data=stan_semisup_region,iter=200,chains=1,init=0)
result_theta_age <- stan('./20141130-theta=age,tl,nth-phi=accos,tl,pkg_amt,age.stan',data=stan_semisup_region,iter=2000,chains=1,init=0)
