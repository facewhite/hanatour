library("data.table")
library("rstan")

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

I = length(unique(t2$cust_no))
J = maxTravel
S1 = 5
S2 = 8
alpha <- rep(1,S1);
beta <- rep(0.1,S2);

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = maxTravel,
                    S1 = 3,
                    S2 = 9,
                    regions = region_matrix,
                    accos = accos_matrix,
                    age = age_matrix,
                    tnum = tnum
                    )

# 1279, 1232, 1238, 1304, 1224, 1204, 1162, 996, 998, 930 : large

# tables of the number of customer travel occasions
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28 
#336 364 310 281 264 269 272 328 450 771 465 292 178 117  91  57  36  33  21  17  12   5  10  13   6   1   4   2 
# 29  30  31  32  39  41  43  44  71 
#  5   4   1   1   1   1   1   1   1 

stan_table_simple <- list (
                    J = maxTravel,
                    S1 = 5,
                    S2 = 8,
                    regions = region_matrix[1279,],
                    accos = accos_matrix[1279,],
                    gender = genders[1279],
                    beta = rep(0.1,9)
                    )

result_age <- stan('hmm_simple.stan',data=stan_table,iter=200,chains=2,init=0)
