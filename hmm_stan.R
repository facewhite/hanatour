library("data.table")
library("rstan")

maxTravel <- 29


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
t2 <- t1[cust_no %in% names(t1Tab[t1Tab>10])]


#t2 <- t1[total_n <= maxTravel] # 39994 records
#tt <- t2[cust_no %in% sample(unique(t2$cust_no), 1000)] # sample 1000 customers

t2$booking_type <- factor(t2$booking_type)
t2$package_code <- factor(t2$package_code)
t2$area_code <- factor(t2$area_code)
#t2$accos <- factor(t2$accos)
t2$province <- factor(t2$province)
#t2$gender <- factor(t2$gender)
t2$booking_path <- factor(t2$booking_path)
t2$cust_no <- factor(t2$cust_no)
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

for (rownum in 1:nrow(t2)) {
    row <- t2[rownum,]
    cno <- row$cust_no
    region <- row$regions
    accos <- row$accos

    if (custbuf != cno) {
        custbuf = cno
        regionbuf <- c(regionbuf, rep(0,maxTravel - (length(regionbuf) %% maxTravel)))
        accosbuf <- c(accosbuf, rep(0,maxTravel - (length(accosbuf) %% maxTravel)))
        genders <- c(genders, row$gender)
    }

    regionbuf <- c(regionbuf, region)
    accosbuf <- c(accosbuf, accos)

}

regionbuf <- tail(regionbuf, -1 * maxTravel)
regionbuf <- c(regionbuf, rep(0,maxTravel - (length(regionbuf) %% maxTravel)))
region_matrix <- matrix(regionbuf, ncol = maxTravel, byrow=TRUE)

accosbuf <- tail(accosbuf, -1 * maxTravel)
accosbuf <- c(accosbuf, rep(0, maxTravel  -(length(accosbuf) %% maxTravel)))
accos_matrix <- matrix(accosbuf, ncol = maxTravel, byrow=TRUE)

genders <- c(tail(genders,-1), row$gender)
genders <- as.integer(genders)

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = maxTravel,
                    S1 = 5,
                    S2 = 8,
                    regions = region_matrix,
                    accos = accos_matrix,
                    gender = genders
                    )

result <- stan('stan_model.stan',data=stan_table,iter=200,chains=2,init=0)
