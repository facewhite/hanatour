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

t2 <- t1[total_n <= maxTravel] # 39994 records
tt <- t2[cust_no %in% sample(unique(t2$cust_no), 1000)] # sample 1000 customers

tt$booking_type <- factor(tt$booking_type)
tt$package_code <- factor(tt$package_code)
tt$area_code <- factor(tt$area_code)
#tt$accos <- factor(tt$accos)
tt$province <- factor(tt$province)
tt$gender <- factor(tt$gender)
tt$booking_path <- factor(tt$booking_path)
tt$cust_no <- factor(tt$cust_no)
tt$seq <- factor(tt$seq)

tt$regions <- sapply(tt$regions,code_region)

region_matrix <- matrix(ncol=maxTravel)
accos_matrix <- matrix(ncol=maxTravel)

# for (i in inds){
#     records <- tt[cust_no == i]
#     records <- records[with(records, order(cust_no, nth))]
# }

tt <- tt[with(tt, order(cust_no, nth))]
custbuf <- 0
regionbuf <- vector()
accos <- vector()

for (rownum in 1:nrow(tt)) {
    row <- tt[rownum,]
    cno <- row$cust_no
    region <- row$regions
    accos <- row$accos

    if (custbuf != cno) {
        custbuf = cno

        regionbuf <- c(regionbuf,rep(0,maxTravel-length(regionbuf)))
        region_matrix <- rbind(region_matrix, regionbuf)
        regionbuf <- region

        accosbuf <- c(accosbuf, rep(0,maxTravel-length(accosbuf)))
        accos_matrix <- rbind(accos_matrix, accosbuf)
        accosbuf <- accos
    } else {
        regionbuf <- c(regionbuf, region)
        accosbuf <- c(accosbuf, accos)
    }

}

regionbuf <- c(regionbuf,rep(0,maxTravel-length(regionbuf)))
region_matrix <- rbind(region_matrix, regionbuf)

region_matrix <- region_matrix[-1,]
region_matrix <- region_matrix[-1,]

acccosbuf <- c(acccosbuf,rep(0,maxTravel-length(acccosbuf)))
acccos_matrix <- rbind(acccos_matrix, acccosbuf)

acccos_matrix <- acccos_matrix[-1,]
acccos_matrix <- acccos_matrix[-1,]

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = maxTravel,
                    regions = region_matrix,
                    accos = accos_matrix
                    )

model_ver1 <- '
model {
    
}'
