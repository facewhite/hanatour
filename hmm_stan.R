library("data.table")
library("rstan")

maxTravel <- 29


code_region <- function(regionname) {
    regionlist <- list("A","C","E","F","H","J","P","S")
    return(grep(regionname, regionlist))
}

table_to_matrix <- function(table) {
    
}

t1 <- data.table(read.csv("~/Documents/hanatour/hanatour/over10.csv")) # 40943 records
t1$booking_type <- factor(t1$booking_type)
t1$package_code <- factor(t1$package_code)
t1$area_code <- factor(t1$area_code)
#t1$accos <- factor(t1$accos)
t1$province <- factor(t1$province)
t1$gender <- factor(t1$gender)
t1$booking_path <- factor(t1$booking_path)
t1$cust_no <- factor(t1$cust_no)
t1$seq <- factor(t1$seq)

t2 <- t1[total_n<=maxTravel] # 39994 records

inds <- unique(t2$cust_no)

region_matrix <- matrix()
for (i in inds){
    records <- t2[cust_no == i]
    records <- records[with(records, order(nth))]

}

rs <- sapply(t2$regions,code_region)

stan_table <- list (I = length(unique(t2$cust_no)),
                    J = max(t2$total_n),
                    regions
                    )

model_ver1 <- '
model {
    
}'
