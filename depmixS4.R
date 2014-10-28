library("depmixS4")
library("depmix")
library("data.table")

t1 <- data.table(read.csv("over10.csv"))
t1$booking_type <- factor(t1$booking_type)
t1$package_code <- factor(t1$package_code)
t1$area_code <- factor(t1$area_code)
#t1$accos <- factor(t1$accos)
t1$province <- factor(t1$province)
t1$gender <- factor(t1$gender)
t1$booking_path <- factor(t1$booking_path)
t1$cust_no <- factor(t1$cust_no)
t1$seq <- factor(t1$seq)

mod <- dmm(nstates = 4, itemtypes = 51, stval = t1$area_code)
#mod <- depmix(response = area_code ~ 1, data = t1, nstates = 4)``
