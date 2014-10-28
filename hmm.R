library("RODBC")
library("data.table")
library("ggplot2")
library("scales")
library("gdata")
require("depmixS4")

####################### READING DATA FROM MYSQL ##########################
con <- odbcConnect("hanatour",uid='root',pwd='299792458')

# Fetch tables and put on the list with their names
booking <- data.table(sqlFetch(con,"bkg_camp_ver4")) # only with products with grade information.
booking <- booking[area_code != "AK"] #remove domestic travels
booking <- booking[travel_length > 0] #remove erratic bookings with travel length <= 0
booking <- booking[attr_code == "P"] #remove all bookings other than package
booking[,accos:=factor(acco_no)] # add length of the journey
booking[,regions:=factor(substring(area_code,1,1))]
booking[,next_regions:=factor(substring(next_area,1,1))]
booking[,sms_camp_week:=floor(recent_camp_sms/7)]
booking[,email_camp_week:=floor(recent_camp_email/7)]
booking[,depart_year:=year(depart_date)]
booking[,depart_month:=month(depart_date)]
booking[,depart_ym:=as.Date(paste(depart_year,depart_month,1,sep="-"),format="%Y-%m-%d")]
booking[,booking_year:=year(booking_date)]
booking[,booking_month:=month(booking_date)]
booking[,booking_ym:=as.Date(paste(booking_year,booking_month,1,sep="-"),format="%Y-%m-%d")]
booking[booking_type=='']$booking_type <- 'None'
booking$booking_type <- factor(booking$booking_type)
booking$package_code <- factor(booking$package_code)
booking$area_code <- factor(booking$area_code)
booking$attr_code <- factor(booking$attr_code)
booking$accos <- factor(booking$accos)

gc()

############# get customers with more than 5 bookings #########

bkgOver5 <- booking[total_n > 4]

sp <- bkgOver5[cust_no %in% sample(cust_no,100000)]
spl <- bkgOver5[cust_no %in% sample(cust_no,1000)]

####################### APPLY SIMPLE HMM #########

set.seed(1) # in order to guarantee replication

mod <- depmix(response = regions ~ 1, data = spl, family = multinomial("identity"), nstates = 2, trstart = runif(4))
mod3 <- depmix(response = regions ~ 1, 
              transit = ~ depart_month,
              data = spl, family = multinomial("identity"),
              nstates = 5,
              instart = runif(5),
              ntimes = c(1,2,3,4,),
              )

fm3 <- fit(mod3, emc = em.control(maxit = 50000, rand = FALSE))
