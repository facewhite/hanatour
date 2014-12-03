alter table product add travel_length integer;

update product set travel_length = timestampdiff(day, start_date,arrive_date); # added travel length to the product table

select * from product;
select count(*) from product where travel_length is null; #2280
select count(*) from product where start_date is null; #0
select * from product where arrive_date is null; #2280
select * from product where arrive_date is null and numberOfPurchase != 0; #116
# 암트랙, 유레일패스, 그레이하운드, 크루즈, 각종 에어텔, ((((허니문 괌 4박5일 등...)))), 당일 국내여행, 국제학생증 등

#-------------------------------------------
# add columns to indicate msg_category in english
select distinct(msg_category) from campaign;
select * from campaign where msg_category is null; #0
select * from campaign where category is null; #0 
select * from campaign where contact_date is null; #0

alter table campaign add category varchar(12);

update campaign set category = "China" where msg_category = "중국";
update campaign set category = "SEAsia" where msg_category = "동남아";
update campaign set category = "Japan" where msg_category = "일본";
update campaign set category = "EventAlarm" where msg_category = "이벤트알림";
update campaign set category = "SouthPacific" where msg_category = "남태";
update campaign set category = "Mileage" where msg_category = "마일리지회원";
update campaign set category = "Domestic" where msg_category = "국내";
update campaign set category = "Airtel" where msg_category = "에어텔";
update campaign set category = "Ticket" where msg_category = "항공권";
update campaign set category = "Golf" where msg_category = "골프";
update campaign set category = "Europe" where msg_category = "유럽";
update campaign set category = "Honeymoon" where msg_category = "허니문";
update campaign set category = "Hotel" where msg_category = "호텔";
update campaign set category = "Countryside" where msg_category = "지방출발";
update campaign set category = "America" where msg_category = "미주";
update campaign set category = "Benefit" where msg_category = "혜택알림";

select * from campaign where cust_no = 76766604;

# load booking table with campaign from the csv file

create table booking_with_campaign like booking;

alter table booking_with_campaign add recent_camp_email integer, add recent_camp_sms integer;

load data infile 'd:\\hana tour\\data_temp\\booking_with_camp.csv'
 into table hana_tour_py.booking_with_campaign
 fields terminated by "," escaped by  "\\"
 lines terminated by "\r\n";

# add customer information to the booking to create new table
create table bkg_camp_birth as 
(select booking_with_campaign.*, customer.birth_year, customer.sex as gender,
customer.can_num as 'cust.can_num', customer.num_pur as num_pur, customer.purchaseAmount as 'cust.totalamt',
year(booking_with_campaign.booking_date) % 100 + 100 - customer.birth_year as age,
marriage
from booking_with_campaign left join customer
on booking_with_campaign.cust_no = customer.cust_no);

update bkg_camp_birth set age = age - 100 where age >100;

# indexing
ALTER TABLE `hana_tour_py`.`bkg_camp_birth` 
ADD PRIMARY KEY (`seq`, `code`, `cust_no`, `package_code`);
# summary


# add travel length and grade to the table

create table bkg_grade_length
(
seq int primary key,
package_code varchar(20),
grade varchar(4),
travel_length int
);


load data infile 'd:\\hana tour\\data_temp\\bkg_camp_birth_ver2_grade.csv'
 into table hana_tour_py.bkg_grade_length
 fields terminated by "," escaped by  "\\"
 lines terminated by "\r\n";

create table bkg_camp_ver2 as
(select bkg_camp_birth.*, grade, travel_length
from bkg_camp_birth inner join bkg_grade_length
on bkg_camp_birth.seq = bkg_grade_length.seq);

# summary
ALTER TABLE `hana_tour_py`.`bkg_camp_ver2` 
ADD PRIMARY KEY (`seq`, `cust_no`, `package_code`);

# add nth to the table
select count(distinct cust_no) from bkg_camp_birth where booking_date is null; # 755 customers
select count(*) from bkg_camp_birth where can_date is null and
cust_no in (select distinct cust_no from bkg_camp_birth where booking_date is null); #2446 bookings and 2292 bookings that is not cancelled

create table bkg_nth
(
seq int primary key,
cust_no varchar(20),
booking_date date,
nth int,
total_n int
);


load data infile 'd:\\hana tour\\data_temp\\bkg_camp_nth.csv'
 into table hana_tour_py.bkg_nth
 fields terminated by "," escaped by  "\\"
 lines terminated by "\r\n";

create table bkg_camp_ver3 as
(select bkg_camp_ver2.*, nth, total_n
from bkg_camp_ver2 inner join bkg_nth
on bkg_camp_ver2.seq = bkg_nth.seq);


ALTER TABLE `hana_tour_py`.`bkg_camp_ver3` 
ADD PRIMARY KEY (`seq`, `cust_no`, `package_code`);


create table bkg_camp_ver5 as
(select bkg_camp_ver4.*, prev_area
from bkg_camp_ver4 inner join prevarea
on bkg_camp_ver4.seq = prevarea.seq);

select count(*) from bkg_camp_ver5; # 6794904
select * from booking where seq=2447356;
select * from booking where package_code="CCP753020212MUA";
select * from product where code="CCP753020212MUA";