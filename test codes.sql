SELECT * FROM hana_tour_py.campaign;

select count(distinct cust_no) from bkg_final; #4791727
select count(*) from bkg_final where nth=1 and (gender= 0 or gender = 1); #4743500 all, 4695516 with valid gender
select count(*) from bkg_final where gender = 0 or gender = 1; #6522395
select count(*) from bkg_final; #6571903
select count(*) from bkg_final where not nth > 0;
select count(*) from bkg_final where not num_pur > 0;
select count(*) from (select distinct cust_no from bkg_final where nth = 1) t1 inner join (select distinct cust_no from bkg_final where nth>1) t2
on t1.cust_no = t2.cust_no;

select * from booking where cust_no=79082243;
select * from product where code = 'JHT500090728KE';

select count(*), count(distinct cust_no), count(distinct package_code) from bkg_camp_ver3 where recent_camp_email <100;
select * from bkg_camp_ver3 where recent_camp_email < 3;

select count(*) from campaign where cust_no = 78566478;
########################### CAMPAIGN #############################
select count(*) as camp_count from campaign group by cust_no;

select avg(t.camp_count)
from (select count(*) as camp_count, cust_no from campaign group by cust_no) t; # 7.054

select avg(t.bkg_count)
from (select count(*) as bkg_count, cust_no from bkg_camp_ver3
group by cust_no) t; # 1.37 평균 예약 수

select avg(t.bkg_count)
from (select count(*) as bkg_count, cust_no from bkg_camp_ver3
where cust_no in
(select distinct cust_no from bkg_camp_ver3
where recent_camp_email is not null)
group by cust_no) t; # 3.29 캠페인을 받은 사람들의 평균 예약 수

select avg(t.camp_count)
from (select count(*) as camp_count, cust_no from campaign 
where cust_no in
(select distinct cust_no from bkg_camp_ver3
where recent_camp_email < 100 or recent_camp_sms < 100)
group by cust_no) t; # 13.6633 프로모션 100일 이내에 예약한 사람에게의 평균 캠페인 전달 횟수

select avg(t.bkg_count)
from (select count(*) as bkg_count, cust_no from bkg_camp_ver3
where cust_no in
(select distinct cust_no from bkg_camp_ver3
where recent_camp_email < 100 or recent_camp_sms < 100)
group by cust_no) t; # 3.55 프로모션 100일 이내에 예약한 사람의 평균 예약 수

select avg(t.camp_count)
from (select count(*) as camp_count, cust_no from campaign 
where cust_no in
(select distinct cust_no from bkg_camp_ver3
where recent_camp_email < 20 or recent_camp_sms < 20)
group by cust_no) t; # 18.3460 프로모션 20일 이내에 예약한 사람에게의 평균 캠페인 전달 횟수

select avg(t.bkg_count)
from (select count(*) as bkg_count, cust_no from bkg_camp_ver3
where cust_no in
(select distinct cust_no from bkg_camp_ver3
where recent_camp_email < 20 or recent_camp_sms < 20)
group by cust_no) t; # 3.86 프로모션 20일 이내에 예약한 사람의 평균 예약 수

########################## TRAVEL LENGTH #######################
select count(*) from bkg_camp_ver3 where travel_length <= 0; # 2388
select * from bkg_camp_ver3 where travel_length = 1 and area_code like "S%";
select * from product where code = "SSP200010503";
select * from bkg_camp_ver3 where travel_length <= 0 and attr_code = "P" and package_amt = 0;
select * from bkg_camp_ver3 where package_amt = 0;
select * from product where code in (select distinct package_code from bkg_camp_ver3 where package_amt = 0);
select * from product where code="JKP550100915SFH";


############################ grade ######################
select distinct grade from bkg_camp_ver3;
select count(*) from bkg_camp_ver3 where grade is null;

############################# attr_code #################
select * from bkg_camp