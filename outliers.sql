select * from bkg_camp_ver5 where travel_length <0;

select * from product where code="SSP286091109OZ";

select * from bkg_camp_ver5 where area_code="AK";

select * from bkg_camp_ver5 where code in (select code from bkg_camp_ver5 where cust_no = 79114692);

select * from bkg_camp_ver5 where cust_no = 78859861;

select * from customer where cust_no =79973732;