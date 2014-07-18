from datetime import date, timedelta, datetime
import _mysql

# initial list and dictionaries
no_camp = []
camp_dict = {}
varlist = ['seq', 'code', 'number', 'depart_date', 'booking_date',
          'booking_path', 'package_code', 'area_code', 'attr_code',
           'age_type', 'package_amt', 'charge_amt', 'net_amt', 'fee',
           'acco_no', 'cust_no', 'can_date', 'booking_type', 'province',
           'recent_camp_email', 'recent_camp_sms']
camp_dict = {}

db = _mysql.connect('localhost', 'root', '299792458', 'hana_tour_py')

db.query('''select grade, travel_length, start_date, code,
         numberOfPurchase as productNum from product
         where code = "AEQ110020614OZS"''')

r = db.store_result()

d = r.fetch_row(how=1)  # fetch as a dictionary

# Get all bookings in the booking table
db.query("select * from booking")
bkg = db.store_result()

# Get all campaign data in the campaign table
db.query('''select cust_no, channel_code, category,
         contact_date, campaign_id
         from campaign''')
find_cust = db.store_result()
camps = find_cust.fetch_row(find_cust.num_rows(),0)
for c in camps:
    if c[0] in camp_dict:
        camp_dict[c[0]].append(c)
    else:
        camp_dict[c[0]] = [c]

write_file = open('D:/hana tour/data_temp/booking_with_camp.csv','w')
lines = ''

for i in xrange(bkg.num_rows()):

    #check current progress status
    if not i % 100000:
        print i, '/', bkg.num_rows()
    b = bkg.fetch_row(how=1)[0]
    cn = b['cust_no']
    if b['booking_date'] is None:
        bkg_date = None
    else:
        bkg_date = datetime.strptime(b['booking_date'], "%Y-%m-%d")

    # if cust_no is encountered first,
    if cn in camp_dict and not bkg_date is None:
        # find most recent campaign activity
        recent_camp_email = None
        recent_camp_sms = None
        for c in camp_dict[cn]:
            cate = c[2]
            chan = c[1]
            cdate = datetime.strptime(c[3],"%Y-%m-%d")
            cid = c[4]
            time_from_camp = (bkg_date - cdate).days

            if chan == '2':
                if time_from_camp < 0:
                    continue
                elif recent_camp_email is None or time_from_camp < b['recent_camp_email']:
                    b['recent_camp_email'] = time_from_camp
                    recent_camp_email = c
            elif chan == '4':
                if time_from_camp < 0:
                    continue
                elif recent_camp_sms is None or time_from_camp < b['recent_camp_sms']:
                    b['recent_camp_sms'] = time_from_camp
                    recent_camp_sms = c

    if not 'recent_camp_email' in b:
        b['recent_camp_email'] = '\N'
    if not 'recent_camp_sms' in b:
        b['recent_camp_sms'] = '\N'

    # write booking rows to file
    writeline = ''
    for k in varlist:
        if not b[k] is None:
            writeline += str(b[k]) + ','
        else:
            writeline += '\N,'

    writeline = writeline[:-1] + '\n'

    write_file.write(writeline)

write_file.close()
