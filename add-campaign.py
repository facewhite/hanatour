from datetime import date, timedelta, datetime
import time
import _mysql

# initial list and dictionaries
no_camp = []
camp_dict = {}
varlist = []

db = _mysql.connect('localhost', 'root', '299792458', 'hana_tour_py')


# Get all bookings in the booking table
db.query("select * from booking")
bkg = db.store_result()
bkg_all = bkg.fetch_row(bkg.num_rows(),1)

write_file = open('D:/hana tour/data_temp/booking_with_camp.csv','w')

for i in xrange(bkg.num_rows()):

    #check current progress status
    print i, '/', bkg.num_rows()
    b = bkg_all[i][0]
    if varlist == []:
        varlist = b.keys()
        varlist.append('recent_camp_email')
        varlist.append('recent_camp_sms')
    cn = b['cust_no']
    bkg_date = datetime.strptime(b['booking_date'], "%Y-%m-%d")

    # if cust_no is encountered first,
    if cn in no_camp:
        #No campaign data
        continue
    elif not cn in camp_dict:
        db.query('''select cust_no, channel_code, category,
                 contact_date, campaign_id
                 from campaign''')
        find_cust = db.store_result()
        if find_cust.num_rows() == 0:
            #No campaign data
            no_camp.append(cn)
            continue

        camp_dict[cn] = {}
        for i in xrange(find_cust.num_rows()):
            camp = find_cust.fetch_row(how=1)[0]
            cate = camp['category']
            chan = camp['channel_code']
            cdate = datetime.strptime(camp['contact_date'], "%Y-%m-%d")
            cid = camp['campaign_id']
            camp_dict[cn][cid] = {'category': cate,
                                  'channel_code': chan,
                                  'contact_date': cdate}

    # find most recent campaign activity
    recent_camp_email = None
    recent_camp_sms = None
    for cid, c in camp_dict[cn].iteritems():
        cate = c['category']
        chan = c['channel_code']
        cdate = c['contact_date']
        time_from_camp = bkg_date - cdate

        if chan == '2':
            if time_from_camp < timedelta(0):
                continue
            elif recent_camp_email is None or time_from_camp < recent_camp_email['contact_date']:
                b['recent_camp_email'] = time_from_camp
                recent_camp_email = c
        elif chan == '4':
            if time_from_camp < timedelta(0):
                continue
            elif recent_camp_sms is None or time_from_camp < recent_camp_sms['contact_date']:
                b['recent_camp_sms'] = time_from_camp
                recent_camp_sms = c

    if not 'recent_camp_email' in b:
        b['recent_camp_email'] = ''
    if not 'recent_camp_sms' in b:
        b['recent_camp_sms'] = ''


    # write booking rows to file
    writeline = ''
    for k in varlist:
        writeline += str(b[k]) + ','
    writeline = writeline[:-1] + '\n'

    write_file.write(writeline)
