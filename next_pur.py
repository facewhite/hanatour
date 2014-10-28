from datetime import date, timedelta, datetime
import time
import _mysql

# initial list and dictionaries
order_dict = {}
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
    cn = b['cust_no']

    # if cust_no is encountered first,
    if cn not in order_dict:
        order_dict[cn] = {}
        order_dict[cn][b['nth']] = b['area_code']
    else:
        order_dict[cn][b['nth']] = b['area_code']


for i in xrange(bkg.num_rows()):
    print i, '/', bkg.num_rows()
    b = bkg_all[i][0]
    if varlist == []:
        varlist = b.keys()
    cn = b['cust_no']
    # write booking rows to file
    writeline = ''
    for k in varlist:
        writeline += str(b[k]) + ','
    writeline = writeline[:-1] + '\n'

    write_file.write(writeline)
