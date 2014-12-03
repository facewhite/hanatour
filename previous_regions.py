import _mysql

# initial list and dictionaries
order_dict = {}
varlist = ['seq', 'prev_area']

db = _mysql.connect('localhost', 'root', '299792458', 'hana_tour_py')

# Get all bookings in the booking table
db.query("select cust_no, nth, area_code from bkg_camp_ver3")
bkg = db.store_result()

print 'First storing area data'
for i in xrange(bkg.num_rows()):

    #check current progress status
    if not i % 100000:
        print i, '/', bkg.num_rows()
    b = bkg.fetch_row(how=1)[0]
    cn = b['cust_no']
    nth = int(b['nth'])

    # if cust_no is encountered first,
    if cn not in order_dict:
        order_dict[cn] = {}
        order_dict[cn][nth] = b['area_code']
    else:
        order_dict[cn][nth] = b['area_code']

db.query("select seq, cust_no, nth, total_n from bkg_camp_ver3")
bkg = db.store_result()

write_file = open('D:/hana tour/data_temp/prev_area.csv','w')

print 'First writing to a file'
for i in xrange(bkg.num_rows()):

    #check current progress status
    if not i % 100000:
        print i, '/', bkg.num_rows()
    b = bkg.fetch_row(how=1)[0]
    cn = b['cust_no']
    nth = int(b['nth'])
    total_n = int(b['total_n'])

    if nth == 1:
        b['prev_area'] = 'FIRST'
    else:
        try:
            b['prev_area'] = order_dict[cn][nth - 1]
        except KeyError:
            print b['seq']
            b['prev_area'] = 'ERR'

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
