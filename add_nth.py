import MySQLdb
from collections import Counter
#from bisect import bisect_left

db = MySQLdb.connect(host='localhost', user='root', passwd='299792458', db='hana_tour_py')
print "Opening a db connection."
cur = db.cursor()
cur.execute("""select seq, cust_no,booking_date
            from bkg_camp_birth where can_date is null
            and area_code != "AK" and
            cust_no not in
            (select distinct cust_no from bkg_camp_birth
            where booking_date is null);""")
# remove 803 rows with booking_date is null (really booking?)
# remove rows with can_date is not null (cancelled)
# remove domestic flight

#field_name = [i[0] for i in cur.description]
t = cur.fetchall()

print "fetching from booking is completed."
print len(t), "rows are read."

#cur.execute("select distinct cust_no from booking where can_date is null;")
#cust_nos = cur.fetchall()
#cn_index = field_name.index("cust_no")
cn_index = 1
#bkg_date_index = field_name.index("booking_date")
bkg_date_index = 2

#print "fetching cust_no from booking is completed."
#print len(cust_nos), "customers are read."

print "sorting t according to customer number..."
x = sorted(t, key=lambda l: (l[cn_index], l[bkg_date_index]))
y = [i[cn_index] for i in x]
z = dict(Counter(y))

# write in the file
# seq, cust_no, booking_date, nth, total_num
with open("d:/hana tour/data_temp/bkg_camp_nth.csv", 'w') as w:
    cur_cn = 0
    nth = 0
    count = 0
    for item in x:
        count += 1
        if count % 100000 == 0:
            print count, "records are processed."
        if not cur_cn == item[cn_index]:
            cur_cn = item[cn_index]
            nth = 1
        else:
            nth += 1
        writing_line = ','.join([str(i) for i in item])
        writing_line = writing_line.replace('None', '\N')
        w.write(writing_line + ',' + str(nth) + ',' + str(z[item[cn_index]]) + '\n')
