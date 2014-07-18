import MySQLdb
#from bisect import bisect_left


def code_grade(int_code):
    if 5 < int_code < 10:
        c = int_code - 5
    else:
        c = int_code
    return {
        1: "HQ",
        2: "MQ",
        3: "TR",
        4: "PR",
        5: "PT",
        10: "WE",
        11: "ZE",
        90: "ET",
        None: "None"
    }[c]


db = MySQLdb.connect(host='localhost', user='root', passwd='299792458', db='hana_tour_py')
print "Opening a db connection."
cur = db.cursor()
cur.execute("select seq, package_code" +
            " from bkg_camp_birth where can_date is null and booking_date is not null;")

# remove 803 rows with booking_date is null

#field_name = [i[0] for i in cur.description]
t = cur.fetchall()

print "fetching from bkg_camp_birth is completed."
print len(t), "rows are read."

cur.execute("select code, grade, travel_length from product;")

cust = cur.fetchall()

cust_dict = {a:(b,c) for a,b,c in cust}

print "fetching from product is completed."
print len(cust), "rows are read."


with open("d:/hana tour/data_temp/bkg_camp_birth_ver2_grade.csv", 'w') as w:
    cur_cn = 0
    nth = 0
    count = 0
    for item in t:
        count += 1
        if count % 100000 == 0:
            print count, "records are processed."

        writing_line = ','.join([str(i) for i in item])
        try:
            writing_line += ',' + code_grade(cust_dict[item[1]][0])
            writing_line += ',' + str(cust_dict[item[1]][1]) + '\n'
        except KeyError:
            writing_line += ',' + str('KEY')
            writing_line += ',' + str('\N') + '\n'
        writing_line = writing_line.replace('None', '\N')
        w.write(writing_line)

    print("Total " + str(count) + " records are written in the file.")
