regiondict = {'A': 'Asia',
              'C': 'China',
              'E': 'Europe',
              'F': 'Africa',
              'H': 'North America',
              'J': 'Japan',
              'P': 'Pacific',
              'S': 'South America'}

gradedict = {'HQ': 'High Quality',
             'MQ': 'Quality',
             'TR': 'Traditional',
             'PT': 'Practical',
             'PR': 'Prestige',
             'WE': 'Web Tour',
             'ZE': 'Zeus',
             'ET': 'ET',
             'None': 'None'}

folderpath = './analysis-140603/by/'


def make_summary(tlist, path, table, bywhat):
    # make summary statistics for 2-10, 11-
    for k, v in table.iteritems():
        v['2-10'] = dict(zip(tlist, [0 for j in xrange(len(tlist))]))
        v['11-'] = dict(zip(tlist, [0 for j in xrange(len(tlist))]))

    for k, v in table.iteritems():
        for i in v.keys():
            if not isinstance(i, (int, long)):
                if i == '2-10' or i == '11-':
                    continue
                else:
                    raise ValueError(str(i) + ' should be integer, "11-" or "2-10"')
            elif i < 10:
                for k2, v2 in v[i].iteritems():
                    v['2-10'][k2] += v2
            else:
                for k2, v2 in v[i].iteritems():
                    v['11-'][k2] += v2

    #make csv file for second purchase
    nths = ['2-10', '11-']
    for i in xrange(2, 11):
        nths.append(i)
    for nth in nths:
        second_write = ''
        for r in tlist:
            second_write += ',' + r

        second_write += '\n'
        for k, v in table.iteritems():
            second_write += k
            for k2, v2 in v[nth].iteritems():
                second_write += ',' + str(v2)
            second_write += '\n'

        with open(path + bywhat + str(nth) + 'th.csv', 'w') as writefile:
            writefile.write(second_write)


def summarize(bywhat, folderpath, bydict):
    path = folderpath + bywhat
    tlist = bydict.keys()
    table = dict(zip(tlist, [{} for i in xrange(len(tlist))]))

    #reading data from files
    for r in bydict.iterkeys():
        with open(path + r + '/table/' + bywhat + 'NthTable_count.csv') as csvfile:
            for line in csvfile:
                splitted = line.strip().split(',')
                if splitted[0] != '':
                    splitted[0] = splitted[0].strip('"')
                    nums = map(int, splitted)
                    table[r][nums[0]] = dict(zip(bydict, nums[1:]))
                else:
                    tlist = splitted[1:]
                    tlist = [t.strip('"') for t in tlist]

    # make summary statistics for 2-10, 11-

    for k, v in table.iteritems():
        v['2-10'] = dict(zip(tlist, [0 for j in xrange(len(tlist))]))
        v['11-'] = dict(zip(tlist, [0 for j in xrange(len(tlist))]))

    for k, v in table.iteritems():
        for i in v.keys():
            if not isinstance(i, (int, long)):
                if i == '2-10' or i == '11-':
                    continue
                else:
                    raise ValueError(str(i) + ' should be integer, "11-" or "2-10"')
            elif i < 10:
                for k2, v2 in v[i].iteritems():
                    v['2-10'][k2] += v2
            else:
                for k2, v2 in v[i].iteritems():
                    v['11-'][k2] += v2

    #make csv file for second purchase
    nths = ['2-10', '11-']
    for i in xrange(2,11):
        nths.append(i)
    for nth in nths:
        second_write = ''
        for r in tlist:
            second_write += ',' + r

        second_write += '\n'
        for k, v in table.iteritems():
            second_write += k
            for k2, v2 in v[nth].iteritems():
                second_write += ',' + str(v2)
            second_write += '\n'

        with open(path + bywhat + str(nth) + 'th.csv', 'w') as writefile:
            writefile.write(second_write)

    for v in table.itervalues():
        for v2 in v.itervalues():
            s = sum(v2)
            for k in v2.keys():
                v2[k] = round(v2[k]/float(s),2)

if __name__ == '__main__':
    #REGIONS
    regionpath = folderpath + 'regions/'
    regionlist = regiondict.keys()
    regiontable = dict(zip(regionlist, [{} for i in xrange(len(regionlist))]))

    #reading data from files
    for r in regiondict.iterkeys():
        with open(regionpath + r + '/table/regionNthTable_count.csv') as csvfile:
            for line in csvfile:
                splitted = line.strip().split(',')
                if splitted[0] != '':
                    splitted[0] = splitted[0].strip('"')
                    nums = map(int, splitted)
                    regiontable[r][nums[0]] = dict(zip(regiondict, nums[1:]))
                else:
                    regionlist = splitted[1:]
                    regionlist = [t.strip('"') for t in regionlist]

    # make summary statistics for 2-10, 11-

    for k, v in regiontable.iteritems():
        v['2-10'] = dict(zip(regionlist, [0 for j in xrange(len(regionlist))]))
        v['11-'] = dict(zip(regionlist, [0 for j in xrange(len(regionlist))]))

    for k, v in regiontable.iteritems():
        for i in v.keys():
            if not isinstance(i, (int, long)):
                if i == '2-10' or i == '11-':
                    continue
                else:
                    raise ValueError(str(i) + ' should be integer, "11-" or "2-10"')
            elif i < 10:
                for k2, v2 in v[i].iteritems():
                    v['2-10'][k2] += v2
            else:
                for k2, v2 in v[i].iteritems():
                    v['11-'][k2] += v2

    #make csv file for second purchase
    nths = ['2-10', '11-']
    for i in xrange(2,11):
        nths.append(i)
    for nth in nths:
        second_write = ''
        for r in regionlist:
            second_write += ',' + r

        second_write += '\n'
        for k, v in regiontable.iteritems():
            second_write += k
            for k2, v2 in v[nth].iteritems():
                second_write += ',' + str(v2)
            second_write += '\n'

        with open(regionpath + 'region' + str(nth) + 'th.csv', 'w') as writefile:
            writefile.write(second_write)

    for v in regiontable.itervalues():
        for v2 in v.itervalues():
            s = sum(v2)
            for k in v2.keys():
                v2[k] = round(v2[k]/float(s),2)
