library("RODBC")
library("data.table")
library("ggplot2")
library("scales")
library("gdata")

##########FUNCTION LIST#########
# Tools for file operations----------------------------------
# isFolderExist (string filepath) returns string
#
# if there is no folder like filepath, create filepath folder.
# return filepath in success
#
# folderAppend (string path, string fd) returns string
#
# add fd to path and return newpath "path/fd"
#
# fileAppend (string path, string filename) returns string
#
# add filename to path and return newpath "path/filename"
#
# saveTablesToCSV (string path, string filename, data.table tbl, string xcol, string ycol) returns NULL
#
# make cross-tab between two columns with count, total package amount, and average package amount
#

isFolderExist <- function(filepath) {
    if (!file.exists(filepath)){
        dir.create(file.path(filepath))
        print(filepath)
        print("Above path does not exist. Created new folder.")
        flush.console()
    }
    return(filepath)
}

folderAppend <- function(path, fd) {
    newpath <- paste(path,fd,sep="/",collapse="")
    return(isFolderExist(newpath))
}

fileAppend <- function(path, filename) {
    filepath <- paste(isFolderExist(path),filename,sep="/",collapse="")
    return(filepath)
}

saveTablesToCSV <- function(path, filename, tbl,xcol,ycol=NULL) {
    types = list("count","sum","mean")
    for (type in types) {
        saveTableToCSV(path,paste(filename,type,sep="_"),
                       makeTable(tbl,xcol,ycol,type))
    }
}

saveTableToCSV <- function(path, filename, tbl) {
    filepath <- fileAppend(path,paste(filename,"csv",sep="."))
    fileCon <- file(filepath)
    open(fileCon,'w')
    write.csv(tbl, fileCon)
    close(fileCon)
}

saveToFile <- function(path, filename, txt) {
    filepath <- fileAppend(path, filename)
    fileCon <- file(filepath)
    open(fileCon,'w')
    write(txt, fileCon)
    close(fileCon)
}

analyzeBy <- function(path, bkg, tblTitle, type) {
    regions <- list("A","C","E","F","H","J","P","S")
    switch(type,
           "region"={ # do analysis on bookings to each region (8 regions)
               analyzeByRegion(path, bkg, tblTitle)
           },
           "grade"={ # do analysis on bookings of each grade (8 grades except ZE)
               analyzeByGrade(path, bkg, tblTitle)
           },
           "booking_type"={ # do analysis on bookings of each type (16 types with blank)
           },
           "nth"={
           },
           "num_pur"={
           },
           "acco_no"={
           },
           "birth_year"={
           },
           "gender"={
           },
           "attr_code"={
           }
           )
}

analyzeByRegion <- function(path,bkg,tblTitle) {
    regions <- list("A","C","E","F","H","J","P","S")
    regionPath <- folderAppend(path,"regions")
    for (r in regions) {
        rt <- getRegionTab(bkg,r,1)
        rPath <- folderAppend(regionPath,r)
        analyzeBooking(rPath,rt,paste(tblTitle,"Region",r,sep="-"))
    }
}

analyzeByGrade <- function(path,bkg,tblTitle) {
    grades <- list("None","HQ","MQ","TR","PT","PR","WE","ZE","ET")
    gradePath <- folderAppend(path,"grades")
    for (r in grades) {
        rt <- getGradeTab(bkg,r,1)
        gPath <- folderAppend(gradePath,r)
        analyzeBooking(gPath,rt,paste(tblTitle,"Grade",r,sep="-"))
    }
}

#################################NOT YET
anlayzeByAttr <- function(path,bkg,tblTitle) {
    attrs <- list("A", "B", "C", "D", "E", "F", "G", "H",
                  "I", "J", "K", "L", "M", "N", "P", "Q",
                  "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    attrPath <- folderAppend(path,"attr")
    for (r in attrs) {
        rt <- getattrTab(bkg,r,1)
        aPath <- folderAppend(attrPath,r)
        analyzeBooking(aPath,rt,paste(tblTitle,"attr",r,sep="-"))
    }
}

#################################NOT YET
anlayzeByType <- function(path,bkg,tblTitle) {
    types <- list("A", "B", "C", "D", "E", "F", "G", "H",
                  "I", "J", "K", "L", "M", "N", "O")
    typePath <- folderAppend(path,"type")
    for (r in types) {
        rt <- gettypeTab(bkg,r,1)
        tPath <- folderAppend(typePath,r)
        analyzeBooking(aPath,rt,paste(tblTitle,"type",r,sep="-"))
    }
}

analyzeSegment <- function(path,bkg,cust=NULL,goods=NULL,tblTitle="table") {
    isFolderExist(path)
    analyzeBooking(path,bkg,tblTitle)
}

analyzeBooking <- function(path,bkg,tblTitle) {
    if(nrow(bkg) == 0) {
        return(NULL)
    }
    #codeGradeTab(bkg)
    analyzeDesc(path,bkg,tblTitle)
    analyzeGrade(path,bkg)
    analyzeNth(path,bkg)
    analyzeArea(path,bkg)
    analyzeAcco(path,bkg)

    ### How given bookings are distributed among different path.
    pathPath <- folderAppend(path,"path")

    # Histogram for each path
    g <- ggplot(bkg,aes(x=booking_path))
    gr <- g + geom_histogram(aes(weight=package_amt),binwidth=1) #Amount
    drawHistogram(gr,pathPath,"pathAmtHistogram.png")

    gr <- g + geom_histogram(binwidth=1) #Number
    drawHistogram(gr,pathPath,"path#Histogram.png")

    # Histogram for each type
    g <- ggplot(bkg,aes(x=booking_type))
    gr <- g + geom_histogram(aes(weight=package_amt)) #Amount
    drawHistogram(gr,pathPath,"typeAmtHistogram.png")

    gr <- g + geom_histogram() #Number
    drawHistogram(gr,pathPath,"type#Histogram.png")
}

################################## analyze focus on each attr
# analyzeDesc: analyze descriptive statistics of the table
# -Total number of records
# -Average amount of bookings in the table
# -Overall amount of bookings in the table
analyzeDesc <- function(path, bkg,tblTitle) {
    tablePath <- folderAppend(path,"table")
    fname <- "descTable.txt"
    write_str <- tblTitle
    #Print number of records in the table
    write_str <- paste(write_str,"Number of records",sep="\n")
    write_str <- paste(write_str,toString(nrow(bkg)),sep="\n")

    #Print the average amount bookings in the table
    write_str <- paste(write_str,"Average amount of bookings",sep="\n")
    write_str <- paste(write_str,toString(mean(as.numeric(bkg$package_amt))),sep="\n")

    #Print the overall amount bookings in the table
    write_str <- paste(write_str,"Overall amount of bookings",sep="\n")
    write_str <- paste(write_str,toString(sum(as.numeric(bkg$package_amt))),sep="\n")

    #Print above statistics in the file
    saveToFile(tablePath,"descTable.txt",
               write_str)
}

# analyzeGrade: analyze tables for each grade
# -according to grade only by frequency table
analyzeGrade <- function(path,bkg) {
    tablePath <- folderAppend(path,"table")
    saveTablesToCSV(tablePath,"gradeTable",bkg,"grade")
    saveTablesToCSV(tablePath,"gradeRegionTable",bkg,"grade","regions")
    saveTablesToCSV(tablePath,"gradeAgeTable",bkg,"grade","birth_year")
    saveTablesToCSV(tablePath,"gradeAccoTable",bkg,"grade","accos")
}

# analyzeNth: analyze tables for each nth?
# -according to nth by frequency table and correlation coefficient
analyeNth <- function(path,bkg) {
    nthPath <- folderAppend(path,"nth")

    g <- ggplot(bkg,aes(x=factor(nth)))
    gr <- g + geom_histogram(aes(weight=package_amt)) #Amount
    drawHistogram(gr,nthPath,"nthAmtHistogram.png")

    gr <- g + geom_histogram() #Number
    drawHistogram(gr,nthPath,"nth#Histogram.png")

    g <- ggplot(bkg,aes(x=factor(nth),y=package_amt))
    gr <- g + stat_summary(fun.y="mean",geom="bar")
    drawHistogram(gr,nthPath,"nthMeanHistogram.png")

    tablePath <- folderAppend(path,"table")

    #distribution of nth of the table
    saveTablesToCSV(tablePath,"nthTable",bkg,"nth")

    ### For each grade, analyze the repurchase percentage
    saveTablesToCSV(tablePath,"gradeNthTable",bkg,"nth","grade")

    ### For each age, analyze the repurchase percentage
    saveTablesToCSV(tablePath,"ageNthTable",bkg,"nth","birth_year")

    # For each acco, analyze the repurchase percentage
    saveTablesToCSV(tablePath,"accoNthTable",bkg,"nth","accos")

    ### For each region, analyze the repurchase percentage
    forbkg <- bkg[area_code != "AK"]
    print(nrow(bkg) - nrow(forbkg))
    print("domestic bookings are excluded from region analysis.")

    flush.console()
    saveTablesToCSV(tablePath,"regionNthTable",forbkg,"nth","regions")
    rm(forbkg)
    gc()
}

# analyzeArea: analyze tables for each area by frequency table
# -according to area only for foreign travel
# -according to whether domestic or not
analyzeArea <- function(path,bkg) {
    ### How given bookings are distributed around the world.
    areaPath <- folderAppend(path,"area")

    # for each big region
    regionPath <- folderAppend(areaPath,"region")

    g <- ggplot(bkg,aes(x=substring(area_code,1,1)))
    gr <- g + geom_histogram(aes(weight=package_amt)) + xlab("Region") + ylab("Purchase Amount") # Amount
    drawHistogram(gr,regionPath,"BigRegionAmtHist.png")

    gr <- g + geom_histogram() + xlab("Region") + ylab("# of Booking") # Number
    drawHistogram(gr,regionPath,"BigRegion#Hist.png")

    # see domestic travels vs. foreign travels

    g <- ggplot(bkg,aes(x=(area_code=="AK")))
    gr <- g + geom_histogram(aes(weight=package_amt)) + xlab("Domestic") + ylab("Purchase Amount") # Amount
    drawHistogram(gr,regionPath,"Domestic#Hist.png")

    gr <- g + geom_histogram() + xlab("Domestic") + ylab("# of Booking") # Number
    drawHistogram(gr,regionPath,"DomesticAmtHist.png")

    tablePath <- folderAppend(path,"table")
    saveTablesToCSV(tablePath,"regionTable",bkg,"regions")
}

# analyzeAcco: analyze tables for each accompany number
# -according to acco_no by frequency table and correlation coefficient
analyzeAcco <- function(path,bkg) {
    ### How given bookings have different # of accompanies.
    accoPath <- folderAppend(path,"AccoNo")

    g <- ggplot(bkg,aes(x=factor(acco_no)))
    gr <- g + geom_histogram(aes(weight=package_amt)) + xlab("acco") + ylab("Purchase Amount") # Amount
    drawHistogram(gr,accoPath,"AccompanyAmtHist.png")
    gc()

    gr <- g + geom_histogram() + xlab("acco") + ylab("# of Booking") # Number
    drawHistogram(gr,accoPath,"Accompany#Hist.png")
    gc()

    # For each acco, analyze the repurchase percentage
    tablePath <- folderAppend(path,"table")
    saveTablesToCSV(tablePath,"accoTable",bkg,"accos")
    saveTablesToCSV(tablePath,"accoRegionTable",bkg,"accos","regions")
}

analyzeInteraction <- function(path, bkg) {
    tablePath <- folderAppend(path,"table")
    attrlist <- list("grade","regions","birth_year","nth","accos")
    for (col1 in attrlist) {
        for (col2 in attrlist) {
            if (which(attrlist==col1)<which(attrlist==col2)) {
                saveTablesToCSV(tablePath, paste(col1, toupper(col2), "table",sep =""), booking, col1, col2)
            }
        }
    }
}

makeTable <- function(tbl,xcol,ycol=NULL,type) {
    if (type == "count") {
        return(makeCountTable(tbl,xcol,ycol))
    } else if (type == "sum") {
        return(makeSumAmtTable(tbl,xcol,ycol))
    } else if (type == "mean") {
        return(makeMeanAmtTable(tbl,xcol,ycol))
    }
}

makeCountTable <- function(tbl, xcol, ycol=NULL) {
    if (is.null(ycol)) {
        return(tbl[,.N,by=get(xcol)])
    } else {
        tbl[,count:=.N,by=list(get(xcol),get(ycol))]

        subtbl <- unique(data.table(cbind(tbl[,xcol,with=FALSE],
                                          tbl[,ycol,with=FALSE],
                                          tbl[,list(count)])))

        return(xtabs(count~get(xcol)+get(ycol),data=subtbl))
    }
}

makeSumAmtTable <- function(tbl, xcol, ycol=NULL) {
    if (is.null(ycol)) {
        return(tbl[,sum(as.numeric(package_amt)),by=get(xcol)])
    } else {
        tbl[,sumAmt:=sum(as.numeric(package_amt)),
            by=list(get(xcol),get(ycol))]
        subtbl <- data.table(cbind(tbl[,xcol,with=FALSE],
                                   tbl[,ycol,with=FALSE],
                                   tbl[,list(sumAmt)]))
        subtbl <- unique(subtbl)
        return(xtabs(sumAmt~get(xcol)+get(ycol),data=subtbl))
    }
}

makeMeanAmtTable <- function(tbl, xcol, ycol=NULL) {
    if (is.null(ycol)) {
        return(tbl[,mean(as.numeric(package_amt)),by=get(xcol)])
    } else {
        tbl[,meanAmt:=mean(as.numeric(package_amt)),
            by=list(get(xcol),get(ycol))]
        subtbl <- data.table(cbind(tbl[,xcol,with=FALSE],
                                   tbl[,ycol,with=FALSE],
                                   tbl[,list(meanAmt)]))
        subtbl <- unique(subtbl)
        return(xtabs(meanAmt~get(xcol)+get(ycol),data=subtbl))
    }
}

drawHistogram <- function(gr,path,filename) {
    ggsave(file=fileAppend(path,filename),plot=gr)
}

codeGradeTab <- function(tab) {
    if (1 %in% tab$grade) {
        tab[,grade:=codeGrade(grade)]
    }
    return(tab)
}

codeGrade <- function(grade) {
    ng <- 0
    if (grade>5 && grade<10) {
        ng <- grade-5
    }
    else {
        ng <- grade
    }
    returnValue <- ""
    if (ng == 1) {
        returnValue <- "HQ"
    } else if (ng == 2) {
        returnValue <- "MQ"
    } else if (ng == 3) {
        returnValue <- "TR"
    } else if (ng == 4) {
        returnValue <- "PR"
    } else if (ng == 5) {
        returnValue <- "PT"
    } else if (ng == 10) {
        returnValue <- "WE"
    } else if (ng == 11) {
        returnValue <- "ZE"
    } else {
        returnValue <- "ET"
    }
    return(returnValue)
}

getRegionTab <- function(tab,region,n) {
    regTab <- subset(tab,substring(area_code,1,1) == region)
    nthTab <- subset(regTab,nth==n)
    return(tab[cust_no %in% unique(nthTab$cust_no)])
}

getGradeTab <- function(tab,g,n) {
    gradeTab <- subset(tab,grade == g)
    nthTab <- subset(gradeTab,nth == n)
    return(tab[cust_no %in% unique(nthTab$cust_no)])
}

################ TO DO: MODULARIZE ################
# getTab <- function(tab,column,value,n) {
#     sub1 <- 
# }

byAnalyze <- function(path,bkg,tblTitle) {
    bypath <- folderAppend(path,"by")
    analyzeByRegion(bypath,bkg,tblTitle)
    analyzeByGrade(bypath,bkg,tblTitle)
}

# autoCorrAnalyze <- function() {
#     autoCorrRegion
#     autoCorrGrade
#     autoCorrPath
# }

#analyzeSegment("d:/hana\ tour/graphs/temp",bookingOver3)


# Divide the bookings by the nth booking
con <- odbcConnect("hanatour",uid='root',pwd='299792458')

# Fetch tables and put on the list with their names
booking <- data.table(sqlFetch(con,"bkg_gr_f")) # only with products with grade information.
booking[,accos:=factor(acco_no)]
booking[,regions:=substring(area_code,1,1)]
gc()

folderpath = isFolderExist("d:/Google\ Drive/codes&share/right_now/analysis-140613")
tableTitle = "booking_grade_final_140613"

byAnalyze(folderpath,booking,tableTitle) # whole table
byAnalyze(folderAppend(folderpath,"packageOnly"),booking[attr_code=="P"],tableTitle)
byAnalyze(folderAppend(folderpath,"numOver2"),booking[num_pur>2],tableTitle)

analyzeSegment(folderpath,booking,tblTitle=tableTitle)
analyzeSegment(folderAppend(folderpath,"packageOnly"),booking[attr_code=="P"],tblTitle=tableTitle)
analyzeSegment(folderAppend(folderpath,"numOver2"),booking[num_pur>2],tblTitle=tableTitle)
