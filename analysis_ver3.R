library("RODBC")
library("data.table")
library("ggplot2")
library("scales")
library("gdata")

#FUNCTIONS

############### TABLE OPERATIONS #############
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

############### TABLE OPERATIONS #############

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

# saveTablesToCSV (string path, string filename, data.table tbl, string xcol, string ycol) returns NULL
#
# make cross-tab between two columns with count, total package amount, and average package amount
#
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

#################### TOOLS FOR SUBTABLE ####################

getSubCustTab <- function(tab,xcol, x, ycol = "nth", y = 1) {
    result.tab <- tab[get(xcol) == x][get(ycol) == y]
    return(tab[cust_no %in% unique(result.tab$cust_no)])
}

#################### ANALYZE FUNCTIONS #####################

analyzeBy <- function(path, bkg, tblTitle, xcol) {
    by.path <- folderAppend(path,"subTab")
    save.path <- folderAppend(by.path,xcol)
    bylist <- unique(booking[,get(xcol)]) # categories in xcol
    for (x in bylist) {
        subtbl <- bkg[get(xcol)==x]
        sub.path <- folderAppend(save.path,x)
        analyzeBooking(sub.path,subtbl,paste(tblTitle,xcol,x,sep="-"))
    }
    rm(subtbl)
    gc()
}

analyzeCustomersBy <- function(path, bkg, tblTitle, xcol, ycol = "nth",y = 1) {
    by.path <- folderAppend(path,"byCustomer")
    save.path <- folderAppend(by.path,xcol)
    bylist <- unique(booking[,get(xcol)]) # categories in xcol
    for (x in bylist) {
        subtbl <- getSubCustTab(bkg,xcol,x,ycol,y)
        sub.path <- folderAppend(save.path,x)
        analyzeBooking(sub.path,subtbl,paste(tblTitle,xcol,x,sep="-"))
    }
    rm(subtbl)
    gc()
}

analyzeBooking <- function(path,bkg,tblTitle) {
    if(nrow(bkg) == 0) {
        return(NULL)
    }
    #codeGradeTab(bkg)
    # num_pur indicates the total number of booking including domestic travel
    attrlist <- list("grade","regions","birth_year","nth","accos",
                     "booking_type","total_n","birth_year","gender","attr_code", "age",
                     "travel_length", "recent_camp_email", "recent_camp_sms","email_camp_week",
                     "sms_camp_week","depart_month","depart_year","depart_ym",
                     "booking_year","booking_month","booking_ym")

    analyzeDesc(path,bkg,tblTitle)
    analyzeOneDimension(path,bkg,attrlist)
    analyzeInteraction(path,bkg,attrlist)
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

analyzeOneDimension <- function(path, bkg, attrlist) {

    table.path <- folderAppend(path,"table")
    onedim.path <- folderAppend(path,"OneDimension")

    for (att in attrlist) {
        saveTablesToCSV(onedim.path,paste(att,"Table",sep="-"),bkg,att)
    }
}

analyzeInteraction <- function(path, bkg, attrlist) {
    table.path <- folderAppend(path,"table")
    inter.path <- folderAppend(table.path,"interaction")
    for (col1 in attrlist) {
        for (col2 in attrlist) {
            if (which(attrlist==col1)<which(attrlist==col2)) {
                saveTablesToCSV(inter.path, paste(col1, toupper(col2), "table",sep =""), bkg, col1, col2)
            }
        }
    }
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

####################### MAIN ##########################
# Divide the bookings by the nth booking
con <- odbcConnect("hanatour",uid='root',pwd='299792458')

# Fetch tables and put on the list with their names
booking <- data.table(sqlFetch(con,"bkg_camp_ver3")) # only with products with grade information.
booking <- booking[area_code != "AK"] #remove domestic travels
booking <- booking[travel_length > 0] #remove erratic bookings with travel length <= 0
booking[,accos:=factor(acco_no)] # add length of the journey
booking[,regions:=substring(area_code,1,1)]
booking[,sms_camp_week:=floor(recent_camp_sms/7)]
booking[,email_camp_week:=floor(recent_camp_email/7)]
booking[,depart_year:=year(depart_date)]
booking[,depart_month:=month(depart_date)]
booking[,depart_ym:=as.Date(paste(depart_year,depart_month,1,sep="-"),format="%Y-%m-%d")]
booking[,booking_year:=year(booking_date)]
booking[,booking_month:=month(booking_date)]
booking[,booking_ym:=as.Date(paste(booking_year,booking_month,1,sep="-"),format="%Y-%m-%d")]
gc()

folderpath <- isFolderExist("d:/Google\ Drive/codes&share/right now/analysis-140720")
tableTitle <- "booking_grade_final_140720"

analyzeBooking(folderpath,booking,tableTitle)
AttrBy <- list("regions", "booking_type", "grade", "depart_year", "depart_month")

# Error in attr_code at D
# Error in `[.data.table`(tbl, , .N, by = get(xcol)) : 
#   'by' appears to evaluate to column names but isn't c() or key(). Use by=list(...) if you can. Otherwise, by=eval(get(xcol)) should work. This is for efficiency so data.table can detect which columns are needed.
# In addition: There were 50 or more warnings (use warnings() to see the first 50)

for (att in AttrBy) {
    analyzeCustomersBy (folderpath, booking, tableTitle, att)
    analyzeBy (folderpath, booking, tableTitle, att)
}
