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

getSubTab <- function(tab,xcol, x, ycol = "nth", y = 1) {
    result.tab <- tab[get(xcol) == x][get(ycol) == y]
    return(tab[cust_no %in% unique(result.tab$cust_no)])
}

#################### ANALYZE FUNCTIONS #####################

analyzeCustomersBy <- function(path, bkg, tblTitle, xcol, ycol = "nth",y = 1) {
    by.path <- folderAppend(path,"by")
    save.path <- folderAppend(by.path,xcol)
    bylist <- unique(booking[,get(xcol)]) # categories in xcol
    for (x in bylist) {
        subtbl <- getSubTab(bkg,xcol,x,ycol,y)
        sub.path <- folderAppend(save.path,x)
        analyzeBooking(sub.path,subtbl,paste(tblTitle,xcol,x,sep="-"))
    }
    remove(subtbl)
    gc()
}

analyzeBooking <- function(path,bkg,tblTitle) {
    if(nrow(bkg) == 0) {
        return(NULL)
    }
    #codeGradeTab(bkg)
    analyzeDesc(path,bkg,tblTitle)
    analyzeOneDimension(path,bkg)
    analyzeInteraction(path,bkg)
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

analyzeOneDimension <- function(path, bkg) {

    table.path <- folderAppend(path,"table")
    onedim.path <- folderAppend(path,"OneDimension")

    attrlist <- list("grade","regions","birth_year","nth","accos",
                     "booking_type","num_pur","birth_year","marriage","gender","attr_code", "depart_date", "age",
                     "travel", "recent_camp_email", "recent_camp_sms")

    for (att in attrlist) {
        saveTablesToCSV(onedim.path,paste(att,"Table",sep="-"),bkg,att)
    }
}

analyzeInteraction <- function(path, bkg) {
    table.path <- folderAppend(path,"table")
    inter.path <- folderAppend(table.path,"interaction")
    attrlist <- list("grade","regions","birth_year","nth","accos",
                     "booking_type","num_pur","birth_year","marriage","gender","attr_code", "depart_date", "age",
                     "travel", "recent_camp_email", "recent_camp_sms")
    for (col1 in attrlist) {
        for (col2 in attrlist) {
            if (which(attrlist==col1)<which(attrlist==col2)) {
                saveTablesToCSV(inter.path, paste(col1, toupper(col2), "table",sep =""), booking, col1, col2)
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
booking <- data.table(sqlFetch(con,"bkg_final")) # only with products with grade information.
booking <- booking[area_code != "AK"] #remove domestic travels
booking[,accos:=factor(acco_no)] # add length of the journey
booking[,regions:=substring(area_code,1,1)]
gc()

folderpath <- isFolderExist("d:/Google\ Drive/codes&share/right now/analysis-140616")
tableTitle <- "booking_grade_final_140616"

analyzeCustomersBy(folderpath,booking,tableTitle,"regions")
analyzeCustomersBy(folderpath,booking,tableTitle,"booking_type")
analyzeCustomersBy(folderpath,booking,tableTitle,"grade")
analyzeBooking(folderpath,booking,tableTitle)

byAnalyze(folderpath,booking,tableTitle) # whole table
byAnalyze(folderAppend(folderpath,"packageOnly"),booking[attr_code=="P"],tableTitle)
byAnalyze(folderAppend(folderpath,"numOver2"),booking[num_pur>2],tableTitle)

analyzeSegment(folderpath,booking,tblTitle=tableTitle)
analyzeSegment(folderAppend(folderpath,"packageOnly"),booking[attr_code=="P"],tblTitle=tableTitle)
analyzeSegment(folderAppend(folderpath,"numOver2"),booking[num_pur>2],tblTitle=tableTitle)
