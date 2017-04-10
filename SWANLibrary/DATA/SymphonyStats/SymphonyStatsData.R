library(readxl)

setwd("C:/Path/To/Working/Directory")

members <- read.csv("SWANLibraryMembers.csv")

monthdata_compile <- function(libmonth, libyear) {
  
    month_name <- month.name[libmonth]
    month_number <- ifelse(nchar(as.character(libmonth))==1, 
                           paste0('0', as.character(libmonth)), as.character(libmonth))
    
    file_path <- paste0(libyear, "/", libyear, "-", month_number)
    files <- list.files(path=file_path, pattern=".xls|.xlsx")
    files <- Filter(function(f) !grepl("recip|renewal|internet|opac|compare|Symph", f), files)
    
    #####################
    ### INITIAL IMPORT
    #####################
    dfList <- lapply(files, function(f){
      read_excel(paste(file_path, f, sep="/"))
    })
    dfList <- setNames(dfList, gsub("[0-9].*[\\.xls|\\.xlsx]", "", files))
    
    
    #####################
    ### CLEAN UP
    #####################
    
    # REMOVE TOTALS ROW
    dfList <- lapply(dfList, function(df) {
      df[!grepl("Total|TOTAL", df[,1]),]
    })
    
    # REMOVE SUBSECTION ROWS (ADULT, JUVENILE, ETC.)
    for(t in names(dfList)[c(1:2,4:6)]) {
      dfList[[t]][,1] <- trimws(dfList[[t]][,1])
      dfList[[t]] <- dfList[[t]][nchar(dfList[[t]][,1])==3,]
      rownames(dfList[[t]]) <- NULL
    }
    
    #circat
    dfList$circact <- dfList$circact[nchar(dfList$circact$`ITEM LOC CODE`) < 10 &
                                     !is.na(dfList$circact$PERCENT),]
    
    #ill
    if(is.na(names(dfList$ill)[2])) {
        names(dfList$ill) <- trimws(dfList$ill[1,])
        dfList$ill <- dfList$ill[2:nrow(dfList$ill),]  
        dfList$ill[c(3:ncol(dfList$ill))] <- sapply(dfList$ill[c(3:ncol(dfList$ill))], as.numeric)
        dfList$ill$`CIRC LIB` <- NULL
    } else {
        names(dfList$ill) <- trimws(names(dfList$ill))
        dfList$ill <- dfList$ill[2:nrow(dfList$ill),]  
        dfList$ill[c(2:ncol(dfList$ill))] <- sapply(dfList$ill[c(2:ncol(dfList$ill))], as.numeric)
    }
    
    dfList$ill <- dfList$ill[!is.na(dfList$ill[,1]),]
    names(dfList$ill)[1] <- "BorrowingLibrary"
    rownames(dfList$ill) <- NULL

    # REMOVE EMPTY COLUMNS
    dfList <- lapply(dfList, function(df) {
      df <- Filter(function(col) !is.na(min(col)), df)
    })
    
    for (col in c('mcs', 'mcsj',
                  'czs', 'czsj', 
                  'rosj', 'bzsj', 'ILL', 'max', 'ros',
                  'mjs', 'cnsj','mls','ors','orsj','ehs','ehsj',
                  'gas', 'bvs', 'bvsj')) {
      if(!(col %in% names(dfList$ill))) { 
        dfList$ill[col] <- NA
      }
    }
         
    #####################
    ### LIB ID VARIABLE
    #####################
    for (i in c(1:2,4:6)){
      dfList[[i]]$LibID <- sapply(dfList[[i]][,1], function(i) {
              paste(sapply(seq(nchar(i)), function(j) {
                           if (substr(i, j, j) %in% as.character(0:9)) {
                             n <- as.character(substr(i, j, j))
                           } else if (substr(i, j, j) %in% letters) {
                             n <- LETTERS[which(letters==substr(i, j, j))]
                           } else {
                             n <- ""
                           }
                
                           return(n)
                   })[1:3], 
                   collapse="")
        })
    }
    
    for (i in names(dfList)[grep("add", names(dfList))]) { 
      dfList[[i]]$Location <- NULL
    }
    
    # LIB ID VARIABLE
    dfList$ill$LibID <- sapply(dfList$ill[,1], function(i) substr(i, 1, 3))
    
    #####################
    ### MERGE
    #####################
    dfFinal <- Reduce(function(left, right) merge(left, right, by="LibID", all=TRUE), dfList)
    dfFinal$Month <- month_name
    dfFinal$Year <- libyear
    dfFinal <- dfFinal[c(ncol(dfFinal)-1, ncol(dfFinal), c(1:(ncol(dfFinal)-2)))]
    
    dfFinal <- merge(members, dfFinal, by="LibID", all.x=TRUE)
    dfFinal <- transform(dfFinal, Month = month_name, Year = libyear)
}

# LIBRARY DATA FRAME LISTS BY YEAR
LibDataList <- lapply(c(2011:2015), function(y) {
    lapply(c(1:12), function(m) {
           tryCatch({  
                     monthdata_compile(m, y)  
          }, warning=function(w) return(data.frame(LibID=NA)),
             error=function(e) return(data.frame(LibID=NA)))
    })
})

# FILTER OUT EMPTY DATAFRAMES
LibDataList <- lapply(LibDataList, function(yrList){
    Filter(function(i) nrow(i) > 1, yrList)
})

# ROW BIND ALL MONTHS AND YEARS
LibData <- do.call(rbind, lapply(LibDataList, function(df) do.call(rbind, df)))

rm(LibDataList)

saveRDS(LibData, "LibData.rds")
write.csv(LibData, "LibData.csv", na="")



# FIND MISSING COLUMNS
missingcolsfind <- function() {
  colsdfList <- Map(function(df, m){
    temp <- data.frame(column=names(df))
    temp[paste0('month',m)] <- max(df$Month)
    return(temp)
  }, LibDataList, c(1:12))

  colsdf <- Reduce(function(left, right) merge(left, right, by="column", all=TRUE), colsdfList)

  missingcols <- unique(unlist(sapply(colsdf, function(col) which(is.na(as.character(col)==TRUE)))))
  return(colsdf$column[missingcols])
}

print(missingcolsfind())


