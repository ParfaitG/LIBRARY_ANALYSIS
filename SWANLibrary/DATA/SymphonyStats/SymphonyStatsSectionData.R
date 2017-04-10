library(readxl)

setwd("C:/Path/To/Working/Directory")

itemcodes <- read.csv("SWANLibrarySectionCodes.csv")
members <- transform(read.csv("SWANLibraryMembers.csv"), LibID=substr(LibID, 1, 2))

###########################
### CIRCULATION DATA
###########################
monthcircdata_compile <- function(libmonth, libyear) {
  
  month_name <- month.name[libmonth]
  month_number <- ifelse(nchar(as.character(libmonth))==1, 
                         paste0('0', as.character(libmonth)), as.character(libmonth))
  
  file_path <- paste0("DATA/", libyear, "/", libyear, "-", month_number)
  files <- list.files(path=file_path, pattern="circact", full.names=TRUE)
  
  circdf <- read_excel(files[[1]])
  
  circdf$MONTH <- month_name
  circdf$YEAR <- libyear
  circdf$`ITEM LOC CODE` <- trimws(circdf$`ITEM LOC CODE`)
  circdf$SECTION <- ifelse(substr(circdf$`ITEM LOC CODE`, 4, nchar(circdf$`ITEM LOC CODE`)) %in% c("d", "n"),
                           substr(circdf$`ITEM LOC CODE`, 3, nchar(circdf$`ITEM LOC CODE`)),
                           ifelse(substr(circdf$`ITEM LOC CODE`, 4, nchar(circdf$`ITEM LOC CODE`)) == "nv",
                                  substr(circdf$`ITEM LOC CODE`, 3, nchar(circdf$`ITEM LOC CODE`)),
                                  substr(circdf$`ITEM LOC CODE`, 4, nchar(circdf$`ITEM LOC CODE`))))
  
  circdf$SECTION <- trimws(ifelse(circdf$SECTION=="", "total", circdf$SECTION))
  circdf$`ITEM LOC CODE` <- trimws(toupper(substr(circdf$`ITEM LOC CODE`, 1, 2)))
  names(circdf)[1] <- "LibID"
  
  circdf <- circdf[!is.na(circdf$LibID),]
  circdf <- Filter(function(col) !is.na(min(col)), circdf)
  
  for (col in c('PERCENT')) {
    if(!(col %in% names(circdf))) { 
      circdf[col] <- NA
    }
  }
  
  circdf <- merge(circdf, itemcodes, by="SECTION", all.x=TRUE)
  circdf <- merge(members[c("LibID", "Library")], circdf, by="LibID")
}


# LIBRARY DATA FRAME LISTS BY YEAR
SubLibCircDataList <- lapply(c(2011:2015), function(y) {
  lapply(c(1:12), function(m) {
    tryCatch({  
      monthcircdata_compile(m, y)  
    }, warning=function(w) return(data.frame(LibID=NA)),
    error=function(e) return(data.frame(LibID=NA)))
  })
})

# FILTER OUT EMPTY DATAFRAMES
SubLibCircDataList <- lapply(SubLibCircDataList, function(yrList){
  Filter(function(i) nrow(i) > 1, yrList)
})


# ROW BIND ALL MONTHS AND YEARS
SubLibCircData <- do.call(rbind, lapply(SubLibCircDataList, function(df) do.call(rbind, df)))

rm(SubLibCircDataList)

saveRDS(SubLibCircData, "SymphonyStatsSectionCircData.rds")
write.csv(SubLibCircData, "SymphonyStatsSectionCircData.csv", na="")



###########################
### MATERIALS ADD DATA
###########################
monthadddata_compile <- function(libmonth, libyear) {
  
  month_name <- month.name[libmonth]
  month_number <- ifelse(nchar(as.character(libmonth))==1, 
                         paste0('0', as.character(libmonth)), as.character(libmonth))
  
  file_path <- paste0("DATA/", libyear, "/", libyear, "-", month_number)
  files <- list.files(path=file_path, pattern="(add.*\\.xls)", full.names=TRUE)
  
  dfList <- lapply(files[1:2], function(f){
    df <- read_excel(f)
    
    names(df)[1] <- "LOCATION"
    df$LOCATION <- trimws(df$LOCATION)
    df$SECTION <- ifelse(substr(df$LOCATION, 4, nchar(df$LOCATION)) %in% c("d", "n"),
                         substr(df$LOCATION, 3, nchar(df$LOCATION)),
                         ifelse(substr(df$LOCATION, 4, nchar(df$LOCATION)) == "nv",
                                substr(df$LOCATION, 3, nchar(df$LOCATION)),
                                ifelse(substr(df$LOCATION, 2, nchar(df$LOCATION)) %in% c("sj", "sy"),
                                       sub("s", "", substr(df$LOCATION, 4, nchar(df$LOCATION))),
                                       substr(df$LOCATION, 4, nchar(df$LOCATION)))))
    
    
    df$SECTION <- ifelse(df$SECTION=="", "total", df$SECTION)
    df$LOCATION <- toupper(substr(df$LOCATION, 1, 2))
    names(df)[1] <- "LibID"
    
    df <- df[!is.na(df$LibID),]
    df <- Filter(function(col) !is.na(min(col)), df)
    
    return(df)
    
  })
  
  # MERGE ALL DFS (CIRC/BIBS/ITEMS)
  dfFinal <- Reduce(function(left, right) merge(left, right, by=c("LibID", "SECTION"), all=TRUE), dfList)
  dfFinal$MONTH <- month.name[libmonth]
  dfFinal$YEAR <- libyear
  dfFinal <- unique(dfFinal[c(ncol(dfFinal)-1, ncol(dfFinal), c(1:(ncol(dfFinal)-2)))])
  
  # MERGE ITEM CODES AND MEMBERS    
  dfFinal <- merge(dfFinal, itemcodes, by="SECTION", all.x=TRUE)
  dfFinal <- merge(members[c("LibID", "Library")], dfFinal, by="LibID")
  
}


# LIBRARY DATA FRAME LISTS BY YEAR
SubLibAddDataList <- lapply(c(2011:2015), function(y) {
  lapply(c(1:12), function(m) {
    tryCatch({  
      monthadddata_compile(m, y)  
    }, warning=function(w) return(data.frame(LibID=NA)),
    error=function(e) return(data.frame(LibID=NA)))
  })
})

# FILTER OUT EMPTY DATAFRAMES
SubLibAddDataList <- lapply(SubLibAddDataList, function(yrList){
  Filter(function(i) nrow(i) > 1, yrList)
})


# ROW BIND ALL MONTHS AND YEARS
SubLibAddData <- do.call(rbind, lapply(SubLibAddDataList, function(df) do.call(rbind, df)))

rm(SubLibAddDataList)

saveRDS(SubLibAddData, "SymphonyStatsSectionAddData.rds")
write.csv(SubLibAddData, "SymphonyStatsSectionAddData.csv", na="")


