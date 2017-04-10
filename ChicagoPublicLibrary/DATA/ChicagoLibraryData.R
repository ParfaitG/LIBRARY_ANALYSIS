library(reshape2)

setwd("C:/Path/To/Working/Directory")

# DATA FILES (FOUR SETS)
datafiles <- lapply(c("Circulation", "Holds", "Computer", "Visitors"), function(f){
  files <- list.files(path=f, pattern=".csv", full.names = TRUE)
  data <- lapply(files, function(d){
    df <- read.csv(d)
    names(df)[1] <- "LOCATION"
    df$LOCATION <- trimws(gsub("\\*", "", df$LOCATION))
    df$ADDRESS <- NULL; df$CITY <- NULL; df$ZIP <- NULL; df$ZIP.CODE <- NULL
    df$YTD <- NULL
    df <- df[df$LOCATION!="",]
    df$YEAR <- as.numeric(gsub("[^0-9]", "", d))
    melt(df, idvars=c("LOCATION", "ADDRESS", "CITY", "ZIP.CODE", "YEAR"),
         measure.vars=c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
                        "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"),
         variable.name = "MONTH", value.name = toupper(f))
    })
})

datafiles <- setNames(datafiles, c("Circulation", "Holds", "Computer", "Visitors"))

# ROW BIND ALL SETS
datafiles <- lapply(datafiles, function(f) do.call(rbind, f))

# MERGE ALL SETS INTO ONE
libdata <- Reduce(function(right, left) 
  merge(right, left, by=c("LOCATION", "YEAR", "MONTH"), all=TRUE), datafiles)

saveRDS(libdata, "ChicagoLibData.rds")


### WIFI TOTALS
files <- list.files(path="Wifi", pattern=".csv", full.names = TRUE)
wifidata <- lapply(files, function(d){
  df <- read.csv(d)
  names(df)[1] <- c("MONTH")
  df$MONTH <- as.factor(toupper(trimws(df$MONTH)))
  df$CUMULATIVE.NUMBER.OF.SESSIONS <- NULL;  df$YTD <- NULL
  return(df)
})

wifidata <- setNames(do.call(rbind, wifidata), c("MONTH", "YEAR", "WIFI"))
saveRDS(wifidata, "ChicagoWifiData.rds")



# LOCATIONS DATA
locdata <- read.csv("LibrariesLocations.csv", stringsAsFactors = FALSE)
names(locdata) <- c("LOCATION", names(locdata[2:(ncol(locdata)-1)]), "GEOCODES")

locdata$LAT <- sapply(locdata$GEOCODES, function(s)
                   gsub("\\(", "", strsplit(s, split=",")[[1]][1]),
                   USE.NAMES = FALSE)

locdata$LON <- sapply(locdata$GEOCODES, function(s)
                   gsub(")", "", strsplit(s, split=",")[[1]][2]),
                   USE.NAMES = FALSE)

saveRDS(locdata, "ChicagoLocationData.rds")



