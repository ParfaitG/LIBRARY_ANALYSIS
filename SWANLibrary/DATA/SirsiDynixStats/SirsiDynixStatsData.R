library(RSQLite)
library(readxl)
library(reshape2)

setwd("C:/Path/To/Working/Directory")

members <- read.csv("../SymphonyStats/SWANLibraryMembers.csv")

# FILE SYSTEM FILES BY YEAR AND MONTH -----------------
listFiles <- lapply(c(2015:2017), function(yr) {
  lapply(seq(12), function(mth) {
    mth <- ifelse(nchar(mth)==1, paste0('0',mth), paste0(mth))
    
    if (file.exists(paste0(yr,"/",yr, '-', mth))) {
      list.files(pattern=paste0("Monthly holds|Monthly renewals|Holds_Interface|",
                                "Renewals_Interface|checkouts by interface|Checkouts_Interface|",
                                "Profile|Patrons_Added|users added|_ILL|Category 7 ILL|",
                                "checkins|Checkins_"), 
                 path=paste0("DATA/", yr,"/",yr, '-', mth), full.names = TRUE)
    }
  })
})

listFiles <- lapply(listFiles, function(d) Filter(function(f) length(f) > 1, d))

cleanup_cols <- function(df){
  rawnames <- c("WorkFlow", "unknown", "unkown", "Self.Checks", "MobileCirc")
  cleannames <- c("Workflow", "Unknown", "Unknown", "Self.Check", "Mobile.Circ")
  
  for (i in seq(length(rawnames))) {
      names(df)[grep(rawnames[[i]], names(df))] <- gsub(rawnames[[i]], cleannames[[i]], 
                                                        names(df)[grep(rawnames[[i]], names(df))])
  }
  
  dblnames <- vapply(names(df), function(col) col %in% c("Checkouts_Checkouts", "Checkins_Checkins", 
                                                         "Renewals_Renewals", "Holds_Holds"), 
                     logical(1), USE.NAMES = FALSE) 
  
  names(df)[dblnames] <- gsub("_.*$", "", names(df)[dblnames])
  
  return(df)
}

add_missing_cols <- function(df){
  
  for (c in unique_cols_df$COLUMNS) {
    if (!(c %in% names(df))) {
      df[[c]] <- NA
    }
  }
  return(df)
}

# ONE-TO-ONE DATAFRAMES--------------------------------
create_month_dfs <- function(flist) {
  
  print(flist[[1]])
  
  df <- read_excel(flist[grep("Checkouts_Interface|checkouts by interface", flist)], skip = 4)
  names(df)[c(1, ncol(df))] <- c("LibID", "Checkouts")
  names(df)[2:ncol(df)] <- paste0("Checkouts_", trimws(names(df)[2:ncol(df)]))
  df <- cleanup_cols(df)
  chkoutdf <- df[complete.cases(df),]
  
  df <- read_excel(flist[grep("Checkins|checkins", flist)], skip = 4)
  df <- setNames(df[c(1, ncol(df))], c("LibID", "Checkins"))
  df$Checkins <- suppressWarnings(as.numeric(df$Checkins))
  chkindf <- df[complete.cases(df),]
  
  df <- read_excel(flist[grep("Renewals|renewals", flist)], skip = 4)
  names(df)[c(1, ncol(df))] <- c("LibID", "Renewals")
  names(df)[2:ncol(df)] <- paste0("Renewals_", trimws(names(df)[2:ncol(df)]))
  df <- cleanup_cols(df)
  rnwldf <- df[complete.cases(df),]
  
  df <- read_excel(flist[grep("Holds|holds", flist)], skip = 4)
  names(df)[c(1, ncol(df))] <- c("LibID", "Holds")
  names(df)[2:ncol(df)] <- paste0("Holds_", trimws(names(df)[2:ncol(df)]))
  df <- cleanup_cols(df)
  holddf <- df[complete.cases(df),]
  
  df <- read_excel(flist[grep("ILL", flist)], skip = 4)
  names(df)[c(1, ncol(df))] <- c("LibID", "Loans")
  names(df)[2:ncol(df)] <- paste0("ILL_", trimws(names(df)[2:ncol(df)]))
  df <- cleanup_cols(df)
  illdf <- df[complete.cases(df),]
  
  df <- read_excel(flist[grep("Patrons|users", flist)], skip = 3)
  names(df) <- c("LibID", "Patrons_Added")
  df$Patrons_Added <- suppressWarnings(as.numeric(df$Patrons_Added))
  df <- cleanup_cols(df)
  patdf <- df[complete.cases(df),]
  
  dfs <- list(chkoutdf, chkindf, rnwldf, holddf, illdf, patdf)
  
  df <- transform(Reduce(function(right, left) merge(right, left, by=c("LibID"), all=TRUE), dfs), 
                  Year = as.integer(substr(flist[[1]], 1, 4)),
                  Month = month.name[as.integer(substr(flist[[1]], 11, 12))])
  df <- df[c(1, ncol(df)-1, ncol(df), 2:(ncol(df)-2))]
}

# BUILD AND FLATTEN LIST OF DATAFRAMES
month_dfs <- lapply(listFiles, function(d) lapply(d, function(i) create_month_dfs(i)))
month_dfs <- Reduce(append, month_dfs)

# ADD MISSING COLS FOR ROW BIND
unique_cols_df <- aggregate(.~COLUMNS, data.frame(KEY = 1, COLUMNS = Reduce(append, lapply(month_dfs, colnames))), FUN=length)
month_dfs <- lapply(month_dfs, add_missing_cols)
rm(unique_cols_df)

# ROW BIND ALL DATA FRAMES INTO ONE MASTER
LibData <- do.call(rbind, month_dfs)
rm(month_dfs)

# MERGE WITH MEMBER DATA
LibData <- merge(members[c("LibID", "Library")], LibData, by="LibID", all.x=TRUE)

# OUTPUT TO FILE
write.csv(LibData, "SirsiDynixStatsData.csv", na="")
saveRDS(LibData, "SirsiDynixStatsData.rds")


# MANY TO MANY DATAFRAMES --------------------------------

# CONNECT TO DATABASE
dbconn <- dbConnect(SQLite(), dbname = "SirsiDynixStatsSectionData.db")

append_dfs_db <- function(flist, type) {
  
  print(flist[[6]])
  
  # NORMALIZE CHECKINS
  df <- read_excel(flist[grep("Checkins|checkins", flist)], skip = 5)
  names(df)[1] <- "LibID"
  df <- df[Filter(function(n) n!="", names(df))]
  df <- df[complete.cases(df),]
  chkindf <- melt(df, id.vars="LibID", value.name="Checkins", variable.name="Source")
  
  # NORMALIZE CHECKOUTS
  df <- read_excel(flist[grep("Profile", flist)], skip = 5)
  names(df)[1] <- "LibID"
  df <- df[Filter(function(n) n!="", names(df))]
  df <- df[complete.cases(df) & df$LibID!='TOTAL',]
  df$TOTAL <- NULL
  
  # SPLIT SECTION INDICATORS
  mdf <- melt(df, id.vars="LibID", value.name="Checkouts", variable.name="Source_Section")
  mdf$Source_Section <- as.character(mdf$Source_Section)
  
  mdf$Source <- vapply(mdf$Source_Section, function(s)
    ifelse(grepl("_", s), strsplit(s, "_")[[1]][1], s), character(1))
  
  mdf$Section <- vapply(mdf$Source_Section, function(s)
    ifelse(grepl("_[^0-9]", s) & grepl("_[A-Z]{2,}$", s), 
           strsplit(s, "_")[[1]][2], s), character(1))
  
  chkoutdf <- aggregate(Checkouts ~ LibID + Section, mdf, FUN=sum, stringsAsFactors = FALSE)
  
  # IMPORT INTO DATABASE
  chkindf <- transform(chkindf, 
                       Year = as.integer(substr(flist[[1]], 6, 9)),
                       Month = month.name[as.integer(substr(flist[[1]], 16, 17))])
  
  chkoutdf <- transform(chkoutdf, 
                        Year = as.integer(substr(flist[[1]], 6, 9)),
                        Month = month.name[as.integer(substr(flist[[1]], 16, 17))])
  
  dbWriteTable(dbconn, "CheckinsData", chkindf, append=TRUE, row.names=FALSE)
  dbWriteTable(dbconn, "CheckoutsData", chkoutdf, append=TRUE, row.names=FALSE)
}

section_dfs <- lapply(listFiles, function(d) lapply(d, function(i) append_dfs_db(i)))

# CLOSE DATABASE
dbDisconnect(dbconn)
