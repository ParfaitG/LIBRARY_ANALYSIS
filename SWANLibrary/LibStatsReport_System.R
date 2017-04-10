library(RSQLite)
library(xtable)
library(knitr)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

setwd("C:/Path/To/Working/Directory")

members <- read.csv("DATA/SymphonyStats/SWANLibraryMembers.csv")


##############################
## LATEX MARKUP SETTINGS
##############################

regularbody <- "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{geometry}
\\usepackage{pdflscape}

\\title{%s \\\\ %s \\\\ %s}
\\author{}

\\newgeometry{margin=1in}
\\begin{document}
\\pagenumbering{gobble}

\\begin{landscape}
\\maketitle

%s

\\end{landscape}
\\restoregeometry
\\end{document}"

portraitbody <- "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{geometry}
\\usepackage{longtable}

\\title{%s \\\\ %s \\\\ %s}
\\author{}
\\begin{document}
\\pagenumbering{gobble}

\\newgeometry{margin=1cm}
\\maketitle

%s

\\restoregeometry
\\end{document}"


longtablebody <- "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{geometry}
\\usepackage{longtable}
\\usepackage{pdflscape}

\\title{%s \\\\ %s \\\\ %s}
\\author{}
\\begin{document}
\\pagenumbering{gobble}

\\newgeometry{margin=1cm}
\\begin{landscape}
\\maketitle

%s

\\end{landscape}
\\restoregeometry
\\end{document}"


addtorow <- list(pos=list(0), command=c(paste0("\\hline \n",
                                               "\\endhead \n",
                                               "\\hline \n",
                                               "\\endfoot \n",
                                               "\\endlastfoot \n")))

latexbuild <- function(path, strfile, type, table, title, subtitle, thirdtitle){
  
  # OUTPUT TO TEXT
  fileConn<-file(paste0(path, "/", strfile), encoding = "UTF-8")
  writeLines(sprintf(type, title, subtitle, thirdtitle, table), fileConn)
  close(fileConn)
  
  # OUTPUT TO LATEXT PDF
  cd <- paste0(getwd(), "/", path)
  shell(cmd=paste("cd",cd, "&& D:\\XeLatex\\MiKTeX\\miktex\\bin\\pdflatex", strfile), 
        mustWork=TRUE, intern=TRUE, translate=TRUE)
  
}


############################
## SYMPHONY STATS
############################
LibData <- readRDS("DATA/SymphonyStats/SymphonyStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))

# SUMMARY OUTPUT ACROSS MAIN METRICS
LibData$BibsAdded <- as.numeric(LibData$BibsAdded)
latextable <- print(xtable(summary(LibData[c("CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "BibsAdded", "ItemsAdded", "PatronsAdded")])))

# OUTPUT REPORT
res <- latexbuild("Libraries/_System", "SymphonySummaryStats.tex", regularbody, latextable,
                  "SWAN Library Circulation Activity", "Symphony data", "2011 - 2015")


# CIRCULATION ACTIVITY AGGREGATION BY LIBRARY AND YEAR
aggdf <- aggregate(.~Library+Year, LibData[c("Library", "Year", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                             "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum)
aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

aggdf$Library <- substr(aggdf$Library, 1, 30)

latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 8)), digits=rep(0,11)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- lapply(seq(3), function(i) 
  latexbuild("Libraries/_System", "SymphonyStatsByLibraryYear.tex", longtablebody, latextable,
             "SWAN Library Circulation Activity By Library and Year", "Symphony data", "2011 - 2015"))



# CIRCULATION SECTION AGGREGATION BY CATEGORY AND YEAR
LibData <- readRDS("DATA/SymphonyStats/SymphonyStatsSectionCircData.rds")

aggdf <- aggregate(.~CATEGORY+YEAR, LibData[LibData$CATEGORY != 'TOTAL', 
                                            c("CATEGORY", "YEAR", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS")], FUN=sum)
aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 4)), digits=rep(0,7)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- lapply(seq(3), function(i) 
  latexbuild("Libraries/_System", "SymphonyStatsByCategoryYear.tex", longtablebody, latextable,
             "SWAN Library Circulation Activity By Category and Year", "Symphony data", "2011 - 2015"))


# CIRCULATION SECTION AGGREGATION BY LIBRARY, CATEGORY, AND YEAR
aggdf <- aggregate(.~Library+CATEGORY+YEAR, LibData[LibData$CATEGORY != 'TOTAL', 
                                            c("Library", "CATEGORY", "YEAR", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS")], FUN=sum)
aggdf[,c(4:ncol(aggdf))] <- sapply(aggdf[,c(4:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

aggdf$Library <- substr(aggdf$Library, 1, 30)

aggdf <- aggdf[-c(12, 40),]

res <- latextable <- print(xtable(aggdf, align=c(rep("l", 4), rep("r", 4)), digits=rep(0,8)), 
                           tabular.environment = "longtable",
                           add.to.row = addtorow,
                           hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
lapply(seq(3), function(i) 
  latexbuild("Libraries/_System", "SymphonyStatsByLibCategoryYear.tex", longtablebody, latextable,
             "SWAN Library Circulation Activity", "By Library, Category, and Year", "Symphony data, 2011 - 2015"))


############################
## SIRSIDYNIX STATA
############################

LibData <- readRDS("DATA/SirsiDynixStats/SirsiDynixStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))

# SUMMARY STATS REPORT
latextable <- print(xtable(summary(LibData[c("Checkouts", "Checkins", "Renewals", "Holds", "ILL_Loans", "Patrons_Added")])))

res <- latexbuild("Libraries/_System", "SirsiDynixSummaryStats.tex", regularbody, latextable,
                  "SWAN Library Circulation Activity", "SirsiDynix data", "2015 - 2017")


# CIRCULATION ACTIVITY BY LIBRARY AND YEAR REPORT
aggdf <- aggregate(.~Library + Year, LibData[c("Library", "Year", "Checkouts", "Checkins", "Renewals", 
                                               "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum)
aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 6)), digits=rep(0,9)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- lapply(seq(3), function(i)
  latexbuild("Libraries/_System", "SirsiDynixSectionCircData.tex", longtablebody, latextable,
             "SWAN Library Circulation Activity", "SirsiDynix data", "2015 - 2017"))


# CHECKOUTS SECTION DATA
dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
LibData <- dbGetQuery(dbconn, "SELECT * FROM CheckoutsData")
dbDisconnect(dbconn)

aggdf <- aggregate(.~Section + Year, LibData[c("Year", "Section", "Checkouts")], FUN=sum)
aggdf[,ncol(aggdf)] <- formatC(aggdf[,ncol(aggdf)], format="d", big.mark=",")

latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 1)), digits=rep(0,4)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- lapply(seq(3), function(i)
  latexbuild("Libraries/_System", "SirsiDynixSectionCheckoutsData.tex", portraitbody, latextable,
             "SWAN Library Checkouts by Section", "SirsiDynix data", "2015 - 2017"))


# CHECKINS SECTION DATA
dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
LibData <- dbGetQuery(dbconn, "SELECT * FROM CheckinsData")
dbDisconnect(dbconn)

aggdf <- aggregate(.~LibID + Source + Year, LibData[LibData$LibID != LibData$Source, c("Year", "LibID", "Source", "CheckIns")], FUN=sum)
aggdf <- aggdf[!aggdf$LibID %in% c('Total', 'TOTAL') & !aggdf$Source %in% c('Total', 'TOTAL')  ,]
aggdf <- aggdf[aggdf$CheckIns > 0,]

# TOP 100 CHECKINS IN EACH YEAR
aggdf <- aggdf[order(aggdf$Year, -aggdf$CheckIns),]
aggdf <- Reduce(rbind, by(aggdf, aggdf$Year, head, n=100))
                
names(aggdf)[1:2] <- c("BorrowedLibID", "SourceLibID")

# RENAMING, REORDERING, CLEANING UP VALUES
aggdf <- transform(merge(members[c("LibID", "Library")], aggdf, by.x="LibID", by.y="SourceLibID"), LibID = NULL)
names(aggdf)[1] <- "SourceLibrary"
aggdf <- transform(merge(members[c("LibID", "Library")], aggdf, by.x="LibID", by.y="BorrowedLibID"), LibID = NULL)
names(aggdf)[1] <- "BorrowedLibrary"


aggdf[, 1:2] <- sapply(aggdf[, 1:2], function(col) trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", col))))
aggdf <- aggdf[order(aggdf$BorrowedLibrary, aggdf$Year, aggdf$SourceLibrary),]
rownames(aggdf) <-seq(nrow(aggdf))
aggdf[,ncol(aggdf)] <- formatC(aggdf[,ncol(aggdf)], format="d", big.mark=",")

latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 2)), digits=rep(0,5)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- lapply(seq(3), function(i)
  latexbuild("Libraries/_System", "SirsiDynixSectionCheckinsData.tex", portraitbody, latextable,
             "SWAN Library ILL Checkins by Source", "(Top 100 in Each Year)", "SirsiDynix data (2015 - 2017)"))

# CLEAN UP LATEX AUXILIARY FILES
for (f in list.files(path="Libraries/_System", 
                     pattern="\\.log|\\.aux|\\.tex",
                     full.names = TRUE)){
  file.remove(f)
}


