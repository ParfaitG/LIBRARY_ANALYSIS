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
mainData <- readRDS("DATA/SymphonyStats/SymphonyStatsData.rds")
mainData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", mainData$Library)))

for (lib in unique(mainData$Library)[43]) {
  
    print(paste(lib, "- Symphony"))
  
    # SUBSET WHOLE DATASET
    LibData <- subset(mainData, Library == lib)
    
    # CREATE FOLDER IF DOES NOT EXIST
    if (dir.exists(paste0("Libraries/", lib)) == FALSE) 
      dir.create(paste0("Libraries/", lib))
  
    # SUMMARY OUTPUT ACROSS MAIN METRICS
    LibData$BibsAdded <- as.numeric(LibData$BibsAdded)
    latextable <- print(xtable(summary(LibData[c("CHKOUTS", "CHKINS", "RENEWALS", "HOLDS",
                                                 "BibsAdded", "ItemsAdded", "PatronsAdded")])))
    
    # OUTPUT REPORT
    res <- latexbuild(paste0("Libraries/", lib), "SymphonySummaryStats.tex", regularbody, latextable,
                      paste(lib, "Circulation Activity"), "Symphony data", "2011 - 2015")
    
    
    # CIRCULATION ACTIVITY AGGREGATION BY LIBRARY AND YEAR
    aggdf <- aggregate(.~Library+Year, LibData[c("Library", "Year", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                                 "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum, na.action=na.pass)
    aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
    
    aggdf$Library <- substr(aggdf$Library, 1, 30)
    
    latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 8)), digits=c(rep(0,11))), 
                        tabular.environment = "longtable",
                        add.to.row = addtorow,
                        hline.after=c(-1))
    
    # RUN THREE TIMES FOR LONG TABLE
    res <- lapply(seq(3), function(i) 
      latexbuild(paste0("Libraries/", lib), "SymphonyStatsByLibraryYear.tex", longtablebody, latextable,
                 paste(lib, "Circulation Activity By Year"), "Symphony data", "2011 - 2015"))
    
    # CIRCULATION SECTION AGGREGATION BY CATEGORY AND YEAR
    sectData <- readRDS("DATA/SymphonyStats/SymphonyStatsSectionCircData.rds")
    sectData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", sectData$Library)))
    
    # SUBSET WHOLE DATASET
    LibData <- subset(sectData, Library == lib)

    if (nrow(LibData) > 0) {
        aggdf <- aggregate(.~CATEGORY+YEAR, LibData[LibData$CATEGORY != 'TOTAL', 
                                                    c("CATEGORY", "YEAR", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS")], 
                           FUN=sum, na.action=na.pass)
        aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
        
        latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 4)), digits=rep(0,7)), 
                            tabular.environment = "longtable",
                            add.to.row = addtorow,
                            hline.after=c(-1))
    
        # RUN THREE TIMES FOR LONG TABLE
        res <- lapply(seq(3), function(i) 
          latexbuild(paste0("Libraries/", lib), "SymphonyStatsByCategoryYear.tex", longtablebody, latextable,
                     paste(lib, "Library Circulation Activity By Category and Year"), "Symphony data", "2011 - 2015"))
        
    
        # CIRCULATION SECTION AGGREGATION BY LIBRARY, CATEGORY, AND YEAR
        aggdf <- aggregate(.~Library+CATEGORY+YEAR, LibData[LibData$CATEGORY != 'TOTAL', 
                                                            c("Library", "CATEGORY", "YEAR", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS")], 
                           FUN=sum, na.action=na.pass)
        aggdf[,c(4:ncol(aggdf))] <- sapply(aggdf[,c(4:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
        
        aggdf$Library <- substr(aggdf$Library, 1, 30)
        aggdf <- aggdf[-c(12, 40),]
        
        latextable <- print(xtable(aggdf, align=c(rep("l", 4), rep("r", 4)), digits=rep(0,8)), 
                            tabular.environment = "longtable",
                            add.to.row = addtorow,
                            hline.after=c(-1))
        
        # RUN THREE TIMES FOR LONG TABLE
        res <- lapply(seq(3), function(i) 
          latexbuild(paste0("Libraries/", lib), "SymphonyStatsByLibCategoryYear.tex", longtablebody, latextable,
                     paste(lib, " Circulation Activity"), "By Category and Year", "Symphony data, 2011 - 2015"))
    }
}


############################
## SIRSIDYNIX STATS
############################
mainData <- readRDS("DATA/SirsiDynixStats/SirsiDynixStatsData.rds")
mainData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", mainData$Library)))
    
for (lib in unique(mainData$Library)[43]) {
  
    print(paste(lib, "- SirsiDynix"))
  
    # SUBSET FROM WHOLE DATASET
    LibData <- subset(mainData, Library == lib)
  
    # CREATE FOLDER IF DOES NOT EXIST
    if (dir.exists(paste0("Libraries/", lib)) == FALSE) 
      dir.create(paste0("Libraries/", lib))
    
    # SUMMARY STATS REPORT
    latextable <- print(xtable(summary(LibData[c("Checkouts", "Checkins", "Renewals", 
                                                 "Holds", "ILL_Loans", "Patrons_Added")])))
    
    res <- latexbuild(paste0("Libraries/", lib), "SirsiDynixSummaryStats.tex", regularbody, latextable,
                      paste(lib, "Circulation Activity"), "SirsiDynix data", "2015 - 2017")
    
    # CIRCULATION ACTIVITY BY LIBRARY AND YEAR REPORT
    aggdf <- aggregate(.~Library + Year, LibData[c("Library", "Year", "Checkouts", "Checkins", "Renewals", 
                                                   "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum, na.action=na.pass)
    
    aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
    
    latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 6)), digits=rep(0, 9)), 
                        tabular.environment = "longtable",
                        add.to.row = addtorow,
                        hline.after=c(-1))
    
    # RUN THREE TIMES FOR LONG TABLE
    res <- lapply(seq(3), function(i)
      latexbuild(paste0("Libraries/", lib), "SirsiDynixSectionCircData.tex", longtablebody, latextable,
                 paste(lib, "Circulation Activity"), "SirsiDynix data", "2015 - 2017"))
    
    
    # CHECKOUTS SECTION DATA
    libID <- unlist(LibData$LibID[[1]])
    
    dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
    LibData <- dbGetQuery(dbconn, paste0("SELECT * FROM CheckoutsData WHERE LibID='", libID, "'"))
    dbDisconnect(dbconn)
    
    if (nrow(LibData) > 0) {
      aggdf <- aggregate(.~Section + Year, LibData[c("Year", "Section", "Checkouts")], FUN=sum, na.action=na.pass)
      aggdf[,ncol(aggdf)] <- formatC(aggdf[,ncol(aggdf)], format="d", big.mark=",")
      
      latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 1)), digits=rep(0, 4)), 
                          tabular.environment = "longtable",
                          add.to.row = addtorow,
                          hline.after=c(-1))
      
      # RUN THREE TIMES FOR LONG TABLE
      res <- lapply(seq(3), function(i)
        latexbuild(paste0("Libraries/", lib), "SirsiDynixSectionCheckoutsData.tex", portraitbody, latextable,
                   paste(lib, "Checkouts by Section"), "SirsiDynix data", "2015 - 2017"))
    }
    
    # CHECKINS SECTION DATA
    dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
    LibData <- dbGetQuery(dbconn, paste0("SELECT * FROM CheckinsData WHERE LibID='", libID, "'"))
    dbDisconnect(dbconn)
    
    if (nrow(LibData) > 0) {
        aggdf <- aggregate(.~LibID + Source + Year, LibData[LibData$LibID != LibData$Source, c("Year", "LibID", "Source", "CheckIns")], 
                           FUN=sum, na.action=na.pass)
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
          latexbuild(paste0("Libraries/", lib), "SirsiDynixSectionCheckinsData.tex", portraitbody, latextable,
                     paste(lib, "ILL Checkins by Source"), "(Top 100 in Each Year)", "SirsiDynix data (2015 - 2017)"))
    }
    
    # CLEAN UP LATEX AUXILIARY FILES
    for (f in list.files(path=paste0("Libraries/", lib), 
                         pattern="\\.log|\\.aux|\\.tex",
                         full.names = TRUE)){
        file.remove(f)
    }
}

