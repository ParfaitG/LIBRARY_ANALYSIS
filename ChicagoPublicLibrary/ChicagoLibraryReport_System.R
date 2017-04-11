library(xtable)
library(knitr)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

setwd("C:/Path/To/Working/Directory")


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

\\newgeometry{margin=1in}
\\maketitle

\\centering
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
  shell(cmd=paste("cd",cd, "&& C:\\Path\\To\\XeLatex\\MiKTeX\\miktex\\bin\\pdflatex", strfile), 
        mustWork=TRUE, intern=TRUE, translate=TRUE)
  
}


##############################
## CHICAGO LIBRARY REPORT
##############################

LibData <- readRDS("DATA\\ChicagoLibData.rds")
WifiData <- readRDS('DATA\\ChicagoWifiData.rds')

# SUMMARY STATS
WIFIs <- by(WifiData[c("WIFI")], WifiData$YEAR, FUN=summary)
WIFIs <- lapply(WIFIs, function(w) append(w, "NA's   :0"))
ALLs <- Map(cbind, by(LibData[c("YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], LibData$YEAR, FUN=summary), WIFI=WIFIs)

latextable <- lapply(ALLs, function(t) print(xtable(t)))

latextable <- paste(print(xtable(cbind(summary(LibData[c("CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")]), WIFI))), 
                    "\\newline \\newline \\newline", "\\newline \\newline \\newline",
                    paste(latextable, collapse="\\newline \\newline \\newline"))

# OUTPUT REPORT
res <- latexbuild("Libraries/_System", "SummaryStats.tex", portraitbody, latextable,
                  "Chicago Public Library", " Circulation Activity", "2011 - 2016")

# STATS BY YEAR
aggdf <- merge(aggregate(.~YEAR, LibData[c("YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], FUN=sum),
               aggregate(.~YEAR, WifiData[c("YEAR", "WIFI")], FUN=sum), by=("YEAR"))

aggdf[,c(2:ncol(aggdf))] <- sapply(aggdf[,c(2:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

latextable <- print(xtable(aggdf, align=c(rep("l", 2), rep("r", 5)), digits=rep(0,7)), 
                    tabular.environment = "longtable",
                    add.to.row = addtorow,
                    hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
res <- latexbuild("Libraries/_System", "ChicagoLibraryByYear.tex", portraitbody, latextable,
                  "Chicago Public Library", "Totals By Year", "2011 - 2016")


# STATS BY LOCATION AND YEAR
aggdf <- aggregate(.~LOCATION + YEAR, LibData[c("LOCATION", "YEAR", "CIRCULATION", 
                                                "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)
aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))

res <- latextable <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 4)), digits=rep(0,7)), 
                           tabular.environment = "longtable",
                           add.to.row = addtorow,
                           hline.after=c(-1))

# RUN THREE TIMES FOR LONG TABLE
lapply(seq(3), function(i) 
  latexbuild("Libraries/_System", "ChicagoLibraryByLocationYear.tex", longtablebody, latextable,
             "Chicago Public Library", "Totals By Library and Year", "2011 - 2016"))

# CLEAN UP LATEX AUXILIARY FILES
for (f in list.files(path="Libraries/_System", 
                     pattern="\\.log|\\.aux|\\.tex",
                     full.names = TRUE)){
  file.remove(f)
}