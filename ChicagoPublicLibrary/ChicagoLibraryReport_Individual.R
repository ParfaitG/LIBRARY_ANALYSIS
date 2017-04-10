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

\\newgeometry{margin=1.5in}
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


##############################
## CHICAGO LIBRARY REPORT
##############################

mainData <- readRDS("DATA/ChicagoLibData.rds")

i = 1
for (lib in unique(mainData$LOCATION)) {
  print("##############################")
  print(paste(i, lib))
  print("##############################")
  
  libdir <- gsub("[/\\*^!@#$%^&]", "_", lib)
  
  LibData <- mainData[mainData$LOCATION == lib,]
  
  if (!(dir.exists(paste0("Libraries/", libdir))))
      dir.create(paste0("Libraries/", libdir))
  
  # SUMMARY STATS
  by_years <- by(LibData[c("YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], LibData$YEAR, FUN=summary)
  by_yearstbl <- lapply(by_years, function(t) print(xtable(t)))
  
  latextable <- paste(print(xtable(summary(LibData[c("CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")]))), 
                      "\\newline \\newline \\newline",
                      paste(by_yearstbl, collapse="\\newline \\newline \\newline"))

  # OUTPUT REPORT
  res <- latexbuild(paste0("Libraries/", libdir), "SummaryStats.tex", portraitbody, latextable,
                    paste(lib, "Library Stats"), " Circulation Activity", "2011 - 2016")
  
  # STATS BY YEAR
  aggdf <- aggregate(.~YEAR, LibData[c("YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], 
                     FUN=sum, na.action=na.pass)
  
  aggdf[,c(2:ncol(aggdf))] <- sapply(aggdf[,c(2:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
  
  latextable <- print(xtable(aggdf, align=c(rep("l", 2), rep("r", 4)), digits=rep(0,6)), 
                      tabular.environment = "longtable",
                      add.to.row = addtorow,
                      hline.after=c(-1))
  
  # RUN THREE TIMES FOR LONG TABLE
  res <- latexbuild(paste0("Libraries/", libdir), "ChicagoLibraryByYear.tex", portraitbody, latextable,
                    paste(lib, "Library Stats"), "Totals By Year", "2011 - 2016")

  # STATS BY MONTH
  aggdf <- aggregate(.~MONTH + YEAR, LibData[c("MONTH", "YEAR", "CIRCULATION", 
                                                  "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)
  aggdf[,c(3:ncol(aggdf))] <- sapply(aggdf[,c(3:ncol(aggdf))], function(col) formatC(col, format="d", big.mark=","))
  
  res <- print(xtable(aggdf, align=c(rep("l", 3), rep("r", 4)), digits=rep(0,7)), 
                      tabular.environment = "longtable",
                      add.to.row = addtorow,
                      hline.after=c(-1))
  
  # RUN THREE TIMES FOR LONG TABLE
  lapply(seq(3), function(i) 
    latexbuild(paste0("Libraries/", libdir), "ChicagoLibraryByMonth.tex", longtablebody, latextable,
               paste(lib, "Library Stats"), "Totals By Month", "2011 - 2016"))
  
  # CLEAN UP LATEX AUXILIARY FILES
  for (f in list.files(path=paste0("Libraries/", libdir), 
                       pattern="\\.log|\\.aux|\\.tex",
                       full.names = TRUE)){
    file.remove(f)
  }
  
  i = i + 1
}

