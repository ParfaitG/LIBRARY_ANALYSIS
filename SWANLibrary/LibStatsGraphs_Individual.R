library(RSQLite)
library(ggplot2)
library(scales)
library(gridExtra)

setwd("C:/Path/To/Working/Directory")

seabornPalette <- c("#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2",
                    "#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868",
                    "#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd",
                    "#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2",
                    "#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868",
                    "#c44e52","#8172b2","#ccb974","#64b5cd","#4c72b0","#55a868","#c44e52","#8172b2","#ccb974","#64b5cd")

##############################
## PLOT GENERATION 
##############################

outputPlot <- function(objPlot, strfile){
  ppi <- 300
  png(paste0("Libraries/", strfile), width=8.5, height=11, units="in", res=ppi)
  print(objPlot)
  
  dev.off()
}


#########################
## SYMPHONY STATS
#########################
mainData <- readRDS("DATA/SymphonyStats/SymphonyStatsData.rds")
mainData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", mainData$Library)))
mainData$BibsAdded <- as.numeric(mainData$BibsAdded)

for (lib in unique(mainData$Library)) {
  
    print(paste(lib, "- Symphony"))
  
    # SUBSET WHOLE DATASET
    LibData <- subset(mainData, Library==lib)
    
    # CREATE FOLDER
    if (dir.exists(paste0("Libraries/", lib)) == FALSE) 
      dir.create(paste0("Libraries/", lib))
    
    #LibData[is.na(LibData)] <- 0
    # CIRCULATION GRAPHS BY YEAR
    aggdf <- aggregate(.~Library+Year, LibData[c("Library", "Year", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                                 "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum, na.action=na.pass)
 
    circActivity <- grid.arrange(
      ggplot(aggdf[c("Library", "Year", "CHKOUTS")], aes(x=Library, y=CHKOUTS, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkouts", x="Library", y="Checkouts") +
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "CHKINS")], aes(x=Library, y=CHKINS, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkins", x="Library", y="Checkins")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "RENEWALS")], aes(x=Library, y=RENEWALS, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Renewals", x="Library", y="Renewals")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "HOLDS")], aes(x=Library, y=HOLDS, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Holds", x="Library", y="Holds")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "ILL")], aes(x=Library, y=ILL, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Loans", x="Library", y="Loans")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
        scale_fill_manual(values = seabornPalette)
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SymphonyCircActivityYear.png"), circActivity, height=11, width=8.5, units="in") 
    
    # MATERIALS BY YEAR
    materialAdd <- grid.arrange(
      ggplot(aggdf[c("Library", "Year", "BibsAdded")], aes(x=Library, y=BibsAdded, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Bibs Added", x="Library", y="Bibs")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "ItemsAdded")], aes(x=Library, y=ItemsAdded, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Items Added", x="Library", y="Items")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "PatronsAdded")], aes(x=Library, y=PatronsAdded, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Patrons Added", x="Library", y="Patrons")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      ncol = 2
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SymphonyMaterialsAddedYear.png"), materialAdd, height=8.5, width=11, units="in") 
    
    
    # CIRCULATION GRAPHS BY MONTH
    aggdf <- aggregate(.~Library+Month, LibData[c("Library", "Month", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                                  "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum, na.action=na.pass)
    aggdf$Month <- factor(aggdf$Month, levels = month.name)
    
    circActivity <- grid.arrange(
      ggplot(aggdf[c("Library", "Month", "CHKOUTS")], aes(x=Library, y=CHKOUTS, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkouts", x="Library", y="Checkouts") + 
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "CHKINS")], aes(x=Library, y=CHKINS, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkins", x="Library", y="Checkins")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "RENEWALS")], aes(x=Library, y=RENEWALS, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Renewals", x="Library", y="Renewals")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "HOLDS")], aes(x=Library, y=HOLDS, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Holds", x="Library", y="Holds")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "ILL")], aes(x=Library, y=ILL, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Loans", x="Library", y="Loans")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
        scale_fill_manual(values = seabornPalette)
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SymphonyCircActivityMonth.png"), circActivity, height=11, width=8.5, units="in") 
    
    # MATERIALS BY MONTH
    materialAdd <- grid.arrange(
      ggplot(aggdf[c("Library", "Month", "BibsAdded")], aes(x=Library, y=BibsAdded, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Bibs Added", x="Library", y="Bibs")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "ItemsAdded")], aes(x=Library, y=ItemsAdded, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Items Added", x="Library", y="Items")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "PatronsAdded")], aes(x=Library, y=PatronsAdded, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Patrons Added", x="Library", y="Patrons")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
        scale_fill_manual(values = seabornPalette),
      ncol = 2
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SymphonyMaterialsAddedMonth.png"), materialAdd, height=8.5, width=11, units="in") 

}


#########################
## SIRSIDYNIX STATS
#########################
mainData <- readRDS("DATA/SirsiDynixStats/SirsiDynixStatsData.rds")
mainData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", mainData$Library)))


for (lib in unique(mainData$Library)) {
    print(paste(lib, "- SirsiDynix"))
    # SUBSET FROM WHOLE DATASET
    LibData <- subset(mainData, Library == lib)
    
    # CREATE FOLDER
    if (dir.exists(paste0("Libraries/", lib)) == FALSE) 
      dir.create(paste0("Libraries/", lib))
    
    # CIRCULATION BY YEAR
    aggdf <- aggregate(.~Library + Year, LibData[c("Library", "Year", "Checkouts", "Checkins", "Renewals", 
                                                    "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum, na.action=na.pass)
    
    circActivity <- grid.arrange(
      ggplot(aggdf[c("Library", "Year", "Checkouts")], aes(x=Library, y=Checkouts, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkouts", x="Libraries", y="Checkouts") +
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "Checkins")], aes(x=Library, y=Checkins, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkins", x="Libraries", y="Checkins")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "Renewals")], aes(x=Library, y=Renewals, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Renewals", x="Library", y="Renewals")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "Holds")], aes(x=Library, y=Holds, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Holds", x="Library", y="Holds")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "ILL_Loans")], aes(x=Library, y=ILL_Loans, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Loans", x="Library", y="Loans")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.5, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Year", "Patrons_Added")], aes(x=Library, y=Patrons_Added, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Patrons Added", x="Library", y="Patrons Added")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette)
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SirsiDynixCircActivityYear.png"), circActivity, height=11, width=8.5, units="in") 
    
    
    # CIRCULATION BY MONTH
    aggdf <- aggregate(.~Library + Month, LibData[c("Library", "Month", "Checkouts", "Checkins", "Renewals", 
                                                    "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum, na.action=na.pass)
    aggdf$Month <- factor(aggdf$Month, levels = month.name)
    
    # CIRCULATION BY YEAR
    circActivity <- grid.arrange(
      ggplot(aggdf[c("Library", "Month", "Checkouts")], aes(x=Library, y=Checkouts, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkouts", x="Library", y="Checkouts") +
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "Checkins")], aes(x=Library, y=Checkins, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Checkins", x="Library", y="Checkins")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "Renewals")], aes(x=Library, y=Renewals, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Renewals", x="Library", y="Renewals")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "Holds")], aes(x=Library, y=Holds, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Holds", x="Library", y="Holds")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "ILL_Loans")], aes(x=Library, y=ILL_Loans, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Loans", x="Library", y="Loans")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.5, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
        scale_fill_manual(values = seabornPalette),
      
      ggplot(aggdf[c("Library", "Month", "Patrons_Added")], aes(x=Library, y=Patrons_Added, fill=factor(Month))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Monthr", nrow=3)) + 
        scale_y_continuous(labels = comma) +
        labs(title="Patrons Added", x="Library", y="Patrons Added")+
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
              legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette)
    )
    
    ggsave(file=paste0("Libraries/", lib, "/SirsiDynixCircActivityMonth.png"), circActivity, height=11, width=8.5, units="in") 
    
    
    # CHECKOUTS SECTION DATA
    libID <- unlist(LibData$LibID[[1]])
    
    dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
    LibData <- dbGetQuery(dbconn, paste0("SELECT * FROM CheckoutsData WHERE LibID='", libID, "'"))
    dbDisconnect(dbconn)
    
    if (nrow(LibData) > 0) {
      # TOP TEN SECTIONS
      aggdf <- aggregate(.~Section + Year, LibData[c("Year", "Section", "Checkouts")], FUN=sum, na.action=na.pass)
      
      aggdf <- aggdf[order(aggdf$Year, -aggdf$Checkouts),]
      aggdf <- Reduce(rbind, by(aggdf, aggdf$Year, head, n=10))
      
      # CHECKOUTS BY SECTION
      graph <- ggplot(aggdf, aes(x="", y=Checkouts, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Total Checkouts by Sections", x="Section", y="Checkouts") +
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=10), 
              legend.text=element_text(size=10), legend.key.size = unit(1, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette) + facet_wrap(~Section, nrow=5, scales="free")
      
      outputPlot(graph, paste0(lib, "/SirsiDynixTopTenCategoryCheckouts.png"))
    }
    
    # CHECKINS SECTION DATA
    dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
    LibData <- dbGetQuery(dbconn, paste0("SELECT * FROM CheckinsData WHERE LibID='", libID, "'"))
    dbDisconnect(dbconn)
    
    if (nrow(LibData) > 0) {
      # TOP TEN SECTIONS
      aggdf <- aggregate(.~Source + Year, LibData[LibData$LibID != LibData$Source, c("Year", "Source", "CheckIns")], FUN=sum, na.action=na.pass)
      aggdf <- aggdf[!aggdf$Source %in% c('Total', 'TOTAL'),]
      
      members <- read.csv("DATA/SymphonyStats/SWANLibraryMembers.csv")
      
      aggdf <- aggdf[order(aggdf$Year, -aggdf$CheckIns),]
      aggdf <- Reduce(rbind, by(aggdf, aggdf$Year, head, n=10))
      aggdf <- merge(members[c("LibID", "Library")], aggdf, by.x="LibID", by.y="Source")
      
      # CHECKINS BY SECTION
      graph <- ggplot(aggdf, aes(x="", y=CheckIns, fill=factor(Year))) +
        stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.75) + guides(fill=guide_legend(title="Year")) + 
        scale_y_continuous(labels = comma) +
        labs(title="Total Checkins by External Library Sources", x="Section", y="Checkins") +
        theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=10), 
              legend.text=element_text(size=10), legend.key.size = unit(1, "line"),
              axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
        scale_fill_manual(values = seabornPalette) + facet_wrap(~Library, ncol=2, scales="free")
      
      outputPlot(graph, paste0(lib, "/SirsiDynixTopTenCategoryCheckins.png"))
    }
    

}




