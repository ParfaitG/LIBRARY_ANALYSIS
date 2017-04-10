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
  png(paste0("Libraries/_System/", strfile), width=8.5, height=11, units="in", res=ppi)
  print(objPlot)
  
  dev.off()
}


##############################
## CHICAGO LIBRARY GRAPHS
##############################

mainData <- readRDS("DATA\\ChicagoLibData.rds")

# ITERATE THROUGH ALL LOCATIONS
i = 1
for (lib in unique(mainData$LOCATION)) {
  print(paste(i, lib))
  libdir <- gsub("[/\\*^!@#$%^&]", "_", lib)
  
  LibData <- mainData[mainData$LOCATION == lib,]
  
  # CREATE FOLDER IF DOES NOT EXIST
  if (!dir.exists(paste0("Libraries/", libdir)))
    dir.create(paste0("Libraries/", libdir))
  
  # TOTAL CIRCULATION GRAPHS BY YEAR
  aggdf <- aggregate(.~LOCATION + YEAR, LibData[c("LOCATION", "YEAR", "CIRCULATION", 
                                                  "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)

  circActivity <- grid.arrange(
    ggplot(aggdf[c("LOCATION", "YEAR", "CIRCULATION")], aes(x=LOCATION, y=CIRCULATION, fill=factor(YEAR))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Checkouts/Renewals"), x="Libraries", y="Circulation") +
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "HOLDS")], aes(x=LOCATION, y=HOLDS, fill=factor(YEAR))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Holds"), x="Libraries", y="Holds")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "VISITORS")], aes(x=LOCATION, y=VISITORS, fill=factor(YEAR))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Visitors"), x="Libraries", y="Visitors")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "COMPUTER")], aes(x=LOCATION, y=COMPUTER, fill=factor(YEAR))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Computer Sessions"), x="Libraries", y="Computer Sessions")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette)
  )

  ggsave(file=paste0("Libraries/", libdir, "/CirculationTotalByYear.png"), circActivity, height=11, width=8.5, units="in") 
  
  
  # TOTAL CIRCULATION GRAPHS BY MONTH
  aggdf <- aggregate(.~LOCATION + MONTH, LibData[c("LOCATION", "MONTH", "CIRCULATION", 
                                                   "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)
  
  aggdf$MONTH <- substr(as.character(aggdf$MONTH),1,3)
  aggdf$MONTH <- factor(aggdf$MONTH, levels = substr(toupper(month.name), 1, 3))
  
  circActivity <- grid.arrange(
    ggplot(aggdf[c("LOCATION", "MONTH", "CIRCULATION")], aes(x=LOCATION, y=CIRCULATION, fill=factor(MONTH))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH", nrow = 2)) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Checkouts/Renewals"), x="Libraries", y="Circulation") +
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "HOLDS")], aes(x=LOCATION, y=HOLDS, fill=factor(MONTH))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH", nrow = 2)) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Holds"), x="Libraries", y="Holds")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "VISITORS")], aes(x=LOCATION, y=VISITORS, fill=factor(MONTH))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH", nrow = 2)) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Visitors"), x="Libraries", y="Visitors")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
      scale_fill_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "COMPUTER")], aes(x=LOCATION, y=COMPUTER, fill=factor(MONTH))) +
      stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH", nrow = 2)) + 
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Computer Sessions"), x="Libraries", y="Computer Sessions")+
      theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), legend.title=element_text(size=7), 
            legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
            axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
      scale_fill_manual(values = seabornPalette)
  )
  
  ggsave(file=paste0("Libraries/", libdir, "/CirculationTotalByMonth.png"), circActivity, height=11, width=8.5, units="in") 

  
  # AVG CIRCULATION GRAPHS BY YEAR
  aggdf <- aggregate(.~LOCATION + YEAR, LibData[c("LOCATION", "YEAR", "CIRCULATION", 
                                                  "HOLDS", "COMPUTER", "VISITORS")], FUN=mean, na.action=na.pass)
  
  circActivity <- grid.arrange(
    ggplot(aggdf[c("LOCATION", "YEAR", "CIRCULATION")], aes(x=YEAR, y=CIRCULATION)) +
      geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Checkouts/Renewals"), x=lib, y="Circulation") +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "HOLDS")], aes(x=YEAR, y=HOLDS)) +
      geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Holds"), x=lib, y="Holds") +
      scale_colour_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "VISITORS")], aes(x=YEAR, y=VISITORS)) +
      geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Visitors"), x=lib, y="Visitors") +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette),
    
    ggplot(aggdf[c("LOCATION", "YEAR", "COMPUTER")], aes(x=YEAR, y=COMPUTER)) +
      geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
      scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Computer Sessions"), x=lib, y="CPU Sessions") +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette)
  )
  
  ggsave(file=paste0("Libraries/", libdir, "/CirculationAvgByYear.png"), circActivity, height=8.5, width=11, units="in") 
  
  
  # AVG CIRCULATION GRAPHS BY MONTH
  aggdf <- aggregate(.~LOCATION + MONTH + YEAR, LibData[c("LOCATION", "MONTH", "YEAR", "CIRCULATION", 
                                                          "HOLDS", "COMPUTER", "VISITORS")], FUN=mean, na.action=na.pass)
  
  aggdf$MONTH <- substr(as.character(aggdf$MONTH),1,3)
  aggdf$MONTH <- factor(aggdf$MONTH, levels = substr(toupper(month.name), 1, 3))
  
  
  circActivity <- grid.arrange(
    ggplot(aggdf[c("LOCATION", "MONTH", "YEAR", "CIRCULATION")], aes(x=MONTH, y=CIRCULATION, group=factor(YEAR), colour=factor(YEAR))) +
      geom_line() + geom_point() + scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Checkouts/Renewals"), x=lib, y="Circulation") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette, name = "Year"),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "YEAR", "HOLDS")], aes(x=MONTH, y=HOLDS, group=factor(YEAR), colour=factor(YEAR))) +
      geom_line() + geom_point() + scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Holds"), x=lib, y="Holds") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette, name = "Year"),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "YEAR", "VISITORS")], aes(x=MONTH, y=VISITORS, group=factor(YEAR), colour=factor(YEAR))) +
      geom_line() + geom_point() + scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Visitors"), x=lib, y="Visitors") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette, name = "Year"),
    
    ggplot(aggdf[c("LOCATION", "MONTH", "YEAR","COMPUTER")], aes(x=MONTH, y=COMPUTER, group=factor(YEAR), colour=factor(YEAR))) +
      geom_line() + geom_point() + scale_y_continuous(labels = comma) +
      labs(title=paste(lib, "Computer Sessions"), x=lib, y="CPU Sessions") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = seabornPalette, name = "Year")
  )
  
  ggsave(file=paste0("Libraries/", libdir, "/CirculationAvgByMonth.png"), circActivity, height=8.5, width=11, units="in") 
  
  i = i + 1
}

