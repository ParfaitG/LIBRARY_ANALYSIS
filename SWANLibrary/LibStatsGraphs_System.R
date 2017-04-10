library(RSQLite)
library(ggplot2)
library(maps)
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

#########################
## SYMPHONY
#########################
LibData <- readRDS("DATA/SymphonyStats/SymphonyStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))
LibData$BibsAdded <- as.numeric(LibData$BibsAdded)

# CIRCULATION GRAPHS BY YEAR
aggdf <- aggregate(.~Library+Year, LibData[c("Library", "Year", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                             "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum)
circActivity <- grid.arrange(
  ggplot(aggdf[c("Library", "Year", "CHKOUTS")], aes(x="", y=CHKOUTS, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkouts by SWAN Libraries", x="Libraries", y="Checkouts") +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "CHKINS")], aes(x="", y=CHKINS, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkins by SWAN Libraries", x="Libraries", y="Checkins")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "RENEWALS")], aes(x="", y=RENEWALS, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Renewals by SWAN Libraries", x="Libraries", y="Renewals")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "HOLDS")], aes(x="", y=HOLDS, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Holds by SWAN Libraries", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "ILL")], aes(x="", y=ILL, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Inter-Library Loans by SWAN Libraries", x="Libraries", y="Loans")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/SymphonyCircActivityYear.png", circActivity, height=11, width=8.5, units="in") 
       

# MATERIALS BY YEAR
materialAdd <- grid.arrange(
  ggplot(aggdf[c("Library", "Year", "BibsAdded")], aes(x="", y=BibsAdded, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Bibs Added by SWAN Libraries", x="Libraries", y="Bibs")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ggplot(aggdf[c("Library", "Year", "ItemsAdded")], aes(x="", y=ItemsAdded, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Items Added by SWAN Libraries", x="Libraries", y="Items")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ggplot(aggdf[c("Library", "Year", "PatronsAdded")], aes(x="", y=PatronsAdded, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Patrons Added by SWAN Libraries", x="Libraries", y="Patrons")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ncol = 2
)

ggsave(file="Libraries/_System/SymphonyMaterialsAddedYear.png", materialAdd, height=8.5, width=11, units="in") 


# CIRCULATION GRAPHS BY MONTH
aggdf <- aggregate(.~Library+Month, LibData[c("Library", "Month", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                              "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum)
aggdf$Month <- factor(aggdf$Month, levels = month.name)

circActivity <- grid.arrange(
  ggplot(aggdf[c("Library", "Month", "CHKOUTS")], aes(x="", y=CHKOUTS, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkouts by SWAN Libraries", x="Libraries", y="Checkouts") + 
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "CHKINS")], aes(x="", y=CHKINS, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkins by SWAN Libraries", x="Libraries", y="Checkins")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "RENEWALS")], aes(x="", y=RENEWALS, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Renewals by SWAN Libraries", x="Libraries", y="Renewals")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "HOLDS")], aes(x="", y=HOLDS, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Holds by SWAN Libraries", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "ILL")], aes(x="", y=ILL, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Inter-Library Loans by SWAN Libraries", x="Libraries", y="Loans")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/SymphonyCircActivityMonth.png", circActivity, height=11, width=8.5, units="in") 


# MATERIALS BY MONTH
materialAdd <- grid.arrange(
  ggplot(aggdf[c("Library", "Month", "BibsAdded")], aes(x="", y=BibsAdded, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Bibs Added by SWAN Libraries", x="Libraries", y="Bibs")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ggplot(aggdf[c("Library", "Month", "ItemsAdded")], aes(x="", y=ItemsAdded, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Items Added by SWAN Libraries", x="Libraries", y="Items")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ggplot(aggdf[c("Library", "Month", "PatronsAdded")], aes(x="", y=PatronsAdded, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=2)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Patrons Added by SWAN Libraries", x="Libraries", y="Patrons")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +   
    scale_fill_manual(values = seabornPalette),
  ncol = 2
)

ggsave(file="Libraries/_System/SymphonyMaterialsAddedMonth.png", materialAdd, height=8.5, width=11, units="in") 


# CHECKOUTS DISTRIBUTION BY YEAR
aggdf <- aggregate(.~Library+Year, LibData[c("Library", "Year", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                             "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=sum)
graph <- ggplot(data=aggdf, aes(x=CHKOUTS)) + scale_x_continuous(labels = comma) +
  geom_histogram(aes(fill = ..count..)) + facet_wrap(~Year, nrow=4) +
  labs(title="Distribution of Checkouts", x="Checkouts", y="Count") +
  scale_fill_gradient("Count", low = "#56B1F7", high = "#132B43") + scale_colour_hue(l=70)

outputPlot(graph, "CheckoutsDistribution.png")

# PATRONS ADDED DISTRIBUTION
graph <- ggplot(data=aggdf, aes(x=PatronsAdded)) + scale_x_continuous(labels = comma) +
  geom_histogram(aes(fill = ..count..)) + facet_wrap(~Year, nrow=4) +
  labs(title="Distribution of Patrons Added", x="Patrons", y="Count") +
  scale_fill_gradient("Count", low = "#56B1F7", high = "#132B43") + scale_colour_hue(l=70)

outputPlot(graph, "PatronsAddedDistribution.png")

# CIRCULATION BY ITEMS ADDED SCATTER
circulationItemsScatter <- grid.arrange(
  ggplot(aggdf, aes(x=CHKOUTS, y=ItemsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Checkouts and Items Added", x="Checkouts", y="Items") +   
    scale_x_continuous(labels = comma, limits=c(0,200000)) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),    
  ggplot(aggdf, aes(x=CHKINS, y=ItemsAdded, color=Year)) + geom_point(shape=1) +    
    labs(title="SWAN Library Checkins and Items Added", x="Checkins", y="Items") +  geom_smooth(method=lm, se=FALSE) +  
    scale_x_continuous(labels = comma, limits=c(0,200000)) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=RENEWALS, y=ItemsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Renewals and Items Added", x="Renewals", y="Items") +   
    scale_x_continuous(labels = comma, limits=c(0,20000)) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=HOLDS, y=ItemsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Holds and Items Added", x="Holds", y="Items") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=ILL, y=ItemsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library ILLs and Items Added", x="Loans", y="Items") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10))       
)

ggsave(file="Libraries/_System/CirculationItemsScatterPlot.png", circulationItemsScatter, height=8.5, width=11, units="in") 


# CIRCULATION BY PATRONS ADDED SCATTER
circulationPatronsScatter <- grid.arrange(
  ggplot(aggdf, aes(x=CHKOUTS, y=PatronsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Checkouts and Patrons Added", x="Checkouts", y="Patrons") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,5000)) +
    theme(legend.position="bottom", plot.title = element_text(size=10), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),    
  ggplot(aggdf, aes(x=CHKINS, y=PatronsAdded, color=Year)) + geom_point(shape=1) +    
    labs(title="SWAN Library Checkins and Patrons Added", x="Checkins", y="Patrons") +  geom_smooth(method=lm, se=FALSE) +  
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,5000)) +
    theme(legend.position="bottom", plot.title = element_text(size=10), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=RENEWALS, y=PatronsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Renewals and Patrons Added", x="Renewals", y="Patrons") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,5000)) +
    theme(legend.position="bottom", plot.title = element_text(size=10, face="bold"), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=HOLDS, y=PatronsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library Holds and Patrons Added", x="Holds", y="Patrons") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,5000)) +
    theme(legend.position="bottom", plot.title = element_text(size=10), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=ILL, y=PatronsAdded, color=Year)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="SWAN Library ILLs and Patrons Added", x="Loans", y="Patrons") +   
    scale_x_continuous(labels = comma) + 
    scale_y_continuous(labels = comma, limits=c(0,20000)) +
    theme(legend.position="bottom", plot.title = element_text(size=10), legend.title=element_text(size=8), 
          legend.text=element_text(size=8), legend.key.size = unit(1.15, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10))       
)

ggsave(file="Libraries/_System/CirculationPatronsScatterPlot.png", circulationPatronsScatter, height=8.5, width=11, units="in") 

# TOP 10 BY CHECKOUT
aggdf <- aggdf[with(aggdf, order(Year, -CHKOUTS)),]
dfList <- by(aggdf, aggdf['Year'], head, n=10)
toplibs <- Reduce(rbind, dfList)

graph <- ggplot(toplibs[c("Library", "Year", "CHKOUTS")], aes(x=Library, y=CHKOUTS, fill=factor(Library))) +
  geom_bar(stat="identity") + guides(fill=FALSE) + 
  scale_y_continuous(labels = comma, limits=c(0,600000)) +
  labs(title="Checkouts by Top 10 SWAN Libraries", x="Libraries", y="Checkouts") +  
  geom_text(aes(label=Library), vjust=-0.2, size=2.5, family=c("sans")) + scale_fill_manual(values = seabornPalette) +
  theme(axis.text.x=element_blank()) + facet_wrap(~Year, nrow=5, ncol=1)

outputPlot(graph, "SymphonyTopTenbyCheckout.png")


# TOP 10 BY ITEMS ADDED
aggdf <- aggdf[with(aggdf, order(Year, -ItemsAdded)),]
dfList <- by(aggdf, aggdf['Year'], head, n=10)
toplibs <- Reduce(rbind, dfList)

graph <- ggplot(toplibs[c("Library", "Year", "ItemsAdded")], aes(x=Library, y=ItemsAdded, fill=factor(Library))) +
  geom_bar(stat="identity", width=0.75) + guides(fill=FALSE) + 
  scale_y_continuous(labels = comma, limits=c(0,25000)) + 
  labs(title="Items Added by Top 10 SWAN Libraries", x="Libraries", y="Items") + 
  geom_text(aes(label=Library), vjust=-0.2, size=2.5, family=c("sans")) + scale_fill_manual(values = seabornPalette) +
  theme(axis.text.x=element_blank()) + facet_wrap(~Year, nrow=5, ncol=1)

outputPlot(graph, "SymphonyTopTenbyItems.png")


# TOP 10 BY PATRONS ADDED
aggdf <- aggdf[with(aggdf, order(Year, -PatronsAdded)),]
dfList <- by(aggdf, aggdf['Year'], head, n=10)
toplibs <- Reduce(rbind, dfList)

graph <- ggplot(toplibs[c("Library", "Year", "PatronsAdded")], aes(x=Library, y=PatronsAdded, fill=factor(Library))) +
  geom_bar(stat="identity", width=0.75) + guides(fill=FALSE) + 
  scale_y_continuous(labels = comma, limits=c(0,10000)) + 
  labs(title="Top 10 Libraries by Patrons Added", x="Libraries", y="Patrons") + 
  geom_text(aes(label=Library), vjust=-0.2, size=2.5, family=c("sans")) + scale_fill_manual(values = seabornPalette) +
  theme(axis.text.x=element_blank()) + facet_wrap(~Year, nrow=5, ncol=1)

outputPlot(graph, "SymphonyTopTenbyPatrons.png")


# SECTION DATA
LibData <- readRDS("DATA/SymphonyStats/SymphonyStatsSectionAddData.rds")

LibData$BibsAdded[is.na(LibData$BibsAdded)] <- 0
LibData$ItemsAdded[is.na(LibData$ItemsAdded)] <- 0

aggdf <- aggregate(.~Library+YEAR+CATEGORY, LibData[LibData$CATEGORY != 'TOTAL', 
                                                    c("Library", "YEAR", "CATEGORY", "BibsAdded", "ItemsAdded")], FUN=sum, na.rm=TRUE)

# CHECKOUTS BY CATEGORY
graph <- ggplot(aggdf[c("Library", "YEAR", "CATEGORY", "ItemsAdded")], aes(x="", y=ItemsAdded, fill=factor(YEAR))) +
  stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
  scale_y_continuous(labels = comma) +
  labs(title="Total SWAN Libraries Items Added by Section", x="Libraries", y="Items") +
  theme(legend.position="top", plot.title = element_text(size=11), legend.title=element_text(size=7), 
        legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
        axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
  scale_fill_manual(values = seabornPalette) + facet_wrap(~CATEGORY, ncol=2, scales="free")

outputPlot(graph, "SymphonyCategoryCheckouts.png")


# ITEMS ADDED BY CATEGORY
graph <- ggplot(aggdf[c("Library", "YEAR", "CATEGORY", "ItemsAdded")], aes(x="", y=ItemsAdded, fill=factor(YEAR))) +
  stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
  scale_y_continuous(labels = comma) +
  labs(title="Total SWAN Libraries Items Added by Section", x="Libraries", y="Items") +
  theme(legend.position="top", plot.title = element_text(size=11), legend.title=element_text(size=7), 
        legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
        axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
  scale_fill_manual(values = seabornPalette) + facet_wrap(~CATEGORY, ncol=2, scales="free")

outputPlot(graph, "SymphonyCategoryItems.png")


# BIBS ADDED BY CATEGORY
graph <- ggplot(aggdf[aggdf$BibsAdded > 0, c("Library", "YEAR", "CATEGORY", "BibsAdded")], aes(x="", y=BibsAdded, fill=factor(YEAR))) +
  stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
  scale_y_continuous(labels = comma) +
  labs(title="Total SWAN Libraries Bibs Added by Section", x="Libraries", y="Bibs") +
  theme(legend.position="top", plot.title = element_text(size=11), legend.title=element_text(size=7), 
        legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
        axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
  scale_fill_manual(values = seabornPalette) + facet_wrap(~CATEGORY, ncol=2, scales="free")

outputPlot(graph, "SymphonyCategoryBibs.png")


# MAP GRAPHS
LibData <- readRDS("DATA/SymphonyStats/SymphonyStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))
LibData$Library <- ifelse(LibData$Library == "Oak Park Main", "Oark Park", 
                          ifelse(LibData$Library == "LaGrange", "La Grange", 
                                 ifelse(LibData$Library == "LaGrange Park", "La Grange Park",
                                        ifelse(LibData$Library == "Matteson Area", "Matteson",
                                               ifelse(LibData$Library == "South Suburban College Learning Resources Center","South Suburban College",
                                                      ifelse(LibData$Library == "The Morton Arboretum Sterling Morton", "Morton Arboretum",
                                                             ifelse(LibData$Library == "Markham Bradford Anderson Oglesby", "Markham", LibData$Library)))))))

LibData$BibsAdded <- as.numeric(LibData$BibsAdded)

LocData <- read.csv("DATA/SymphonyStats/SwanLibraryAddress.csv")
LocData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LocData$Library)))
LibData <- merge(LibData, LocData, by="Library", all.x=TRUE)


aggdf <- aggregate(.~LibID + Library + LAT + LON + Year, 
                   data=LibData[c("LibID", "Library", "Year", "LAT", "LON", "CHKOUTS", "CHKINS", "RENEWALS", "HOLDS", "ILL",
                                  "BibsAdded", "ItemsAdded", "PatronsAdded")], FUN=mean, na.action=na.pass)

# CIRCULATION GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(CHKOUTS)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Checkouts", label=comma) +
    labs(title="SWAN Libraries Average Checkouts by Geography, 2011-2015", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16))
)

ggsave(file="Libraries/_System/SymphonyGeoMapCheckouts.png", geoplots, height=8.5, width=11, units="in") 


# HOLDS GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(HOLDS)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Holds", label=comma) +
    labs(title="SWAN Libraries Average Holds by Geography, 2011-2015", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16))
)

ggsave(file="Libraries/_System/SymphonyGeoMapHolds.png", geoplots, height=8.5, width=11, units="in") 


# ILL GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(ILL)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Inter-Library Loans", label=comma) +
    labs(title="SWAN Libraries Average ILL by Geography, 2011-2015", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16))
)

ggsave(file="Libraries/_System/SymphonyGeoMapILL.png", geoplots, height=8.5, width=11, units="in") 


# ITEMS ADDED GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(ItemsAdded)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Items Added", label=comma) +
    labs(title="SWAN Libraries Average Items Added by Geography, 2011-2015", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16))
)

ggsave(file="Libraries/_System/SymphonyGeoMapItems.png", geoplots, height=8.5, width=11, units="in") 


# PATRONS ADDED GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(PatronsAdded)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Patrons Added", label=comma) +
    labs(title="SWAN Libraries Average Patrons Added by Geography, 2011-2015", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16))
)

ggsave(file="Libraries/_System/SymphonyGeoMapPatrons.png", geoplots, height=8.5, width=11, units="in") 


#########################
## SIRSIDYNIX STATS
#########################

LibData <- readRDS("DATA/SirsiDynixStats/SirsiDynixStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))

aggdf <- aggregate(.~Library + Year, LibData[c("Library", "Year", "Checkouts", "Checkins", "Renewals", 
                                               "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum)

# CIRCULATION BY YEAR
circActivity <- grid.arrange(
  ggplot(aggdf[c("Library", "Year", "Checkouts")], aes(x="", y=Checkouts, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkouts by SWAN Libraries", x="Libraries", y="Checkouts") +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "Checkins")], aes(x="", y=Checkins, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkins by SWAN Libraries", x="Libraries", y="Checkins")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "Renewals")], aes(x="", y=Renewals, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Renewals by SWAN Libraries", x="Libraries", y="Renewals")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "Holds")], aes(x="", y=Holds, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Holds by SWAN Libraries", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "ILL_Loans")], aes(x="", y=ILL_Loans, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Inter-Library Loans by SWAN Libraries", x="Libraries", y="Loans")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.5, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Year", "Patrons_Added")], aes(x="", y=Patrons_Added, fill=factor(Year))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Year")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Patrons Added by SWAN Libraries", x="Libraries", y="Patrons Added")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/SirsiDynixCircActivityYear.png", circActivity, height=11, width=8.5, units="in") 


# CIRCULATION BY MONTH
aggdf <- aggregate(.~Library + Month, LibData[c("Library", "Month", "Checkouts", "Checkins", "Renewals", 
                                              "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum)
aggdf$Month <- factor(aggdf$Month, levels = month.name)

# CIRCULATION BY YEAR
circActivity <- grid.arrange(
  ggplot(aggdf[c("Library", "Month", "Checkouts")], aes(x="", y=Checkouts, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkouts by SWAN Libraries", x="Libraries", y="Checkouts") +
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "Checkins")], aes(x="", y=Checkins, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Checkins by SWAN Libraries", x="Libraries", y="Checkins")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "Renewals")], aes(x="", y=Renewals, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Renewals by SWAN Libraries", x="Libraries", y="Renewals")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "Holds")], aes(x="", y=Holds, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Holds by SWAN Libraries", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "ILL_Loans")], aes(x="", y=ILL_Loans, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Month", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Inter-Library Loans by SWAN Libraries", x="Libraries", y="Loans")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.5, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("Library", "Month", "Patrons_Added")], aes(x="", y=Patrons_Added, fill=factor(Month))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge", width=0.5) + guides(fill=guide_legend(title="Monthr", nrow=3)) + 
    scale_y_continuous(labels = comma) +
    labs(title="Total Patrons Added by SWAN Libraries", x="Libraries", y="Patrons Added")+
    theme(legend.position="bottom", plot.title = element_text(size=11), legend.title=element_text(size=7), 
          legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/SirsiDynixCircActivityMonth.png", circActivity, height=11, width=8.5, units="in") 


# TOP TEN LIBRARIES IN CHECKOUTS
aggdf <- aggregate(.~Library + Year, LibData[c("Library", "Year", "Checkouts", "Checkins", "Renewals", 
                                               "Holds",  "ILL_Loans", "Patrons_Added")], FUN=sum)

aggdf <- aggdf[with(aggdf, order(Year, -Checkouts)),]
dfList <- by(aggdf, aggdf['Year'], head, n=10)
toplibs <- Reduce(rbind, dfList)

graph <- ggplot(toplibs[c("Library", "Year", "Checkouts")], aes(x=Library, y=Checkouts, fill=factor(Library))) +
  geom_bar(stat="identity") + guides(fill=FALSE) + scale_y_continuous(labels = comma, limits=c(0,600000)) +
  labs(title="Total Checkouts by SWAN Libraries", x="Libraries", y="Checkouts") +  
  geom_text(aes(label=Library), vjust=-0.2, size=2.5, family=c("sans")) + scale_fill_manual(values = seabornPalette) +
  theme(axis.text.x=element_blank()) + facet_wrap(~Year, nrow=5, ncol=1)

outputPlot(graph, "SirsiDynixTopTenbyCheckouts.png")


# TOP TEN LIBRARIES IN PATRONS
aggdf <- aggdf[with(aggdf, order(Year, -Patrons_Added)),]
dfList <- by(aggdf, aggdf['Year'], head, n=10)
toplibs <- Reduce(rbind, dfList)

graph <- ggplot(toplibs[c("Library", "Year", "Patrons_Added")], aes(x=Library, y=Patrons_Added, fill=factor(Library))) +
  geom_bar(stat="identity") + guides(fill=FALSE) + scale_y_continuous(labels = comma) +
  labs(title="Total Patrons Added by SWAN Libraries", x="Libraries", y="Patrons") +  
  geom_text(aes(label=Library), vjust=-0.2, size=2.5, family=c("sans")) + scale_fill_manual(values = seabornPalette) +
  theme(axis.text.x=element_blank()) + facet_wrap(~Year, nrow=5, ncol=1)

outputPlot(graph, "SirsiDynixTopTenbyPatron.png")


# CHECKOUTS SECTION DATA
dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
LibData <- dbGetQuery(dbconn, "SELECT * FROM CheckoutsData")
dbDisconnect(dbconn)

# TOP TEN SECTIONS
aggdf <- aggregate(.~Section + Year, LibData[c("Year", "Section", "Checkouts")], FUN=sum)

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

outputPlot(graph, "SirsiDynixTopTenCategoryCheckouts.png")


# CHECKINS SECTION DATA
dbconn <- dbConnect(SQLite(), dbname = "DATA/SirsiDynixStats/SirsiDynixStatsSectionData.db")
LibData <- dbGetQuery(dbconn, "SELECT * FROM CheckinsData")
dbDisconnect(dbconn)

aggdf <- aggregate(.~Source + Year, LibData[LibData$LibID != LibData$Source, c("Year", "Source", "CheckIns")], FUN=sum)
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

outputPlot(graph, "SirsiDynixTopTenCategoryCheckins.png")



# MAP GRAPHS
LibData <- readRDS("DATA/SirsiDynixStats/SirsiDynixStatsData.rds")
LibData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LibData$Library)))
LibData$Library <- ifelse(LibData$Library == "Oak Park Main", "Oark Park", 
                          ifelse(LibData$Library == "LaGrange", "La Grange", 
                                 ifelse(LibData$Library == "LaGrange Park", "La Grange Park",
                                        ifelse(LibData$Library == "Matteson Area", "Matteson",
                                               ifelse(LibData$Library == "South Suburban College Learning Resources Center","South Suburban College",
                                                      ifelse(LibData$Library == "The Morton Arboretum Sterling Morton", "Morton Arboretum",
                                                             ifelse(LibData$Library == "Markham Bradford Anderson Oglesby", "Markham", LibData$Library)))))))


LocData <- read.csv("DATA/SymphonyStats/SwanLibraryAddress.csv")
LocData$Library <- trimws(gsub("\\s+", " ", gsub("Public|Library|District|--", "", LocData$Library)))
LibData <- merge(LibData, LocData, by="Library", all.x=TRUE)


aggdf <- aggregate(.~Library + LAT + LON + Year, 
                   data=LibData[c("Library", "Year", "LAT", "LON", "Checkouts", "Checkins", "Renewals", 
                                  "Holds",  "ILL_Loans", "Patrons_Added")], FUN=mean, na.action=na.pass)

# CIRCULATION GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(Checkouts)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Checkouts", label=comma) +
    labs(title="SWAN Libraries Average Checkouts by Geography, 2015-2017", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16)) 
)

ggsave(file="Libraries/_System/SirsiDynixGeoMapCheckouts.png", geoplots, height=8.5, width=11, units="in") 


# HOLDS GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(Holds)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Holds", label=comma) +
    labs(title="SWAN Libraries Average Holds by Geography, 2015-2017", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16)) 
)

ggsave(file="Libraries/_System/SirsiDynixGeoMapHolds.png", geoplots, height=8.5, width=11, units="in") 

# ILL GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(ILL_Loans)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Inter-Library Loans", label=comma) +
    labs(title="SWAN Libraries Average ILL by Geography, 2015-2017", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16)) 
)

ggsave(file="Libraries/_System/SirsiDynixGeoMapILL.png", geoplots, height=8.5, width=11, units="in") 

# Patrons Added GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("county"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="olivedrab3" ) +
    coord_map(xlim = c(-88.15, -87.5),ylim = c(41.33, 42)) + 
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(Patrons_Added)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=Library), colour="darkgreen", size=3) +
    scale_size(name="Patrons Added", label=comma) +
    labs(title="SWAN Libraries Average Patrons Added by Geography, 2015-2017", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16)) 
)

ggsave(file="Libraries/_System/SirsiDynixGeoMapPatrons.png", geoplots, height=8.5, width=11, units="in") 

