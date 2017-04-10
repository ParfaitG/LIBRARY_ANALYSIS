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

##############################
## CHICAGO LIBRARY GRAPHS
##############################

LibData <- readRDS("DATA/ChicagoLibData.rds")
WifiData <- readRDS('DATA/ChicagoWifiData.rds')

# CIRCULATION GRAPHS BY YEAR
aggdf <- aggregate(.~LOCATION + YEAR, LibData[c("LOCATION", "YEAR", "CIRCULATION", 
                                                "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)

circActivity <- grid.arrange(
  ggplot(aggdf[c("LOCATION", "YEAR", "CIRCULATION")], aes(x="", y=CIRCULATION, fill=factor(YEAR))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Circulation (Checkouts/Renewals)", x="Libraries", y="Circulation") +
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "YEAR", "HOLDS")], aes(x="", y=HOLDS, fill=factor(YEAR))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Holds", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "YEAR", "VISITORS")], aes(x="", y=VISITORS, fill=factor(YEAR))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Visitors", x="Libraries", y="Visitors")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "YEAR", "COMPUTER")], aes(x="", y=COMPUTER, fill=factor(YEAR))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Computer Sessions", x="Libraries", y="Computer Sessions")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(WifiData, aes(x="", y=WIFI, fill=factor(YEAR))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="YEAR")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Wifi Sessions", x="Libraries", y="Wifi")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/CircActivityYear.png", circActivity, height=11, width=8.5, units="in") 


# CIRCULATION GRAPHS BY MONTH
aggdf <- aggregate(.~LOCATION + MONTH, LibData[c("LOCATION", "MONTH", "CIRCULATION", 
                                                 "HOLDS", "COMPUTER", "VISITORS")], FUN=sum,  na.action=na.pass)
aggdf$MONTH <- factor(aggdf$MONTH, levels = toupper(month.name))

circActivity <- grid.arrange(
  ggplot(aggdf[c("LOCATION", "MONTH", "CIRCULATION")], aes(x="", y=CIRCULATION, fill=factor(MONTH))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Circulation (Checkouts/Renewals)", x="Libraries", y="Circulation") +
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "MONTH", "HOLDS")], aes(x="", y=HOLDS, fill=factor(MONTH))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Holds", x="Libraries", y="Holds")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "MONTH", "VISITORS")], aes(x="", y=VISITORS, fill=factor(MONTH))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Visitors", x="Libraries", y="Visitors")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5), 
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette),
  
  ggplot(aggdf[c("LOCATION", "MONTH", "COMPUTER")], aes(x="", y=COMPUTER, fill=factor(MONTH))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Computer Sessions", x="Libraries", y="Computer Sessions")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) + 
    scale_fill_manual(values = seabornPalette),
  
  ggplot(WifiData, aes(x="", y=WIFI, fill=factor(MONTH))) +
    stat_summary(fun.y="sum", geom="bar", position="dodge") + guides(fill=guide_legend(title="MONTH")) + 
    scale_y_continuous(labels = comma) +
    labs(title="Wifi Sessions", x="Libraries", y="Wifi")+
    theme(legend.position="bottom", plot.title = element_text(size=11, hjust = 0.5),
          legend.title=element_text(size=7), legend.text=element_text(size=7), legend.key.size = unit(0.75, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)) +      
    scale_fill_manual(values = seabornPalette)
)

ggsave(file="Libraries/_System/CircActivityMonth.png", circActivity, height=11, width=8.5, units="in") 


# CIRCULATION DISTRIBUTION BY YEAR
aggdf <- aggregate(.~LOCATION + YEAR, LibData[c("LOCATION", "YEAR", "CIRCULATION", 
                                                "HOLDS", "COMPUTER", "VISITORS")], FUN=sum, na.action=na.pass)
graph <- ggplot(data=aggdf, aes(x=CIRCULATION)) + scale_x_continuous(labels = comma) +
  geom_histogram(aes(fill = ..count..), bins=5) + facet_wrap(~YEAR, nrow=4) +
  labs(title="Distribution of Circulation", x="Circulation", y="Count") +
  scale_fill_gradient("Count", low = "#56B1F7", high = "#132B43") + scale_colour_hue(l=70) +
  theme(plot.title = element_text(hjust = 0.5))

outputPlot(graph, "CirculationDistribution.png")

# VISITORS DISTRIBUTION BY YEAR
graph <- ggplot(data=aggdf, aes(x=VISITORS)) + scale_x_continuous(labels = comma) +
  geom_histogram(aes(fill = ..count..), bins=5) + facet_wrap(~YEAR, nrow=4) +
  labs(title="Distribution of Visitors", x="Visitors", y="Count") +
  scale_fill_gradient("Count", low = "#56B1F7", high = "#132B43") + scale_colour_hue(l=70) +
  theme(plot.title = element_text(hjust = 0.5))

outputPlot(graph, "VisitorsDistribution.png")


# CIRCULATION TRENDS SCATTER PLOTS
circulationScatter <- grid.arrange(
  ggplot(aggdf, aes(x=HOLDS, y=CIRCULATION, color=YEAR)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="Chicago Library Holds and Circulation", x="Holds", y="Circulation") +   
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    theme(legend.position="bottom", plot.title = element_text(size=10, hjust = 0.5),
          legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(2, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),    
  ggplot(aggdf, aes(x=VISITORS, y=CIRCULATION, color=YEAR)) + geom_point(shape=1) +    
    labs(title="Chicago Library Visitors and Circulation", x="Visitors", y="Circulation") +  geom_smooth(method=lm, se=FALSE) +  
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    theme(legend.position="bottom", plot.title = element_text(size=10, hjust = 0.5),
          legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(2, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10)),
  ggplot(aggdf, aes(x=COMPUTER, y=CIRCULATION, color=YEAR)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    labs(title="Chicago Library Computer Use and Circulation", x="Computer Use", y="Circulation") +   
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    theme(legend.position="bottom", plot.title = element_text(size=10, hjust = 0.5),
          legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(2, "line"),
          axis.text=element_text(size=8),axis.title=element_text(size=10))
)

ggsave(file="Libraries/_System/CirculationTrends.png", circulationScatter, height=11, width=8.5, units="in") 


# TOP TEN LIBRARIES BY CIRCULATION
aggdf <- aggdf[with(aggdf, order(YEAR, -CIRCULATION)),]
dfList <- by(aggdf, aggdf['YEAR'], head, n=10)
toplibs <- Reduce(rbind, dfList)

toplibs$LOCATION <- ifelse(grepl("Harold Washington", toplibs$LOCATION), "Harold Washington", toplibs$LOCATION)
toplibs$LOCATION <- ifelse(grepl("Daley, Richard M.-W Humboldt", toplibs$LOCATION), "Daley W Humboldt", toplibs$LOCATION)

graph <- ggplot(toplibs[c("LOCATION", "YEAR", "CIRCULATION")], aes(x=LOCATION, y=CIRCULATION, fill=factor(LOCATION))) +
  geom_bar(stat="identity") + guides(fill=guide_legend(title="Library Branch")) +  
  scale_y_continuous(labels = comma, expand = c(.1, .1)) +  scale_x_discrete(expand = c(.1, .1)) +
  labs(title="Top Ten Chicago Libraries by Circulation", x="Libraries", y="Circulation") +  
  scale_fill_manual(values = seabornPalette) + facet_wrap(~YEAR, ncol=2, scales = "free") +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5, size=12),
       legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(0.75, "line"),
       axis.text.x=element_blank(),axis.title=element_text(size=10)) 

outputPlot(graph, "TopTenByCirculation.png")


# TOP TEN LIBRARIES BY VISITORS
aggdf <- aggdf[with(aggdf, order(YEAR, -VISITORS)),]
dfList <- by(aggdf, aggdf['YEAR'], head, n=10)
toplibs <- Reduce(rbind, dfList)

toplibs$LOCATION <- ifelse(grepl("Harold Washington", toplibs$LOCATION), "Harold Washington", toplibs$LOCATION)
toplibs$LOCATION <- ifelse(grepl("Daley, Richard M.-W Humboldt", toplibs$LOCATION), "Daley W Humboldt", toplibs$LOCATION)

graph <- ggplot(toplibs[c("LOCATION", "YEAR", "VISITORS")], aes(x=LOCATION, y=VISITORS, fill=factor(LOCATION))) +
  geom_bar(stat="identity") + guides(fill=guide_legend(title="Library Branch")) +  
  scale_y_continuous(labels = comma, expand = c(.1, .1)) +  scale_x_discrete(expand = c(.1, .1)) +
  labs(title="Top Ten Chicago Libraries by Visitors", x="Libraries", y="VISITORS") +  
  scale_fill_manual(values = seabornPalette) + facet_wrap(~YEAR, ncol=2, scales = "free") +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5, size=12),
        legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(0.75, "line"),
        axis.text.x=element_blank(),axis.title=element_text(size=10)) 

outputPlot(graph, "TopTenByVisitors.png")


# TOP TEN LIBRARIES BY COMPUTER
aggdf <- aggdf[with(aggdf, order(YEAR, -COMPUTER)),]
dfList <- by(aggdf, aggdf['YEAR'], head, n=10)
toplibs <- Reduce(rbind, dfList)

toplibs$LOCATION <- ifelse(grepl("Harold Washington", toplibs$LOCATION), "Harold Washington", toplibs$LOCATION)
toplibs$LOCATION <- ifelse(grepl("Daley, Richard M.-W Humboldt", toplibs$LOCATION), "Daley W Humboldt", toplibs$LOCATION)

graph <- ggplot(toplibs[c("LOCATION", "YEAR", "COMPUTER")], aes(x=LOCATION, y=COMPUTER, fill=factor(LOCATION))) +
  geom_bar(stat="identity") + guides(fill=guide_legend(title="Library Branch")) +  
  scale_y_continuous(labels = comma, expand = c(.1, .1)) +  scale_x_discrete(expand = c(.1, .1)) +
  labs(title="Top Ten Chicago Libraries by COMPUTER", x="Libraries", y="COMPUTER") +  
  scale_fill_manual(values = seabornPalette) + facet_wrap(~YEAR, ncol=2, scales = "free") +
  theme(legend.position="top", plot.title = element_text(hjust = 0.5, size=12),
        legend.title=element_text(size=8), legend.text=element_text(size=8), legend.key.size = unit(0.75, "line"),
        axis.text.x=element_blank(),axis.title=element_text(size=10)) 

outputPlot(graph, "TopTenByComputer.png")


# AVG CIRCULATION GRAPHS BY YEAR
aggdf <- aggregate(.~YEAR, LibData[c("YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], FUN=mean)

circActivity <- grid.arrange(
  ggplot(aggdf[c("YEAR", "CIRCULATION")], aes(x=YEAR, y=CIRCULATION)) +
    geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = comma) +
    labs(title="Chicago Libraries Average Checkouts/Renewals", x="", y="Circulation") +
    scale_colour_manual(values = seabornPalette),
  
  ggplot(aggdf[c("YEAR", "HOLDS")], aes(x=YEAR, y=HOLDS)) +
    geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = comma) +
    labs(title="Chicago Libraries Average Holds", x="", y="Holds") +
    scale_colour_manual(values = seabornPalette),
  
  ggplot(aggdf[c("YEAR", "VISITORS")], aes(x=YEAR, y=VISITORS)) +
    geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = comma) +
    labs(title="Chicago Libraries Average Visitors", x="", y="Visitors") +
    scale_colour_manual(values = seabornPalette),
  
  ggplot(aggdf[c("YEAR", "COMPUTER")], aes(x=YEAR, y=COMPUTER)) +
    geom_line(color="grey50") + geom_point(aes(color = factor(YEAR)), size = 2) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = comma) +
    labs(title="Chicago Libraries Average Computer Sessions", x="", y="CPU Sessions") +
    scale_colour_manual(values = seabornPalette)
)

ggsave(file=paste0("Libraries/_System/CirculationAvgByYear.png"), circActivity, height=8.5, width=11, units="in") 


# AVG CIRCULATION GRAPHS BY MONTH
aggdf <- aggregate(.~MONTH + YEAR, LibData[c("MONTH", "YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], FUN=mean)

aggdf$MONTH <- substr(as.character(aggdf$MONTH),1,3)
aggdf$MONTH <- factor(aggdf$MONTH, levels = substr(toupper(month.name), 1, 3))

circActivity <- grid.arrange(
  ggplot(aggdf[c("MONTH", "YEAR", "CIRCULATION")], aes(x=MONTH, y=CIRCULATION, group=factor(YEAR), colour=factor(YEAR))) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = comma) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title="Chicago Libraries Average Checkouts/Renewals", x="", y="Circulation") +
    scale_colour_manual(values = seabornPalette, name = "Year"),
  
  ggplot(aggdf[c("MONTH", "YEAR", "HOLDS")], aes(x=MONTH, y=HOLDS, group=factor(YEAR), colour=factor(YEAR))) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = comma) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title="Chicago Libraries Average Holds", x="", y="Holds") +
    scale_colour_manual(values = seabornPalette, name = "Year"),
  
  ggplot(aggdf[c("MONTH", "YEAR", "VISITORS")], aes(x=MONTH, y=VISITORS, group=factor(YEAR), colour=factor(YEAR))) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = comma) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title="Chicago Libraries Average Visitors", x="", y="Visitors") +
    scale_colour_manual(values = seabornPalette, name = "Year"),
  
  ggplot(aggdf[c("MONTH", "YEAR","COMPUTER")], aes(x=MONTH, y=COMPUTER, group=factor(YEAR), colour=factor(YEAR))) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = comma) + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title="Chicago Libraries Average Computer Sessions", x="", y="CPU Sessions") +
    scale_colour_manual(values = seabornPalette, name = "Year")
)

ggsave(file=paste0("Libraries/_System/CirculationAvgByMonth.png"), circActivity, height=8.5, width=11, units="in") 


# MAP GRAPHS
LocData <- readRDS("DATA/ChicagoLocationData.rds")
LocData$LOCATION <- ifelse(grepl("Harold Washington", LocData$LOCATION), "Harold Washington Library Center", LocData$LOCATION)

LocData <- merge(LocData, LibData, by="LOCATION", all=TRUE)

# AGGREGATE DATA FRAME
aggdf <- aggregate(.~LOCATION + LAT + LON + YEAR, 
                   data=LocData[c("LOCATION", "LAT", "LON", "YEAR", "CIRCULATION", "HOLDS", "COMPUTER", "VISITORS")], FUN=mean)

aggdf$LAT <- as.numeric(aggdf$LAT)
aggdf$LON <- as.numeric(aggdf$LON)
aggdf$YEAR <- as.factor(as.character(aggdf$YEAR))


# NEIGHBORHOOD BORDERS
neighData <- read.csv("DATA/ChicagoNeighborhoods.csv", stringsAsFactors = FALSE)

neighdf <- do.call(rbind, Map(function(p, g, m) {
  temp <- strsplit(gsub(")))", "", gsub("MULTIPOLYGON \\(\\(\\(", "", g)), split="\\s+")

  data.frame(neighborhood = p,
             neighgrp = m,
             LON = as.numeric(temp[[1]][seq(1, length(temp[[1]]), 2)]),
             LAT = as.numeric(gsub(",", "", temp[[1]][seq(2, length(temp[[1]]), 2)])))
  
}, neighData$PRI_NEIGH, neighData$the_geom, seq(nrow(neighData))))

rownames(neighdf) <- NULL


# CIRCULATION GEO PLOT
geoplots <- grid.arrange(
      ggplot() +
       geom_polygon(data=subset(map_data("state"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="#64b5cd" ) +
       geom_polygon(data=neighdf, aes(x=LON, y=LAT, group=neighborhood), colour="white", fill="#64b5cd" ) +
       coord_map(xlim = c(-87.9, -87.5),ylim = c(41.65, 42.03)) + 
       geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(CIRCULATION)), color="#c44e52") + 
       geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=LOCATION), colour="steelblue4", size=3) +
       scale_size(name="Circulation", label=comma) +
       labs(title="Chicago Libraries Circulation by Geography", x="Longitude", y="Latitude") +
       theme(plot.title = element_text(size=16, hjust = 0.5)) 
)

ggsave(file="Libraries/_System/GeoMapCirculation.png", geoplots, height=11, width=8.5, units="in") 

# VISITORS GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("state"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="#64b5cd" ) +
    geom_polygon(data=neighdf, aes(x=LON, y=LAT, group=neighborhood), colour="white", fill="#64b5cd" ) +
    coord_map(xlim = c(-87.9, -87.5),ylim = c(41.65, 42.03)) +
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(VISITORS)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=LOCATION), colour="steelblue4", size=3) +
    scale_size(name="Visitors", label=comma) +
    labs(title="Chicago Libraries Visitors by Geography", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16, hjust = 0.5))
)

ggsave(file="Libraries/_System/GeoMapVisitors.png", geoplots, height=11, width=8.5, units="in") 

# HOLDS GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("state"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="#64b5cd" ) +
    geom_polygon(data=neighdf, aes(x=LON, y=LAT, group=neighborhood), colour="white", fill="#64b5cd" ) +
    coord_map(xlim = c(-87.9, -87.5),ylim = c(41.65, 42.03)) +
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(HOLDS)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=LOCATION), colour="steelblue4", size=3) +
    scale_size(name="Holds", label=comma) +
    labs(title="Chicago Libraries Holds by Geography", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16, hjust = 0.5))
)

ggsave(file="Libraries/_System/GeoMapHolds.png", geoplots, height=11, width=8.5, units="in") 


# COMPUTER GEO PLOT
geoplots <- grid.arrange(
  ggplot() +
    geom_polygon(data=subset(map_data("state"), region=="illinois"), aes(x=long, y=lat, group=group),colour="white", fill="#64b5cd" ) +
    geom_polygon(data=neighdf, aes(x=LON, y=LAT, group=neighborhood), colour="white", fill="#64b5cd" ) +
    coord_map(xlim = c(-87.9, -87.5),ylim = c(41.65, 42.03)) +
    geom_point(data=aggdf, aes(x=LON, y=LAT, size = as.numeric(COMPUTER)), color="#c44e52") + 
    geom_text(data=aggdf, hjust=0.5, vjust=-0.5, aes(x=LON, y=LAT, label=LOCATION), colour="steelblue4", size=3) +
    scale_size(name="CPU Sessions", label=comma) +
    labs(title="Chicago Libraries Computer Sessions by Geography", x="Longitude", y="Latitude") +
    theme(plot.title = element_text(size=16, hjust = 0.5))
)

ggsave(file="Libraries/_System/GeoMapComputer.png", geoplots, height=11, width=8.5, units="in") 

