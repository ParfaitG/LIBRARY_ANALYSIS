library(XML)

setwd("C:/Path/To/Working/Directory")

# PARSE MEMBERS WEB PAGE
doc <- htmlParse(readLines("https://support.swanlibraries.net/members/list"))

# EXTRACT SPECIFIC HTML TABLE
tables <- xpathApply(doc, "//table")
df <- readHTMLTable(tables[[1]], stringsAsFactors = FALSE, 
                           header = c("Library", "ILDSDeliveryStop", 
                                      "CatalogLink", "Empty"))
df$Empty <- NULL

# GENERATE LIBID 
df$LibID <- substr(df$Library, nchar(df$Library) - 2, nchar(df$Library))
df$Library <- gsub("\u0095.*", "",df$Library)
df$Library <- substr(df$Library, 1, nchar(df$Library) -1)

# REVIEW AND EXPORT OUTPUT
head(df$Library)
write.csv(df, "DATA/SWANLibraryMembers.csv", row.names =  FALSE)


# LIBRARY ADDRESS MAP DATA
df <- unique(xmlToDataFrame("SWANLibraryAddress.xml", stringsAsFactors = FALSE))
rownames(df) <- NULL
names(df)[1] <- "Library"

df$Address <- gsub("Learning Center ", "", gsub("Learning Resources Center ", "", df$address))
df$Address<- sapply(seq(nrow(df)), function(i)
              gsub("\\.", "", strsplit(df$address[[i]], split=",")[[1]][1]))
df$City <- sapply(seq(nrow(df)), function(i)
              trimws(strsplit(df$address[[i]], split=",")[[1]][2]))
df$Zip<- sapply(seq(nrow(df)), function(i)
              trimws(gsub("IL ", "", strsplit(df$address[[i]], split=",")[[1]][3])))

# RETRIEVE GEOCODES BY ADDRESS VIA BING MAPS API
BingMapsAPIkey <- "*****"
BingMapsAPIkey <- "AiUV813Xbo8CEariNmeksQ0jDJeR8mgbRjbEAQQ9b-OXarp_eKacFoucr-Apndb_"

geodfs <- lapply(seq(nrow(df)), function(i){
  url <- paste0("http://dev.virtualearth.net/REST/v1/Locations/US/IL/", 
                df$Zip[[i]], "/", gsub(" ", "%20", df$City[[i]]), "/", 
                gsub(" ", "%20", df$Address[[i]]), "?o=xml&key=", BingMapsAPIkey)
  
  doc <- xmlParse(url)
  
  data.frame(LAT = as.numeric(xpathSApply(doc, "//ns:GeocodePoint[2]/ns:Latitude", xmlValue, 
                                          namespaces = c(ns="http://schemas.microsoft.com/search/local/ws/rest/v1"))),
             LON = as.numeric(xpathSApply(doc, "//ns:GeocodePoint[2]/ns:Longitude", xmlValue,
                                          namespaces = c(ns="http://schemas.microsoft.com/search/local/ws/rest/v1"))))
})

# FINAL DATAFRAME
df <- cbind(df, do.call(rbind, geodfs))
df <- df[order(df$name),]

write.csv(df, "SwanLibraryAddress.csv", row.names = FALSE)

