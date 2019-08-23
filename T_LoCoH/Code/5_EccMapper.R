#B. Raynor
#Plots GPS points, isopleths sorted by eccentricity on open street map

library(ggmap)
library(ggplot2)
library(ggsn)
library(RColorBrewer)
library(rgdal)
library(cowplot)
library(rgeos)
library(maptools)

#Set working directory to github rabies folder
setwd("~/2019_DogMovementCode")

###########################################################################################################
#Load Data
###########################################################################################################
data1 <- read.csv("Data/CleanedData_streets.csv")
data1 <- data1[3:7]
colnames(data1) <- c("UniqueID", "Date", "Time", "X", "Y")


#Combine time stamp
data1$Timestamp <- do.call(paste, data1[,c("Date", "Time")])
data1$Timestamp <- as.POSIXct(data1$Timestamp,format="%Y-%m-%d %H:%M:%S")
data1$Timestamp <- as.numeric(data1$Timestamp)

#Format dataframe
df0 <- data.frame(data1$UniqueID, data1$X, data1$Y, data1$Timestamp)
colnames(df0) <- c("uniqeID", "Latitude", "Longitude", "Timestamp")

#Load Water channels ("torrenteras") and river ("Rio") data 
torrenteras<-read.csv("Data/WaterChannels/TORRENTERAS.csv") #water channels
torrenteras <- torrenteras[, c("ident","long","lat")] #store long and lat fields
rio<-read.csv("Data/WaterChannels/Rio Chili.csv")
rio <- rio[, c("ident","long","lat")] #store long and lat fields

###########################################################################################################
#Loop
###########################################################################################################
#set up names for loop
ID <- unique(df0$uniqeID); #ID <- levels(droplevels(ID)); ID <- as.data.frame(ID)
ID <- unique(df0$uniqeID); ID <- levels(droplevels(ID))
IDLength <- length(ID)

for(i in 2:IDLength) {
  #subset data by dog of interest
  dogname <- ID[i]
  df <- subset.data.frame(df0, uniqeID==dogname)
  df <- as.data.frame(df)
  n1 <- 10500
  df <-  na.omit(df)
  
  #Load tlocoh shape files
  path <- paste("~/2019_DogMovementCode/T_LoCoH/Output/Shapefiles", sep="/") ###############################################
  setwd(path)
  sh.name <- paste(dogname, ".iso.srt-ecc.iso-q.h", n1, ".i5.00.iso.shp", sep="" )
  while (!file.exists(sh.name)) {
    n1 <- n1-1
    sh.name <- paste(dogname, ".iso.srt-ecc.iso-q.h", n1, ".i5.00.iso.shp", sep="" )
  } 
  
  sh <- readOGR(sh.name)
  sh <- spTransform(sh, CRS("+proj=longlat +datum=WGS84"))
  sh <- fortify(sh)
  sh.0 <- subset(sh, id==0) #0.95 isopleth
  sh.1 <- subset(sh, id==1) #0.75 isopleth
  sh.2 <- subset(sh, id==2) #0.5 isopleth
  sh.3 <- subset(sh, id==3) #0.25 isopleth
  sh.4 <- subset(sh, id==4) #0.1` isopleth
  
  ############################################################################################################
  #Map
  ############################################################################################################
  #Choose map bounderies
  Lat.T <- -16.37 #Northern most lat
  Lat.B <- -16.42 #Southern most lat
  Lon.L <- -71.55 #Western most lon
  Lon.R <- -71.48 #Eastern most lon
  
  
  map1 <- get_stamenmap(bbox= c(left = Lon.L, bottom = Lat.B,
                                right = Lon.R, top = Lat.T),
                        maptype = "terrain", zoom=15)
  
  #get bounding box for map in order to properly set scalebar
  bb <- attr(map1, "bb")
  bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))
  
  #set up manual colors
  isopleth.color <- c("mediumpurple1", "slateblue", "mediumblue", "darkmagenta", "red")
  isopleth.level <- c("0.95", "0.75", "0.5", "0.25", "0.1")
  
  #real map
  p0 <- ggmap(map1) +
    geom_path(data=torrenteras, aes(x= long, y= lat), size=1)+ #maps torrenteras
    geom_path(data=rio, aes(x= long, y= lat), size=2)+ #maps rio
    geom_polygon(data=sh.0, aes(x = long, y = lat, group = group, fill=isopleth.color[1]), fill="mediumpurple1", alpha=0.4)+ #.95
    geom_polygon(data=sh.1, aes(x = long, y = lat, group = group, fill=isopleth.color[2]), fill="slateblue", alpha=0.3)+ #0.75
    geom_polygon(data=sh.2, aes(x = long, y = lat, group = group, fill=isopleth.color[3]), fill="mediumblue",  alpha=0.3)+ #0.5
    geom_polygon(data=sh.3, aes(x = long, y = lat, group = group, fill=isopleth.color[4]), fill="darkmagenta",  alpha=0.3)+ #0.25
    geom_polygon(data=sh.4, aes(x = long, y = lat, group = group, fill=isopleth.color[5]), fill="red",  alpha=0.3)+ #0.1
    ggtitle(dogname) +
    labs(y= "Latitude", x = "Longitude")+
    geom_point(data = df, aes(x = Longitude, y = Latitude), color="black", size = 0.01)+ #GPS
    theme(legend.position = "none",
          axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    scalebar(data=bb2, dist=1, dd2km=TRUE, model = "WGS84", location= "topleft",
             anchor = c(
               x = bb$ll.lon + 0.1 * (bb$ur.lon - bb$ll.lon),
               y = bb$ll.lat + 0.9 * (bb$ur.lat - bb$ll.lat)
             ))
  
  #savemap
  setwd("~/2019_DogMovementCode/T_LoCoH/Output/EccMaps")
  png(paste(dogname, "Ecc.png", sep=""), width=8, height=4, units="in", res=300) 
  print(p0)
  dev.off()
}
