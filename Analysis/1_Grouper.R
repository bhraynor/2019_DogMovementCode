#B. Raynpr
#Code counts number of points per dog falls within the water channel buffer
#sorts dogs into groups based on water channel usage
#Points spent in a buffer around the home were not included 
###home data not included for dog owners privacy
#Groups dogs via kmeans based on time spent in water channels

############################################################################################################
#Load data
############################################################################################################
#packages
library(classInt)
library(Gmedian)
library(cluster)
library(sp)
library(rgdal)
library(maps)
library(plyr)

#load GPS dog data (UTM)
setwd("~/2019_DogMovementCode")
dogs <- read.csv("Data/CleanedData.csv")
dogs <- na.omit(dogs)

#count number of observations for each dog
df.obs <- ddply(dogs, .(UniqueId), summarize, TotalPoints = length(UniqueId))

#load dogs' houses (UTM)
houses <- read.csv("---") #not included to protect owner's privacy
houses <- houses[2:4]

############################################################################################################
#Don't check GPS points that are within r distance of dogs' houses
############################################################################################################
r <- 100 #set to radius you want around the house in meters

ID <- unique(houses$UniqueID)
ID <- levels(droplevels(ID))

IDLength <- length(ID)#load torrenteras buffer data
setwd("~/2019_DogMovementCode/Data/WaterChannels")
Tor2  <- readOGR(".", "TOR2A_MMELGAR_HUNTER_bufferlonglat")
Tor3   <- readOGR(".", "TOR3_MRFLES_HUNTER_bufferlonglat")
Tor4 <- readOGR(".", "TOR4_ASA_PTEGRAU_bufferlonglat")
df.nothouses <-NULL

for(i in 1:IDLength) {
  #loop variables
  dog <- ID[i]
  df.sub <- subset(dogs, UniqueId==dog) #subset df for each dog
  house.ref <- subset(houses, UniqueID==dog)
  x.dist <- df.sub$X - house.ref[1,2]
  y.dist <- df.sub$Y - house.ref[1,3]
  xy.dist <- sqrt(x.dist^2+ y.dist^2)
  df.sub <- data.frame(df.sub[2:6], xy.dist)
  df.sub <- subset(df.sub, xy.dist > r)
  df.nothouses <- rbind(df.nothouses, df.sub)
}

df.obsa <- ddply(df.nothouses, .(UniqueId), summarize, TotalPointsNotHouse = length(UniqueId))
df.obs$NotHouseTotal <- df.obsa$TotalPointsNotHouse

############################################################################################################
#Convert UTM to lat long
############################################################################################################
#If using long/lat: convert UTM to long/lat
df.ll <-  na.omit(df.nothouses)
df.ll <- as.data.frame(df.ll)
sputm <- SpatialPoints(cbind(df.ll$X,df.ll$Y), proj4string=CRS("+proj=utm +south +zone=19K +ellps=WGS84"))
spgeo<-spTransform(sputm,CRS("+proj=longlat +datum=WGS84"))
spgeo<- as.data.frame(spgeo)
#format df
df <- cbind.data.frame(df.ll$UniqueId, df.ll$Date, df.ll$Time, spgeo$coords.x2, spgeo$coords.x1)
colnames(df) <- c("uniqeID", "Date", "Time", "Latitude", "Longitude")

############################################################################################################
#Find percent of GPS points in the water channets
############################################################################################################
#loop through all dogs to find number of points in Torrenteras 2
df.obs2 <-NULL
for(i in 1:IDLength) {
  dog <- ID[i]
  df.sub <- subset(df, uniqeID==dog)   #subset by dog
  coordinates(df.sub) <- ~ Longitude + Latitude   #change data frame to spatial points data frame
  proj4string(df.sub) <- proj4string(Tor2)   #data points and polygon are in same projection
  inside.tor2 <- over(df.sub, as(Tor2, "SpatialPolygons")) #find overlap of points in polygon
  inside.tor2 <- as.data.frame(inside.tor2) #convert to data frame
  inside.tor2 <- subset(inside.tor2, inside.tor2 == 1) #subset to have only overlapped points
  l2 <-length(inside.tor2$inside.tor2) #count number of overlapping points
  df.sub <- data.frame(dog, l2) #save as df
  df.obs2 <- rbind(df.obs2, df.sub) #bind to table recording all dogs
}

#loop through all dogs to find number of points in Torrenteras 3
df.obs3 <-NULL
for(i in 1:IDLength) {
  dog <- ID[i]
  df.sub <- subset(df, uniqeID==dog) #subset df for each dog
  coordinates(df.sub) <- ~ Longitude + Latitude
  proj4string(df.sub) <- proj4string(Tor3)
  inside.tor3 <- over(df.sub, as(Tor3, "SpatialPolygons"))
  inside.tor3 <- as.data.frame(inside.tor3)
  inside.tor3 <- subset(inside.tor3, inside.tor3 == 1)
  l3 <-length(inside.tor3$inside.tor3)
  df.sub <- data.frame(dog, l3)
  df.obs3 <- rbind(df.obs3, df.sub)
}

#loop through all dogs to find number of points in Torrenteras 4
df.obs4 <-NULL
for(i in 1:IDLength) {
  dog <- ID[i]
  df.sub <- subset(df, uniqeID==dog) #subset df for each dog
  coordinates(df.sub) <- ~ Longitude + Latitude
  proj4string(df.sub) <- proj4string(Tor4)
  inside.tor4 <- over(df.sub, as(Tor4, "SpatialPolygons"))
  inside.tor4 <- as.data.frame(inside.tor4)
  inside.tor4 <- subset(inside.tor4, inside.tor4 == 1)
  l4 <-length(inside.tor4$inside.tor4)
  df.sub <- data.frame(dog, l4)
  df.obs4 <- rbind(df.obs4, df.sub)
}

#save all obervations lengths to single df
df.obs$Torr2Points <- df.obs2$l2
df.obs$Torr3Points <- df.obs3$l3
df.obs$Torr4Points <- df.obs4$l4
df.obs$TotalTorrPoints <- df.obs$Torr2Points + df.obs$Torr3Points + df.obs$Torr4Points
df.obs$TorrPercent <- df.obs$TotalTorrPoints/df.obs$NotHouseTotal

#save df as a csv
setwd("~/2019_DogMovementCode/Analysis/Output")
write.csv(df.obs, "TimeTorrUsage.csv") 

############################################################################################################
#Kmeans to group dogs
############################################################################################################
df.kmeans <- read.csv("TimeTorrUsage.csv")

K1 <- subset(df.kmeans, df.kmeans$TorrPercent==0)
df.kmeans <- subset(df.kmeans, !df$TorrPercent==0)

#classIntervals(df$actual.distance, 3, style="kmeans")
K2 <- kmeans(df.kmeans$TorrPercent, 2)

#Data compiled manually into csv: "2019_DogMovementCode/Data/Dog_Reference"
