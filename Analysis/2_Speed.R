#B. Raynor
#This code takes all GPS data from all dogs 
#and compares the speed of each individual 
#dog of its time out of the water channels 
#vs in the water channels
#of dogs house is removed
###House location data are not provided to protect owners 
###privacy
####################################################################################################
#Load and format data
####################################################################################################
#libraries
require(sp)
require(rgdal)
require(maps)
library(plyr)
library(dplyr)
library(ggpubr)

#load data with speed
df.speed <- read.csv("~/Data/SpeedDF.csv")
ID <- unique(df.speed$UniqueID)
ID <- levels(droplevels(ID))
IDLength <- length(ID)

#load torrenteras buffer data
setwd("~/2019_DogMovementCode/Data/WaterChannels") #set working directory
Tor2  <- readOGR(".", "TOR2A_MMELGAR_HUNTER_bufferlonglat")
Tor3   <- readOGR(".", "TOR3_MRFLES_HUNTER_bufferlonglat")
Tor4 <- readOGR(".", "TOR4_ASA_PTEGRAU_bufferlonglat")

#load dogs' houses (UTM)
houses <- read.csv("---")
houses <- houses[2:4]

############################################################################################################
#Take out points were the dog is inside or near its house, because dog is likely sleeping etc
############################################################################################################
r <- 100 #set to radius you want around the house in meters

df.NotHouses <-NULL

for(i in 1:IDLength) {
  #loop variables
  dog <- ID[i]
  df.sub <- subset(df.speed, UniqueID==dog) #subset df for each dog
  house.ref <- subset(houses, UniqueID==dog)
  x.dist <- df.sub$X - house.ref[1,2]
  y.dist <- df.sub$Y - house.ref[1,3]
  house.dist <- sqrt(x.dist^2+ y.dist^2)
  df.sub <- data.frame(df.sub[c(2,3,4,5,6,9,12)], house.dist)
  df.sub <- subset(df.sub, house.dist > r)
  df.NotHouses <- rbind(df.NotHouses, df.sub)
}

############################################################################################################
#Convert to lat/long
############################################################################################################
#If using long/lat: convert UTM to long/lat
df.ll <-  na.omit(df.NotHouses)
df.ll <- as.data.frame(df.ll)
sputm <- SpatialPoints(cbind(df.ll$X,df.ll$Y), proj4string=CRS("+proj=utm +south +zone=19K +ellps=WGS84"))
spgeo<-spTransform(sputm,CRS("+proj=longlat +datum=WGS84"))
spgeo<- as.data.frame(spgeo)
#format df
df <- cbind.data.frame(df.ll$UniqueID, df.ll$Date, df.ll$Time, df.ll$CombinedTime, spgeo$coords.x2, spgeo$coords.x1, df.ll$Speed.meterpersec)
colnames(df) <- c("UniqueID", "Date", "Time", "CombinedTime", "Latitude", "Longitude", "Speed")


####################################################################################################
#Find points in the torrenteras
####################################################################################################
df.torr <-NULL

for(i in 1:IDLength) {
  dog <- ID[i]
  df.sub0 <- subset(df, UniqueID==dog) 
  df.sub <- subset(df, UniqueID==dog) #subset df for each dog
  coordinates(df.sub) <- ~ Longitude + Latitude
  
  #Torr2
  proj4string(df.sub) <- proj4string(Tor2)
  inside.tor2 <- over(df.sub, as(Tor2, "SpatialPolygons"))
  inside.tor2 <- as.data.frame(inside.tor2)
  
  #Torr3
  proj4string(df.sub) <- proj4string(Tor3)
  inside.tor3 <- over(df.sub, as(Tor3, "SpatialPolygons"))
  inside.tor3 <- as.data.frame(inside.tor3)
  
  #Torr4
  proj4string(df.sub) <- proj4string(Tor4)
  inside.tor4 <- over(df.sub, as(Tor4, "SpatialPolygons"))
  inside.tor4 <- as.data.frame(inside.tor4)
  
  #make df
  df.sub0$torr2 <- inside.tor2$inside.tor2
  df.sub0$torr3 <- inside.tor3$inside.tor3
  df.sub0$torr4 <- inside.tor4$inside.tor4
  df.torr <- rbind(df.torr, df.sub0)
}

#Subset data frames
df.InTorr <- subset(df.torr, torr2==1 | torr3==1 | torr4==1)
df.torr[is.na(df.torr)] <- 0
df.NotInTorr <- subset(df.torr, torr2==0 & torr3==0 & torr4==0)



####################################################################################################
#Find points in the torrenteras
####################################################################################################
#loop through each dog to find stats (mean, sd, median, sd, max, min, IQR) and save as new row in blank df
df.SpeedStats <- NULL
for(i in 1:IDLength) {
  dog <- ID[i]
  df.sub <- subset(df.InTorr, UniqueID==dog) #subset for points in torrenteras
  df.sub0 <- subset(df.NotInTorr, UniqueID==dog) #subset for points in torrenteras
  l1 <- length(df.sub$UniqueID); l0 <- length(df.sub0$UniqueID)
  mean1 <- mean(df.sub$Speed); mean0 <- mean(df.sub0$Speed)
  sd1 <- sd(df.sub$Speed); sd0 <- sd(df.sub0$Speed)
  median1 <- median(df.sub$Speed); median0 <- median(df.sub0$Speed)
  IQR1 <- IQR(df.sub$Speed); IQR0 <- IQR(df.sub0$Speed)
  min1 <- min(df.sub$Speed); min0 <- min(df.sub0$Speed)
  max1 <- max(df.sub$Speed); max0 <- max(df.sub0$Speed)
  df.sub <- data.frame(dog, l1, mean1, sd1, median1, IQR1, min1, max1, l0, mean0, sd0, median0, IQR0, min0, max0)
  df.SpeedStats <- rbind(df.SpeedStats, df.sub)
}
colnames(df.SpeedStats) <- c("UniqueID", "#PointsInTorr", "TorrMeanSpeed", "TorrSD", "Torrmedian", "TOrrIQR", "TorrMin", "TorrMax",
                             "#PointsNotInTorr", "NotTorrMeanSpeed", "NotTorrSD", "NotTorrmedian", "NotTOrrIQR", "NotTorrMin", "NotTorrMax")


#save df aS CSV for further analysis
setwd("~/2019_DogMetricsCode/Analysis/Output")
write.csv(df.SpeedStats, "SpeedStatsEachdog.csv") 

###################################################################################################################
#Make df of speed stats by individual dog
###################################################################################################################
df.speed <- group_by(df, UniqueID) %>%
  summarise(
    count = n(),
    mean = mean(Speed.meterpersec, na.rm = TRUE),
    sd = sd(Speed.meterpersec, na.rm = TRUE),
    median = median(Speed.meterpersec, na.rm = TRUE),
    IQR = IQR(Speed.meterpersec, na.rm = TRUE),
    min=min(Speed.meterpersec, na.rm=TRUE),
    max=max(Speed.meterpersec, na.rm=TRUE))

write.csv(df.SpeedStats, "SpeedStats.csv") 

