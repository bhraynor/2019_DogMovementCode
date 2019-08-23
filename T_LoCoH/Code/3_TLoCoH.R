#B. Raynor
#1.Code runs t_locoh spatial analasis
#2.Finds and saves spatial metrics (HR, UD, ECC)
###Metric data --> "2019_DogMovementCode/T_LoCoH/Output/DogMetrics.csv"
#3.creates and saves shapefiles isopleths and eccentricity isopleths 
###Shapefiles --> "2019_DogMovementCode/T_LoCoH/Output/Shapefiles" folder

###################################################################################
#Load packages, data
###################################################################################
#Load packages
library(raster)
library(rgdal)
require(tlocoh)
library(plyr)

setwd("~/2019_DogMovementCode")
df1 <- read.csv("Data/CleanedData.csv") #Set to cleaned data file
colnames(df1) <- c("Index", "UniqueID", "Date", "Time", "X", "Y")

####################################################################################
#Loop through all dogs
####################################################################################
#set up names for loop
ID <- unique(df1$UniqueID) 
ID <- levels(droplevels(ID))
IDLength <- length(ID)

#Empty data frame to hold spatial metrics
Dog.metrics <- data.frame(matrix(ncol = 4, nrow = 0))
write.csv(Dog.metrics, "Analysis/Output/DogMetrics.csv")


for(i in 1:IDLength) {
  #get individual dogs' data 
  dogname <- ID[i]
  df <- subset.data.frame(df1, UniqueID==dogname)
  df <- na.omit(df)
  
  #convert GPS data to  real world coordinate system, convert time info 
  df.mat.utm <- data.frame(df$X, df$Y)
  colnames(df.mat.utm) <- c("x","y")
  df.date <- as.character(df$Date); df.time <- as.character(df$Time)
  df.timedata <- paste(df.date, df.time)
  df.utc <- as.POSIXct(df.timedata, tz="UTC")
  
  #Create locoh-xy object
  df.lxy <- xyt.lxy(xy=df.mat.utm, 
                    dt=df.utc, id=dogname, 
                    proj4string=CRS("+proj=utm +south +zone=19K +ellps=WGS84"),
                    show.dup.dt=TRUE)
 
  #get s and a values
  params1 <- read.csv("~/2019_DogMovementCode/T_LoCoH/Output/amaxvalues.csv") 
  params1 <- params1[2:4]
  colnames(params1) <- c("UniqueID","s1", "a1") 
  params <- subset.data.frame(params1, UniqueID==dogname)
  s0 <- params[1,2] 
  a0 <- params[1,3] 
  
  #set IVG (inter-vist gap period)
  t=6 
  
  #Create tlocoh shapefiles 
  df.lxy <- lxy.nn.add(df.lxy, s=s0, a=a0) #hulls
  df.lhs.a1 <- lxy.lhs(df.lxy, s=s0, a=a0, iso.add=T) #isopleths
  df.lhs.a1 <- lhs.ellipses.add(df.lhs.a1) #elipses
  df.lhs.a1 <- lhs.iso.add(df.lhs.a1, sort.metric="ecc") #elongation
  df.lhs.a1 <- lhs.visit.add(df.lhs.a1, ivg=3600*t) #revisitation
  
  #isopleth metrics
  df.isos <- isopleths(df.lhs.a1)
  df.isos <- df.isos[[1]]@data
  HR <- df.isos[3,2] #changed to extract the 50% isopleth
  UD <- df.isos[5,2]
  
  ##calculate eccentricity
  df.ecc <- df.lhs.a1[[1]]
  df.ecc <- df.ecc[[20]]
  df.ecc <- transform(df.ecc, c=sqrt(a^2-b^2))
  df.ecc <- transform(df.ecc, ecc=c/a)
  ECC <- mean(df.ecc$ecc)  
  
  # #Save shapefiles
  setwd("~/2019_DogMovementCode/T_LoCoH/Output/Shapefiles")
  lhs.exp.shp(df.lhs.a1, hpp = TRUE, hulls = FALSE, iso = TRUE,
              iso.idx = NULL, iso.metric = NULL, ellipses = FALSE, dr = FALSE,
              dr.idx = NULL, allpts = FALSE, nn = FALSE, dir = ".",
              file.base = dogname, file.base.auto = TRUE, avl.file = NULL, status = TRUE,
              show.time = TRUE, hm = "all", anv = NULL, hsp = NULL,
              metadata = TRUE)

  #Save spatial metric data
  Dog.metrics <- read.csv( "T_LoCoH/Output/DogMetrics.csv")
  Dog.metrics <- Dog.metrics[2:5]
  Dog.metrics = rbind(Dog.metrics, data.frame(dogname, HR, UD, ECC))
  write.csv(Dog.metrics, "Analysis/Output/DogMetrics.csv")
  
}
