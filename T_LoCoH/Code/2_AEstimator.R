#B. Raynor
#Code finds optimized a value 
#returns csv with saved s and a values for each dog:
#"2019_DogMovementCode/T_LoCoH/Output/svalues_alldogs.csv"

###################################################################################
#Load packages, data
###################################################################################
#Load packages
library(raster)
library(rgdal)
require(tlocoh)
library(plyr)

#Set working directory
setwd("~/2019_DogMovementCode")

#Load data
df1 <- read.csv("Data/CleanedData.csv") #Set to cleaned data file
df1 <- na.omit(df1)
colnames(df1) <- c("Index", "UniqueID", "Date", "Time", "X", "Y")


###################################################################################
#Loop through all dogs to find a value
###################################################################################
#Set up data frame to hold amax outputs
Aval_list <- data.frame(matrix(ncol = 3, nrow = 0))
 #function saves s value and computed a max to this data frame
write.csv(Aval_list, "T_LoCoH/Output/amaxvalues.csv")

#set up names for loop
ID <- unique(df1$UniqueID) 
ID <- levels(droplevels(ID))
IDLength <- length(ID)


for(i in 1:IDLength) {
  dogname <- ID[i]
  df <- subset.data.frame(df1, UniqueID==dogname)
  
  ## Convert data to usable form
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
  
  # Set s value
  s_vals <- read.csv("T_LoCoH/Output/svalues_alldogs.csv")
  s_vals1 <- subset.data.frame(s_vals, dog==dogname)
  s_value <- s_vals1[1,2]
  
  # Find amax
  df.lxy <- lxy.nn.add(df.lxy, s=s_value, a=auto.a(nnn=25, ptp=0.98)) #look at amax to estimate
  a_vals <- capture.output(summary(df.lxy))
  a_vals <- (strsplit(a_vals[20], "\\s+"))
  a_vals <- data.frame(matrix(unlist(a_vals), nrow=8, byrow=T))
  a_val <- a_vals[8,]
  a_val <- levels(droplevels(a_val))
  a_val <- as.numeric(a_val)
  
  #save A max value
  Aval_list <- read.csv("T_LoCoH/Output/amaxvalues.csv")
  Aval_list <- Aval_list[2:4]
  Aval_list = rbind(Aval_list, data.frame(dogname, s_value, a_val))
  write.csv(Aval_list, "T_LoCoH/Output/amaxvalues.csv")
}
