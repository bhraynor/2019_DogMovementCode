#B. Raynor
#1.Load and format data frame
#2.Find farthest GPS location from house for each dog
#### Code is shared but not reference file with home 
#### location to maintain privacy of dog owners
#3.Basic stats by group
#4.Kruskal Wallis p tests

###################################################################################
#1. Load and format data frame
###################################################################################
library(dplyr)
library(ggpubr)
require(sp)
require(rgdal)

#set working directory
setwd("~/2019_DogMovementCode")

#load t-locoh metric data
df.1 <- read.csv("Analysis/Output/DogMetrics.csv")
df.1 <- df.1[3:6]
colnames(df.1) <- c("UniqueID", "HR", "UD", "ECC")
df.1 <- transform(df.1, HR=HR/(1000^2), UD= UD/(1000^2)) #change m -> km

#load dog info.. has different grouping
df.ref <- read.csv("Data/Dog_Reference.csv")
df.ref <- df.ref[1:7]
#Environment legend: 1=urban, 2=periurban near torrenteras, 3= periurban far from torrenteras

#merge to have one df
df <- merge(df.1, df.ref, by="UniqueID")

###################################################################################
#2. Find farthest GPS location from house for each dog
###################################################################################
# #Home location dataset not shared for privacy of owners
# #Here is code for reference but without the home location file
# 
# #Upload house locations
# houses <- read.csv("---") 
# houses <- houses[2:4]
# 
# #set up loop
# distance.table <- NULL
# for(i in 1:IDLength) {
#   #subset for each dog
#   dog <- ID[i]
#   df4 <- subset(df1, UniqueId==dog)
#   df4 <- na.omit(df4)
#   house.ref <- subset(houses, UniqueID==dog)
#   
#   #calculate distance of all points from house
#   x.dist <- df4$X - house.ref[1,2] #calculate x distance
#   y.dist <- df4$Y - house.ref[1,3] #calculate y distance
#   xy.dist <- sqrt(x.dist^2+ y.dist^2) #pythagorean theorum
#   
#   #find max distance and record
#   max.dist <- max(xy.dist)/1000
#   distance.table <- rbind(distance.table, data.frame(dog, max.dist))
# }
# 
# #record max distances to main df as "dist" column
# df$dist <- distance.table$max.dist
# 

###################################################################################
#3. Run basics stats by reference group
###################################################################################
#choose category by second term in group_by(df, ????)
#### ECCENTRICITY ####
df.ecc <- group_by(df, TorrenterasUsage) %>%
  summarise(
    count = n(),
    mean = mean(ECC, na.rm = TRUE),
    sd = sd(ECC, na.rm = TRUE),
    median = median(ECC, na.rm = TRUE),
    IQR = IQR(ECC, na.rm = TRUE),
    min=min(ECC, na.rm=TRUE),
    max=max(ECC, na.rm=TRUE)
  )
#box plot
ggboxplot(df, x = "TorrenterasUsage", y = "ECC", 
          color = "TorrenterasUsage", palette = c("blue", "#E7B800", "red"),
          ylab = "Average Eccentricity", xlab = "Water Channel Usage")

#### CORE HOMERANGE ####
df.hr <- group_by(df, TorrenterasUsage) %>%
  summarise(
    count = n(),
    mean = mean(HR, na.rm = TRUE),
    sd = sd(HR, na.rm = TRUE),
    median = median(HR, na.rm = TRUE),
    IQR = IQR(HR, na.rm = TRUE),
    min=min(HR, na.rm=TRUE),
    max=max(HR, na.rm=TRUE)
  )
#box plot
ggboxplot(df, x = "TorrenterasUsage", y = "HR0.5",
          color = "TorrenterasUsage", palette = c("blue", "#E7B800", "red"),
          #order = c(1:3),
          ylab = "Core Homerange", xlab = "Water Channel Usage")


#### EXTENDED HOME RANGE ####
df.ud <- group_by(df, TorrenterasUsage) %>%
  summarise(
    count = n(),
    mean = mean(UD, na.rm = TRUE),
    sd = sd(UD, na.rm = TRUE),
    median = median(UD, na.rm = TRUE),
    IQR = IQR(UD, na.rm = TRUE),
    min=min(UD, na.rm=TRUE),
    max=max(UD, na.rm=TRUE)
  )
#box plot
ggboxplot(df, x = "TorrenterasUsage", y = "UD", 
          color = "TorrenterasUsage", palette = c("blue", "#E7B800", "red"),
          #order = c(1:3),
          ylab = "Extended home range", xlab = "Water Channel Usage")

#### MAX DISTANCE FROM HOUSE ####
df.dist <- group_by(df, TorrenterasUsage) %>%
  summarise(
    count = n(),
    mean = mean(dist, na.rm = TRUE),
    sd = sd(dist, na.rm = TRUE),
    median = median(dist, na.rm = TRUE),
    IQR = IQR(dist, na.rm = TRUE),
    min=min(dist, na.rm=TRUE),
    max=max(dist, na.rm=TRUE)
  )
#box plot
ggboxplot(df, x = "TorrenterasUsage", y = "dist", 
          color = "TorrenterasUsage", palette = c("blue", "#E7B800", "red"),
          #order = c(1:3),
          ylab = "Max distance from house", xlab = "Water Channel Usage")



###################################################################################
#4. Kruskal Wallis p tests
###################################################################################
#kruskal.test(variable of interest, grouping, data=df)
kruskal.test(ECC ~ TorrenterasUsage, data = df)
kruskal.test(HR0.5 ~ TorrenterasUsage, data = df)
kruskal.test(UD ~ TorrenterasUsage, data = df)
kruskal.test(dist ~ TorrenterasUsage, data = df)
