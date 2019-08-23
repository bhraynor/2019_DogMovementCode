############################################
# M. De la Puente                          #
# 07 January 2019                          #
# This script gets the svalues of all dogs #
# in a csv file                            #
############################################

## packages
library(sp)
library(raster)
library(rgdal)
# install.packages("tlocoh", 
#                  dependencies=T, repos=c("http://R-Forge.R-project.org" ,
#                                          "http://cran.cnr.berkeley.edu"))
require(tlocoh)
library(plyr)

## set path
setwd("~/2019_DogMovementCode")

## upload fuction
source("~/2019_DogMovementCode/T_LoCoH/Code/Rfunction_pre_tlocoh_Amethod.R")

## Upload csv file with combined data
setwd("~/2019_DogMovementCode/Data")
df <- read.csv("CleanedData.csv") #Set to cleaned data file
colnames(df) <- c("Index", "UniqueID", "Date", "Time", "X", "Y")

# Dates are separated by / and by -.
# Make them the same
df$Year <- substr(df$Date, 1, 4)
df$Month <- substr(df$Date, 6, 7)
df$Day <- substr(df$Date, 9, 10)
df$Date <- paste(df$Year, "-", df$Month, "-", df$Day, sep="")
names(df)
df <- df[, c(1:7)]
  
# Set s value. Can be modified when running fuction if needed
time1 <- 4
time2 <- 6
time3 <- 12
time4 <- 24

setwd("~/2019_DogMovementCode/T_LoCoH/Output/MedianSvals")

# For Bethoven
# pre plots
pretlocoh_Amethod(dogname = "Bethoven")
# gives us the values: 0.001 for 6 hrs

# For Blanco
pretlocoh_Amethod(dogname = "Blanco")

# For Bocanegra
pretlocoh_Amethod(dogname = "Bocanegra")

# For Boyca
pretlocoh_Amethod(dogname = "Boyca")

# For Cabeza_Blanca
pretlocoh_Amethod(dogname = "Cabeza_Blanca")

# For Cafetin
pretlocoh_Amethod(dogname = "Cafetin")

# For Dayflu
pretlocoh_Amethod(dogname = "Dayflu")

# For Doki
pretlocoh_Amethod(dogname = "Doki")

# For Dorito
pretlocoh_Amethod(dogname = "Dorito")

# For Draico
pretlocoh_Amethod(dogname = "Draico")

# For Gringasho
pretlocoh_Amethod(dogname = "Gringasho")

# For Kucznski
pretlocoh_Amethod(dogname = "Kucznski")
# cant make projection work

# For Lobo
pretlocoh_Amethod(dogname = "Lobo")

# For Loca
pretlocoh_Amethod(dogname = "Loca")

# For Lunareja
pretlocoh_Amethod(dogname =  "Lunareja")

# For Maycol
pretlocoh_Amethod(dogname = "Maycol")

# For Negra
pretlocoh_Amethod(dogname = "Negra")

# For Negro_I
pretlocoh_Amethod(dogname = "Negro_I")

# For Negro_II
pretlocoh_Amethod(dogname = "Negro_II")

# For Otis
pretlocoh_Amethod(dogname =  "Otis")

# For Tarzan
pretlocoh_Amethod(dogname = "Tarzan")

# For Tofy
pretlocoh_Amethod(dogname = "Tofy")

# For Toto
pretlocoh_Amethod(dogname =  "Toto")

# Join s values
dog_names <- c("Bethoven", "Blanco", "Bocanegra", "Boyca", "Cabeza_Blanca",
               "Cafetin", "Dayflu", "Doki", "Dorito","Draico",
               "Gringasho", 
               "Kucznski", 
               "Lobo", "Loca", "Lunareja", 
               "Maycol", "Negra", "Negro_I", "Negro_II", "Otis", 
               "Tarzan","Tofy","Toto")

# Create a datafrme
svals_list <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("4 hr", "6 hr", "12 hr", "24 hr", "dog")
colnames(svals_list) <- x

#svals_list <- dataframe()
for(i in dog_names){
  print(i)
  median_vals <- read.csv(paste(i, "_median_values_boxplot.csv", sep=""))
  #median_vals <- as.data.frame(t(median_vals))
  median_vals= setNames(data.frame(t(median_vals[,-1])), median_vals[,1])
  median_vals <- round(median_vals, 6)
  median_vals$dog <- i
  svals_list <- rbind(svals_list, median_vals)

    }
# Save csv file
setwd("~/2019_DogMovementCode/T_LoCoH/Output")
write.csv(svals_list, file = "svalues_alldogs.csv", row.names = F)


