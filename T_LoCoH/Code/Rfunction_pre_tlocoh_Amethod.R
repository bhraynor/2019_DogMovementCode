# Run the tlocoh using A method
# Args
#  dogname: name of the dog
#  s_value: value for s
#  a_value: value for A in Amethod convex hull
# Return
#  String matrix

pretlocoh_Amethod <- function(dogname)
  { 
  # load data
  df <- subset.data.frame(df, UniqueID==dogname)
  df <- na.omit(df)
  
  ## Convert data to usable form
  #convert to real world coordinate system, convert time info 
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
  
  #differnt data displays
  plot(df.lxy) #plots points w/out map background
  hist(df.lxy) #gives time usage plots

  ## Decide s parameter
  ##From the graph read the value of s where the proportion of time selected hulls is between 0.4 & 0.8 
  # s relates to time period you're looking at: 
  # png(paste("output/", dogname, "_lineplot.png"), width=8, height=4, 
  #      units="in", res=300)
  df.lxy1 <- lxy.ptsh.add(df.lxy) 
  boxplot_data <- lxy.plot.sfinder(df.lxy1, delta.t=3600*c(time1,time2,time3,time4))
  dev.off() # it is the same with dflxy

  # save results of boxplot
  boxplot_matrix <- as.matrix(boxplot_data)
  dog_list <-  boxplot_matrix[[1]]
  median_t1 <- median(dog_list$svals[[1]]) # 4 hrs
  median_t2 <- median(dog_list$svals[[2]]) # 6 hrs
  median_t3 <- median(dog_list$svals[[3]]) # 12 hrs
  median_t4 <- median(dog_list$svals[[4]]) # 24 hrs
  labels <- c(paste(time1,"hr"), paste(time2, "hr"), 
              paste(time3, "hr"), paste(time4, "hr"))
  medians <- c(median_t1, median_t2, median_t3, median_t4)
  table <- data.frame(labels, medians)
  write.csv(table,file = paste(dogname, "_median_values_boxplot.csv", sep=""),
            row.names = F)
  }

