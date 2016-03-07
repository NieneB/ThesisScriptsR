# Thesis Methodology Accelerometer files
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 30-10-2015

# This is 2PFunctions.R script
# Defining various functions needed for the analysis of the accelerometer files
# and the matching gps files



################################################################################
### Load Data
################################################################################

### Read csv file of Accelerometer data and assign the right time. 
AccPre <- function(x, date){
  data <- read.csv(file = x , header = T, sep = ";" )
  data$time <- gsub(pattern = ":", x = data$time, replacement = ".")
  options(digits.secs=3)
  data$date <- date
  data$time <- as.POSIXct(strptime(paste(data$date, data$time), "%Y-%m-%d %H.%M.%OS"))
  print("Accelerometer.csv loaded")
  return(data)
}

### Load GPS file and change time to CET tz OUTPUT: Dataframe of GPS with coordinates in RDnew
gpsTimeSerie <- function(x){
  GPS <- readOGR(dsn = paste(x, '.shp', sep='' ), layer = x)
  dfGPS <- as.data.frame(GPS)
  dfGPS$DateTime <- as.POSIXct(x = strptime(dfGPS$time, format= "%Y-%m-%d %H:%M:%S"), tz="CET")
  # attributes(dfGPS$DateTime)$tzone <- "CET"
  print("GPS file loaded as data frame")
  return(dfGPS)
}

################################################################################
### Additional statistics
################################################################################

#### Calculate gereneral values of the accerlerometer data frame
statistics <- function(data){
  data$m <- sqrt((data$x^2)+(data$y^2)+(data$z^2))  ## Overall acceleration
  #   data$SMA <- abs(data$x)+abs(data$y)+abs(data$z)   ## Sum Mean Acceleration
  #   data$v <- acos(data$z-mean(data$z))*180/pi   ## Change in Angle of z 
  #   data$vx <- acos(data$x- mean(data$x))*180/pi  ## change in angle of x 
  #   data$vy <- acos(data$y - mean(data$y))*180/pi ## change in angle of y
  print("Statistics of Accerlerometer data frame compiled")
  return(data)
}

descriptiveStats <- function(x){
  
  print(paste("Seconds: ", nseconds(x)))
  print(paste("Points: ", length(xcoredata(x)$index)))
  
  x <- as.data.frame(x)
  m <- mean(x$z, na.rm = T)
  sd <- sd(x$z, na.rm = T)
  v <- var(x$z, na.rm = T)
  med <- median(x$z, na.rm = T)
  min <- min(x$z, na.rm = T)
  max <- max(x$z, na.rm = T)
  q <- quantile(x$z, na.rm = T)
  q1 <- q[2]
  q3 <- q[4]
  t <- cbind(m,sd,v,min, med, max, q1, q3)
  print(t)
  hist(x$z, xlim = c(0,2))
  boxplot(x$z, ylim =c(0,2))
  # t <- rnorm(brick4$z, mean=mean(brick4$z),sd = sd(brick4$z))
  return(t)
}

################################################################################
### Time series and Plotting
################################################################################

### Create time series of the accelerometer data frame
timeseries <- function(x, date){
  # Load csv file
  Acc <- AccPre(x, date)
  print(head(Acc))
  #Calculate extra stats
  Acc <- statistics(Acc)
  ## Make time series Accelerometer data
  freq <- 5
  tsAccx <-   xts(x = Acc$x, order.by=Acc$time, unique=T, frequency = freq)
  tsAccy <-   xts(x = Acc$y, order.by=Acc$time, unique=T, frequency = freq)
  tsAccz <-   xts(x = Acc$z, order.by=Acc$time, unique=T, frequency = freq)
  tsAccm <-   xts(x = Acc$m, order.by=Acc$time, unique=T, frequency = freq)
  tsAccg <-   xts(x = Acc$gforce, order.by=Acc$time, unique=T, frequency = freq)
#   tsAccsma <- xts(x = Acc$SMA, order.by=Acc$time, unique=T, frequency = freq)
#   tsAccv <-   xts(x = Acc$v, order.by=Acc$time, unique=T, frequency = freq)
  ## bind together
  tsAcc <- cbind(tsAccx, tsAccy, tsAccz, tsAccm, tsAccg,  deparse.level = 1)
  names(tsAcc) <- c("x", "y", "z", "m", "gforce")
  print("Timeseries compiled with x,y,z,m,gforce")
  name <- deparse(substitute(x)) 
  pdf(file =paste(name,".pdf"), width = 2.5 ,height = 3, pointsize = 7)
  plot(tsAcc[,3], ylim=c(0,2), major.ticks="minutes", mar = c(0,0,0,0), main="", ylab="z-acceleration (m/s/s)", xlab="time")
  legend(x = 'topleft', legend = c(paste(name)))
  dev.off()
  return(tsAcc)
}


### Create time series of the accelerometer data frame with SMA
timeseriessma <- function(x){
  # Load csv file
  Acc <- AccPre(x)
  #Calculate extra stats
  Acc <- statistics(Acc)
  ## Make time series Accelerometer data
  tsAccx <- SMA(xts(x = Acc$x, order.by=Acc$time, unique=T), n=10)
  tsAccy <- SMA(xts(x = Acc$y, order.by=Acc$time, unique=T), n=10)
  tsAccz <- SMA(xts(x = Acc$z, order.by=Acc$time, unique=T), n=10)
  tsAccm <- SMA(xts(x = Acc$m, order.by=Acc$time, unique=T), n=10)
  tsAccg <- SMA(xts(x = Acc$gforce, order.by=Acc$time, unique=T), n=10)
  tsAccsma <- SMA(xts(x = Acc$SMA, order.by=Acc$time, unique=T), n=10)
  tsAccv <- SMA(xts(x = Acc$v, order.by=Acc$time, unique=T), n=10)
  
  tsAcc <- cbind(tsAccx, tsAccy, tsAccz, tsAccm, tsAccg, tsAccsma, tsAccv)
  
  plot(tsAcc[,1], ylim=c(-2,2))
  lines(tsAcc[,2], col="red")
  lines(tsAcc[,3], col="blue")
  lines(tsAcc[,6], col="green")
  return(tsAcc)
}

### Safe the accelerometer output as a jpeg graph. With x, y, z and g acceleration
savegraph <- function(x){
  name <- deparse(substitute(x)) 
  pdf(file =paste(name,".pdf"), width = 5 ,height = 3, pointsize = 8)
  plot.zoo(x[,2], 
           type = "l",
           col = "grey55",
           xlab = "Time",
           lwd = 1,
           ylab = "Acceleration (m/s/s)",
           ylim = c(-2, 2),
           main = paste("Acceleration"),
           sub = paste("Setting:", name),
           mar = c(0,0,0,0),
           screens=1)
  lines(x[,3], col="grey35", lty=3)
  lines(x[,4], col="grey10", lty=1)
  legend(x = 'topleft', legend = c("X", "Y", "Z"),
         lty = c(1,3,1), lwd = 2, col=c("grey55","grey10",  "grey35" ))
  dev.off()
}



################################################################################
### Change points / break points
################################################################################

### Returns a list of the changepoints index number of the z-ax acceleration! 
GetChangePointsVar <- function(cpx){  # x = accelerometer dataframe with column indication
  plot(cpx)
  summary(cpx)
  breakpoints <- cpx@cpts 
  var <- cpx@param.est$variance
  r <- as.data.frame(cbind(breakpoints, var))
  return(r)
}

GetChangePointsMean <- function(cpx){  # x = accelerometer dataframe with column idication
  plot(cpx)
  summary(cpx)
  breakpoints <- cpx@cpts 
  mean <- cpx@param.est$mean
  r <- as.data.frame(cbind(breakpoints, mean))
  return(r)
}




################################################################################
### Location & Export
################################################################################

## Binds the changepoints to the Accelerometer data OUTPUT: as a dataframe
GetAccData <- function(x, y){  ## Accelerometer, Changepoints
  df <- NULL
  for(i in 1:length(y$breakpoints)){
    brp <- y$breakpoints[i]
    data <- x[brp,]
    newrow <- cbind(y[i,],data)
    df <- rbind(df, newrow )
    i <- i+1
  }
  return(df)
} 




### Create location for accelerometer file with the gps file. Match is on time. Works for breakpoints and normal data frames. Not spatial of timeseries!! Insert file name and workspace. 

createpoints <- function(x , y, date){    #(gps, accelerometer file name)
  df <- NULL
  x <- gpsTimeSerie(x)
  y <- AccPre(y, date)
  y <- statistics(y)
 head(y)
  ### for every accpoint, get 2 GPS points.One before, one after
  for(i in 1:length(y$z)){
    timeacc <- as.POSIXct(y$time[i])
    print(timeacc)
    s <- x[x$DateTime  < timeacc,] # dataframe with all values lower
    b <- x[x$DateTime  > timeacc,] # dataframe with all values larger
    l <- length(s$Id)
    start <- s[l,]  # first point 
    end  <-  b[1,]  # second point
    
    ## Get the time difference and the speed to calculate the distance for the new point
    timest <- as.POSIXct(start$DateTime)
    speed <- end$Speed           ## speed of second point. in (m/sec)
    dif  <- as.numeric(difftime(time1 = timeacc , time2 = timest , units = "secs"))
    Distance <- dif*speed        ## distance in m from first point to acc point 
    
    ## coordinates of both points
    x1 <- start$coords.x1
    x2 <- end$coords.x1
    y1 <- start$coords.x2
    y2 <- end$coords.x2
    
    ## Calculating the new point coordinates
    xc <- x1 + (Distance / sqrt( (y1-y2)^2 + (x1-x2)^2) ) * (x2-x1)  ## van Teije B 
    yc <- y1 + (Distance / sqrt( (y1-y2)^2 + (x1-x2)^2) ) * (y2-y1)  ## van Teije B
    
    print(xc)
    ## If no coordinates give default coordinates
    if(length(xc)!=0 || length(yc)!=0){
      GPSpoint <- cbind(xc, yc)  
    } else{ 
      xc <-  173273.3 
      yc <- 442337.3
      GPSpoint  <- cbind(xc,yc)
      }
    ## bind the coordinates and insert into new dataframe
    print(y[i,])
    print(GPSpoint)
    print(speed)
    newrow <- cbind(y[i,], GPSpoint, speed)
    df <- as.data.frame(rbind(df, newrow))
    
    ## loop
    i <- i+1
    print(i)
  }
  print("Dataframe created")
  return(df)
}


## Export a data frame with xc and yc to Shapefile
exportShp <- function(x) {
  name <- deparse(substitute(x))
  print(name)
  x <- x[complete.cases(x),]
  coordinates(x) <- ~xc+yc
  pr <- CRS("+proj=stere +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs" )
  projection(x) <- pr
  writeOGR(obj = x, dsn = "ChangePointDetection", layer = paste(name) , driver = "ESRI Shapefile", overwrite_layer = T )
}


#############################################
## Raster extraction
###########################################

extract_rast <- function(x){
#   ahnwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/i39fn1.tif")
#   slopewag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/slope_i39fn1.tif")
#   ruggedwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/rudged_i39fn1.tif")
  ahnwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/i39fz1.tif")
  slopewag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/slope_i39fz1.tif")
  ruggedwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/rudged_i39fz1.tif")

  location <- cbind(x$xc ,x$yc)
  height <- extract(x = ahnwag , y = location)
  slope <- extract(x = slopewag , y = location)
  #curvature <- extract(x = curvewag , y = location)
  rugged <- extract(x=ruggedwag, y=location)
  
  height[is.na(height)] <- mean(height, na.rm=T)
  slope[is.na(slope)] <- mean(slope, na.rm=T)
  rugged[is.na(rugged)] <- mean(rugged, na.rm=T)
  t <- cbind(x, height, slope, rugged) #
  print(head(t))
  return(t)
}
