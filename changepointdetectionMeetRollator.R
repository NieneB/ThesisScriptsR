# This is the changepoint detection script for finding the location of the change point in the accelerometer, speed and slope data of the meetrollator walk on 3th of november 2015.
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 05-02-2016
# Change point detection and location assigning

setwd("~/Documents/00_Msc_Thesis/02_MeetRollator/2_MeetRollatorRoute")

### Functions different from functions package. 

### Load GPS file and change time to CET tz OUTPUT: Dataframe of GPS with coordinates in RDnew
gpsTimeSerie <- function(x){
  GPS <- readOGR(dsn = paste(x, '.shp', sep='' ), layer = x)
  dfGPS <- as.data.frame(GPS)
  dfGPS$DateTime <- as.POSIXct(x = strptime(dfGPS$time, format= "%Y-%m-%d %H:%M:%S"), tz="CET")
  # attributes(dfGPS$DateTime)$tzone <- "CET"
  print("GPS file loaded as data frame")
  return(dfGPS)
}


createpoints <- function(x , y, date){    #(gps, accelerometer file name)
  df <- NULL
  x <- gpsTimeSerie(x)
  y <- AccPre(y, date)
  y <- statistics(y)
  
  ### for every accpoint, get 2 GPS points.One before, one after
  for(i in 1:length(y$z)){
    timeacc <- as.POSIXct(y$time[i])
    print(timeacc)
    s <- x[x$DateTime  < timeacc,] # dataframe with all values lower
    b <- x[x$DateTime  > timeacc,] # dataframe with all values larger
    l <- length(s$x)
    start <- s[l,]  # first point 
    end  <-  b[1,]  # second point
    
    ## Get the time difference and the speed to calculate the distance for the new point
    timest <- as.POSIXct(start$DateTime)
    speed <- end$speed           ## speed of second point. in (m/sec)
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

extract_rast <- function(x){
    ahnwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/i39fn1.tif")
    slopewag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/slope_i39fn1.tif")
    ruggedwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/rudged_i39fn1.tif")
#   ahnwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/i39fz1.tif")
#   slopewag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/slope_i39fz1.tif")
#   ruggedwag <- raster(x ="~/Documents/00_Msc_Thesis/02_GISanlayse/AHN2/Orgineel/Wageningen/rudged_i39fz1.tif")
  
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




########################################################################################
### Load data meetrollator test:
meetrollator03_11 <- createpoints("meetrollator_03_11","route.csv", "2015-11-3")
plot(meetrollator03_11$xc, meetrollator03_11$yc)

### drop all values after no location is specified. 
meetrollator03_11 <- meetrollator03_11[!(is.na(meetrollator03_11$xc)) | !(is.na(meetrollator03_11$yc)),]

## Extract all values form the rasters
meetrollator03_11_plus <- extract_rast(meetrollator03_11)

## Get changepoints

### Detecting changepoint for the route. 
varz <-  cpt.var(meetrollator03_11_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
varm <-  cpt.var(meetrollator03_11_plus$m, method = "PELT")
speed <- cpt.mean(meetrollator03_11_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
height <- cpt.mean(meetrollator03_11_plus$height, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
slope <- cpt.mean(meetrollator03_11_plus$slope, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")

summary(slope)
summary(speed)
summary(varz)

####  plots for changepoint comparison
pdf(file =paste("ChangePointComparison.pdf"), width = 5 ,height = 7, pointsize = 10)
par(mfrow=c(5,1), pin=c(3,3), mar=c(3,4,0,1))

plot(varz, ylab="z-ax acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("Az", "Average Variance"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)


plot(varm, ylab="Total acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("Am", "Average Variance"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(speed, ylab="Speed (m/s)", xlab="Time index")
legend(legend = c("Speed", "Mean speed"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(height, ylab="Height (m above NAP)", xlab ="Time index")
legend(legend = c("Height", "Mean height"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(slope, ylab="Slope (%)", xlab ="Time index")
legend(legend = c("Slope", "Mean slope"), x = "topright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

dev.off()

## Bind changepoints to location
CPvarz <- GetChangePointsVar(varz)
CPvarz <- GetAccData(meetrollator03_11_plus , CPvarz)


CPvarm <- GetChangePointsVar(varm)
CPvarm <- GetAccData(meetrollator03_11_plus , CPvarm)

CPmeanSpeed <-  GetChangePointsMean(speed)
CPmeanSpeed <- GetAccData(meetrollator03_11_plus , CPmeanSpeed)


CPmeanheight <- GetChangePointsMean(height)
CPmeanheight <- GetAccData(meetrollator03_11_plus , CPmeanheight)

CPmeanSlope <- GetChangePointsMean(slope)
CPmeanSlope <- GetAccData(meetrollator03_11_plus , CPmeanSlope)

### Export all to shapefiles
exportShp(meetrollator03_11_plus)
exportShp(CPvarz)
exportShp(CPvarm)
exportShp(CPmeanSpeed)
exportShp(CPmeanheight)
exportShp(CPmeanSlope)






