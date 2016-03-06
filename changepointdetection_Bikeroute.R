# This is the changepoint detection script for finding the location of the change point in the accelerometer, speed and slope data of the bike route on 26th of october 2015.
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 05-02-2016
# Change point detection and location assigning

setwd("~/Documents/00_Msc_Thesis/02_MeetRollator/3_BikeRoute")


### Functions different from functions package. 

### Load GPS file and change time to CET tz OUTPUT: Dataframe of GPS with coordinates in RDnew
gpsTimeSerie <- function(x){
  GPS <- readOGR(dsn = paste(x, '.shp', sep='' ), layer = x)
  dfGPS <- as.data.frame(GPS)
  dfGPS$DateTime <- as.POSIXct(x = strptime(dfGPS$DateTime, format= "%Y-%m-%d %H:%M:%S"), tz="GMT")
  # attributes(dfGPS$DateTime)$tzone <- "CET"
  print("GPS file loaded as data frame")
  return(dfGPS)
}


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

### Load data bike route:
bike_route26_10 <- createpoints("bike_route_aangepast", "bike_normal.csv", "2015-10-26" )
plot(bike_route26_10$xc, bike_route26_10$yc)
### drop all values after no location is specified. 
bike_route26_10 <- bike_route26_10[!(is.na(bike_route26_10$xc)) | !(is.na(bike_route26_10$yc)) ,]

## Extract all values form the rasters
bike_route26_10_plus <- extract_rast(bike_route26_10)

### Detecting changepoint for the route. 
varz <-  cpt.var(bike_route26_10_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
varm <-  cpt.var(bike_route26_10_plus$m, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
speed <- cpt.mean(bike_route26_10_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
height <- cpt.mean(bike_route26_10_plus$height, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
slope <- cpt.mean(bike_route26_10_plus$slope, method = "PELT")

summary(slope)
summary(speed)
summary(varz)


### Plots
#### Final plots for changepoint comparison
pdf(file =paste("ChangePointComparisonbike.pdf"), width = 5 ,height = 7, pointsize = 10)
par(mfrow=c(5,1), pin=c(3,3), mar=c(3,4,0,1))
plot(varz, ylab="z-ax acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("Az", "Average Variance"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(varm, ylab="total acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("Am", "Average Variance"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(speed, ylab="Speed (m/s)", xlab="Time index")
legend(legend = c("Speed", "Mean speed"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(height, ylab="Height (m above NAP)", xlab ="Time index")
legend(legend = c("Height", "Mean height"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(slope, ylab="slope", xlab ="Time index")
legend(legend = c("Slope", "Mean slope"), x = "topleft", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
dev.off()

## Get all possible changepoints

## Bind changepoints to location
CPvarz <- GetChangePointsVar(varz)
CPvarz <- GetAccData(bike_route26_10_plus , CPvarz)


CPvarm <- GetChangePointsVar(varm)
CPvarm <- GetAccData(bike_route26_10_plus , CPvarm)

CPmeanSpeed <-  GetChangePointsMean(speed)
CPmeanSpeed <- GetAccData(bike_route26_10_plus , CPmeanSpeed)


CPmeanheight <- GetChangePointsMean(height)
CPmeanheight <- GetAccData(bike_route26_10_plus , CPmeanheight)

CPmeanSlope <- GetChangePointsMean(slope)
CPmeanSlope <- GetAccData(bike_route26_10_plus , CPmeanSlope)


## Export all to shapefiles
exportShp(bike_route26_10_plus)
exportShp(CPvarz)
exportShp(CPvarm)
exportShp(CPmeanSpeed)
exportShp(CPmeanheight)
exportShp(CPmeanSlope)

