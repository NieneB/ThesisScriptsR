# This is the changepoint detection script for finding the location of the change point in the accelerometer data.
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 12-11-2015
# Change point detection and location


## Load data walk with ms Schaap.
schaap <- createpoints("sept_schaap_clean","1_3Sept_MvSchaap/sept3s.csv", "2015-09-03")
exportShp(schaap)
head(schaap)


### Load data meetrollator test:
setwd("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator")
meetrollator03_11 <- createpoints("route_03_11_Meetrollator","3-11-surface/route.csv", "2015-11-3")
### drop all values after no location is specified. 
meetrollator03_11 <- meetrollator03_11[!(is.na(meetrollator03_11$xc)) | !(is.na(meetrollator03_11$yc)),]
## Extract all values form the rasters
meetrollator03_11_plus <- extract_rast(meetrollator03_11)

plot(meetrollator03_11_plus$height)
## Get all possible changepoints
CPmeanSpeed <- CPspeedmean(meetrollator03_11_plus)
CPvarSpeed <- CPspeedvar(meetrollator03_11_plus)
CPvarz <- CPzvar(meetrollator03_11_plus)
CPvarm <- CPzmean(meetrollator03_11_plus)
CPmeanheight <- CPheight(meetrollator03_11_plus)
CPmeanSlope <- CPslope(meetrollator03_11_plus)
CPmeanCurvature <- CPcurve(meetrollator03_11_plus)

mean(meetrollator03_11_plus$height, na.rm=T )
exportShp(CPmeanSpeed)
exportShp(CPvarSpeed)
exportShp(CPvarz)
exportShp(CPvarm)
exportShp(CPmeanheight)
exportShp(CPmeanSlope)
exportShp(CPmeanCurvature)
exportShp(meetrollator03_11_plus)
pdf(file = "CPmeanSpeed.pdf", width = 5 ,height = 3)
plot(meetrollator03_11_plus$height)
dev.off()
savegraph(meetrollator03_11_plus)


### Load data bike route:
setwd("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator")
bike_route26_10var <- createpoints("bike_rd", "AppTests/bike_normal.csv","2015-10-26" )
exportShp(bike_route26_10var)
bike_route26_10_breakpointsvar <- GetChangePoints(bike_route26_10var)
exportShp(bike_route26_10_breakpointsvar)
g <- GetChangeSpeed("bike_rd")

head(bike_route26_10_breakpointsvar)
head(bike_route26_10var)

library(scatterplot3d)
install.packages("scatterplot3d")
### Make 3D scatter plot
head(bike_route26_10var)
data <- as.data.frame(bike_route26_10var)
scatterplot3d()
scatterplot3d(x = data$xc, y = data$yc, z = data$z, 
          type="l", 

          main="bike ride acceleration in z-ax",
          clab="speed m/s", d=2,
          xlab = "x RDnew", ylab = "y RDnew",
          zlab = "Acceleration in z-ax (m/s/s)"
          )


, theta = 0 ,phi = 60, 
          bty = "g", pch = 20, cex = 1, alpha=0.5,
          
