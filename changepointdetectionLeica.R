# This is the changepoint detection script for finding the location of the change point in the speed and slope data of the Leica meetrollator walk on september the 3th.
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 05-02-2016
# Change point detection and location assigning

setwd("~/Documents/00_Msc_Thesis/02_MeetRollator/Leica_slope_speed")



### Functions different from functions package. 

### Creating 4 points in between every set of GPS points. To increase the detail in Slope extraction.
createpoints <- function(x){    #(gps)
  df <- NULL
  ### for every point, get 2 GPS points.One before, one after
  for(i in 1:length(x$Speed)){
    print(i)
    k <- i+1
    start <- x[i,]  # first point 
    end  <-  x[k,]  # second point
    
    ## Get the distance difference between 2 points
    Dist <- x[k,]$Distance/5      ## distance in m from first point to acc point 
    
    j <- 1 
    
    for(j in 1:4){
      print(j)
      Distance <- Dist*j
      print(Distance)
      ## coordinates of both points
      x1 <- start$X
      x2 <- end$X
      y1 <- start$Y
      y2 <- end$Y
      
      ## Calculating the new point coordinates
      X <- x1 + (Distance / sqrt( (y1-y2)^2 + (x1-x2)^2) ) * (x2-x1)  ## van Teije B 
      Y <- y1 + (Distance / sqrt( (y1-y2)^2 + (x1-x2)^2) ) * (y2-y1)  ## van Teije B
      
      ## If no coordinates give default coordinates
      if(length(X)!=0 || length(Y)!=0){
        GPSpoint <- cbind(X, Y)  
      } else{ 
        X <- 173273.3 
        Y <- 442337.3
        GPSpoint  <- cbind(X,Y)
      }
      ## bind the coordinates and insert into new dataframe
      olddata <- start[,3:15]
      newrow <- cbind(GPSpoint, olddata)
      df <- as.data.frame(rbind(df, newrow))
      
      ## loop
      j <- j+1
    }
    df <- as.data.frame(rbind(df, x[i,]))
    ## loop
    i <- i+1
    
  }
  print("Dataframe created")
  return(df)
}


### Change point and binding of slope ts
CPslope <-  function(x){
  plot(x$slope)
  cpx <- cpt.mean(x$slope, method = "PELT")
  cpx2 <- GetChangePointsMean(cpx)
  rr <- GetAccData(x,cpx2)
  return(rr)
}


### Change point and binding of speed ts
CPspeed <-  function(x){
  cpx <- cpt.mean(x$Speed, method = 'PELT', penalty = "AIC") 
  cpx2 <- GetChangePointsMean(cpx)
  rr <- GetAccData(x,cpx2)
  return(rr)
}

### Because of overfitting the model of speed a penatlyt type of BIC AIC or Hanan-Quinn can be chosen to solve the problem of overfitting.
##  a penalty term for the number of parameters in the model; the penalty term is larger in BIC than in AIC.
##he penalty discourages overfitting (increasing the number of parameters in the model almost always improves the goodness of the fit).
## we use an adjusted penalty to overcome the problem of overfitting. The GPS contains abonormal peaks due to measurement errors. If a model is generated that fits too well, these are taken as truth. Using a penalty these extreme measurments are not taken into the analysis. These are plausibly artifacts of the data rather
## than true changes in the underlying process. In an effort to remove these seemingly spurious changepoints we can increase the penalty to 1.5 * log(n)
## The result seem more plausible. 

## Export the shape files with right projection and x y denotation
exportShp <- function(x) {
  name <- deparse(substitute(x))
  print(name)
  x <- x[complete.cases(x),]
  coordinates(x) <- ~X+Y
  pr <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel" )
  projection(x) <- pr
  writeOGR(obj = x, dsn = "ChangePointDetection", layer = paste(name) , driver = "ESRI Shapefile", overwrite_layer = T )
}


#########################################################################33

## Load data leica route with speed
leica <- read.csv(file = 'Leica_route/leica.csv', header = T)
head(leica)
plot(leica$X, leica$Y)
leica <- as.data.frame(leica)
length(leica$X)
leica$Distance <- leica$Distance * 0.0000001 #meter
leica$Speed <- leica$Speed * 0.00001 #m/s


## create extra points in between with same speed
### Create location for accelerometer file with the gps file. Match is on time. Works for breakpoints and normal data frames. Not spatial of timeseries!! Insert file name and workspace. 
leica1 <- createpoints(leica)
plot(leica1$X, leica1$Y)
plot(leica1$Course)
length(leica1$X)

## Extract slope values form the rasters
slope <- raster(x ="AHN_slope/slope_i25dn2.tif")
location <- leica1[,1:2]
length(location$X)

slope2 <- extract(x = slope , y = location)
plot(slope2)
length(slope2)
slope2[is.na(slope2)] <- mean(slope2, na.rm=T)
leica2 <- cbind(leica1, slope2)
head(leica2)
plot(leica2$Speed)
length(leica2$X)


#### change cp package debug
trace(cpt.mean, edit=T)
trace(cpt.meanvar, edit=T)
trace(cpt.var, edit=T)


## Get changepoints
slopeCP <- CPslope(leica2)
speedCPaic <- CPspeed(leica2)
speedCPpelt <- CPspeed(leica2)
speedCPpeltlog <- CPspeed(leica2)

## Export as shapefile
exportShp(leica2)
exportShp(slopeCP)
exportShp(speedCPaic)
  
### Plotting graphs and export to pdf
speedplot <- cpt.mean(leica2$Speed, method = 'PELT', penalty = "AIC") 
slopeplot <- cpt.mean(leica2$slope, method = "PELT")

pdf(file =paste("comparisonMethodsLeica.pdf"), width = 5 ,height = 6, pointsize = 10)
par(mfrow=c(2,1), pin=c(3,3), mar=c(4,4,1,1))

plot(x = speedplot, ylab="Speed (m/s)", xlab ="Time index", type='l')
legend(legend = c("Speed", "Mean PELT, AIC penalty"), x = "topleft", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(x = slopeplot, ylab="Slope (%)", xlab ="Time index")
legend(legend = c("Slope", "Mean PELT"), x = "topleft", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

dev.off()
