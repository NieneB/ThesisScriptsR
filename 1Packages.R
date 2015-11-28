# Thesis Methodology Accelerometer files
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 30-10-2015

# This is 1Packages.R script
# For installing packages and loading libraries

### Packages needed
packages <- c("zoo", "xtsExtra", "forecast" , "changepoint", "rgdal", "ggplot2", "raster")

## Function for checking if downloaded
pak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
}

pak(packages)


### Remaining packages
# install.packages('plot3D')
# install.packages("corrgram")
# install.packages("TTR")




### Loading packages
# Crucial:
library(base)
library(sp)
library(rgdal)

# Timeseries:

library(zoo)
library(xts)
library(changepoint)

# Misc:
# library(forecast)
# library(corrgram)
# library(TTR)

# Graphic design:
library(RColorBrewer)
library(plot3D)
library(ggplot2)

