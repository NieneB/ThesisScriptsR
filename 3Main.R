# This is the 3Main.R script for analysing various accelerometer files
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 20-10-2015

setwd("~/Documents/github/ThesisScriptsR/")
# Installing and loading packages
source("1Packages.R")

# Functions
source("2Functions2.R")

# Setting the working directory
setwd("~/Documents/00_Msc_Thesis")
date <- ("2015-11-18")
# Getting plot and statistics per file 
stillGame <- timeseries("still_game.csv" ,date)
walkGame <- timeseries("walk_game.csv",date)
walknormal <- timeseries("walk_normal.csv",date)
stillnormal <- timeseries("still_normal.csv",date)
walkui <- timeseries("walk_UI.csv",date)
stillui <- timeseries("still_UI.csv",date)
stillfast <- timeseries("still_fast.csv",date)
walkfast <- timeseries("walk_fast.csv",date)
shakingui <- timeseries("shaking_UI.csv")
shakinguiold <- timeseries("shaking_UI_oldversion.csv")
schaap <- timeseries("schaap.csv")

meetrollator <- timeseries("/02_MeetRollator/2_9Sept_MeetRollator/meetrollator.csv")

test1 <- timeseries("2septactionthuisfiets.csv")
test2 <- timeseries("2septhuisactionfiets.csv") 

bike <- timeseries("bike_normal.csv")

stil4 <-  timeseries("stillnormaallangsam4.csv" ,date) 
stil3 <-  timeseries("stillnormalsam3.csv",date) 
stil5 <-  timeseries("stillnormalsam5.csv",date) 

stil4 <- descriptiveStats(stil4)
stil3 <- descriptiveStats(stil3)
stil5 <- descriptiveStats(stil5)



movements <- timeseries("movements.csv")
movements <- read.csv("movements.csv", sep=";")

plot.zoo(movements)

# safe graphs as jpeg
savegraph(stillGame)
savegraph(walkGame)
savegraph(stillnormal)
savegraph(walkfast)
savegraph(walknormal)
savegraph(walkui)






