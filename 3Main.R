# This is the 3Main.R script for analysing various accelerometer files
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 20-10-2015

# Installing and loading packages
source("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/RScripts/1Packages.R")

# Functions
source("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/RScripts/2Functions2.R")

# Setting the working directory
setwd("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/AppTests")
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

meetrollator <- timeseries("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/2_9Sept_MeetRollator/meetrollator.csv")

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






