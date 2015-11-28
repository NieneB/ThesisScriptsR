# This is the 4Surfaces.R script for analysing various accelerometer files of different surfaces measured
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 20-10-2015

# Installing and loading packages
source("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/RScripts/1Packages.R")

# Functions
source("D:/Niene_The_one_and_only/06_Thesis/02_MeetRollator/RScripts/2Functions.R")

# Setting the working directory
setwd("~/Documents/00_Msc_Thesis/02_MeetRollator/3-11-Surface")

date <- "2015-11-03"

### Tarmac
termac_old_1 <- timeseries("1 oudasfalt.csv", date)
termac_new_1 <- timeseries("1nieuwasfalt.csv", date)
termac_new_2 <- timeseries("2 nieuwasfalt.csv", date)

newtermac <- rbind(termac_new_1, termac_new_2)
newtermac <- descriptiveStats(newtermac)
oldtermac <- descriptiveStats(termac_old_1)


GetChangePoints(termac) ## see if nescerarily?... .. . . 

### Paving
brick1 <- timeseries("1 brick.csv", date)
brick2 <- timeseries("2 brick.csv", date)
brick3 <- timeseries("3 brick.csv", date)

brick <- rbind(brick1, brick2, brick3)
brick <- descriptiveStats(brick)


### grass
grass <- timeseries("gras.csv", date)
grass <- descriptiveStats(grass)

### tiles
tiles1 <- timeseries("1 tiles.csv", date)
tiles2 <- timeseries("2tiles.csv", date)
bigtiles <- timeseries("bigtiles.csv", date)

tiles <- rbind(tiles1,tiles2)
tiles <- descriptiveStats(tiles)
bigtiles <- descriptiveStats(bigtiles)

### klinkers
stones <-  timeseries("klinkertjes newbold.csv", date)
stones <- descriptiveStats(stones)

### gravel
gravel <- timeseries("grind.csv", date)
gravel <- descriptiveStats(gravel)

## smooth
smooth <- timeseries("betonbinnen.csv", date)
smooth <- descriptiveStats(smooth)


##################
## all stats together 
names <- c("0Smooth", "1newtermac" ,"2oldtermac","3bigtiles", "4tiles", '5brick', "6stones", "7gravel", "8grass" )
all <- rbind(smooth, newtermac, oldtermac,bigtiles, tiles, brick, stones, gravel, grass, deparse.level = 0)
row.names(all) <- names
all <- as.data.frame(all)

### Plot all data in boxplots SOOOO NICE :D 
names2 <- c("Smooth", "New termac" ,"Old termac","Big tiles", "Tiles", 'Brick', "Stones", "Gravel", "Grass" )
ggplot(all,aes(x=as.factor(row.names(all))))+
  geom_boxplot(aes(lower = q1, upper = q3 , middle = med, ymin = min, ymax = max), stat="identity")+
  scale_x_discrete(labels= names2)+ coord_flip()+
  ylab(label ="z acceleration variance")+
  xlab(label = "Surface")+
  labs(title = "Boxplots of variance per surface")

##### Total population and samples
totpop <- rbind(smooth, newtermac,termac_old_1,bigtiles, tiles, brick, stones, gravel, grass)
tot <- descriptiveStats(totpop)
