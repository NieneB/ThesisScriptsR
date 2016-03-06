# This is the changepoint detection script for testing several changepoint methods 
# MSc Thesis GIS
# author: Niene Boeijen
# reg.nr. 900918-088-070
# date: 12-11-2015
# Change point detection and location

### Load data meetrollator test:
## See other script

setwd('~/Documents/00_Msc_Thesis/02_MeetRollator/1_MethodsComparison')

### Comparison differnt method possibilities for speed data
speed.PELT <- cpt.mean(meetrollator03_11_plus$speed, method = "PELT")
speed.PELTm <- cpt.mean(meetrollator03_11_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
speed.SN <- cpt.mean(meetrollator03_11_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "SegNeigh") # computation time toooo hight!!!
speed.BSm <- cpt.mean(meetrollator03_11_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "BinSeg") # not working.. don't know why
speed.BS <- cpt.mean(meetrollator03_11_plus$speed, method = "BinSeg") 
speed.varPELT <- cpt.var(meetrollator03_11_plus$speed, method = "PELT")
speed.varPELTm <- cpt.var(meetrollator03_11_plus$speed, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
speed.mv <- cpt.meanvar(meetrollator03_11_plus$speed,method = "PELT")

### Make the pdf file
pdf(file =paste("comparisonMethodsSpeed.pdf"), width = 5 ,height = 6, pointsize = 10)
par(mfrow=c(4,2), pin=c(3,3), mar=c(3,4,0,1))

plot(speed.PELT, ylab="Speed (m/s)", xlab ="Time index" )
legend(legend = c("Speed", "Mean PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(speed.PELTm, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Mean PELT 1.5 * log(n)"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(speed.BS, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Mean BinSeg"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(speed.BSm, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Mean BinSeg 1.5 * log(n)"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(speed.SN, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Mean SegNeigh"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(speed.mv, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Mean Var PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(speed.varPELT, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Var PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(speed.varPELTm, ylab="Speed (m/s)", xlab ="Time index")
legend(legend = c("Speed", "Var PELT 1.5 * log(n)"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
dev.off()

#################
### Comparison z-ax data
z.varPELT <- cpt.var(meetrollator03_11_plus$z, method = "PELT")
z.varPELTm <- cpt.var(meetrollator03_11_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
z.BSm <- cpt.var(meetrollator03_11_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "BinSeg") # not working.. don't know why
z.BS <- cpt.var(meetrollator03_11_plus$z, method = "BinSeg") 

z.PELT <- cpt.mean(meetrollator03_11_plus$z, method = "PELT")
z.PELTm <- cpt.mean(meetrollator03_11_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")

z.SN <- cpt.mean(meetrollator03_11_plus$z, penalty = "Manual", pen.value = "1.5 * log(n)", method = "SegNeigh") # computation time toooo hight!!!
z.mv <- cpt.meanvar(meetrollator03_11_plus$z,method = "PELT")

### Make the pdf file
pdf(file =paste("comparisonMethodsZax2.pdf"), width = 5 ,height = 6, pointsize = 10)
par(mfrow=c(4,2), pin=c(3,3), mar=c(3,4,0,1))

plot(z.varPELT, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Var PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(z.varPELTm, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Var PELT 1.5 * log(n)"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(z.BS, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Var BinSeg"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(z.BSm, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Var BinSeg 1.5 * log(n)"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(z.PELT, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Mean PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(z.PELTm, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Mean 1.5 * log(n) PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

plot(z.SN, ylab="Acceleration (m/s/s)", xlab ="Time index")
legend(legend = c("z", "Mean SegNeigh"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)
plot(z.mv, ylab="Acceleration (m/s/s))", xlab ="Time index")
legend(legend = c("z", "MeanVar PELT"), x = "bottomright", lty = c(1,1), col = c("black", "red"), bg = "white", cex = 0.8)

dev.off()



