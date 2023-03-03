## KI BLEACHING FIGURE #2 - TEMPERATURE STRESS
# Load necessary packages
library(imager)
library(here)

# Load necessary data
load("data/KI_SB_temp_wKim_1d_mean.RData")
load("data/NOAA_5km_bleachingbysite_mean.RData")
load("data/NOAA_5km_sst_bleachingbysite_mean.RData")


# Set a start and end date for plotting
# Set up and format data
# Set a start and end date for plotting
startdate <- as.POSIXct("2013-07-01 00:00:00",
                        tz="Pacific/Kiritimati", 
                        format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2017-12-31 00:00:00",
                      tz="Pacific/Kiritimati", 
                      format="%Y-%m-%d %H:%M:%S")

### Stopped here
# Truncate the data from startdate to enddate
KI_heat <- dhw_bleachingsites_mean[which(dhw_bleachingsites_mean$date>startdate),]
KI_heat <- KI_heat[which(KI_heat$date<enddate),]
KI_insitu <- insitu_mean[which(insitu_mean$xi3>startdate),]
KI_insitu <- KI_insitu[which(KI_insitu$xi3<enddate),]
KI_sat <- sst_bleachingsites_mean[which(sst_bleachingsites_mean$date>startdate),]
KI_sat <- KI_sat[which(KI_sat$date<enddate),]
# Rename columns
colnames(KI_heat)<- c("time","dhw")

cbar <- read.csv("data/cmap_enso.csv",header=F)
dhw.floor <- floor(KI_heat$dhw)+1
dhw.cc <- cbar[dhw.floor,]
dhw.cc.rgb <- rgb(dhw.cc)
from <- KI_heat$time-1.75*86400
to <- KI_heat$time+1.75*86400

#################################################################################
# polycurve function
# http://www.fromthebottomoftheheap.net/2013/01/11/shading-regions-under-a-curve/
polyCurve <- function(x, y, from, to, n = 50, miny,
                      col = "red", border = col) {
    drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
        Sq <- seq(from = from, to = to, length = n)
        polygon(x = c(Sq[1], Sq, Sq[n]),
                y = c(miny, fun(Sq), miny),
                col = col, border = border)
    }
    lf <- length(from)
    stopifnot(identical(lf, length(to)))
    if(length(col) != lf)
        col <- rep(col, length.out = lf)
    if(length(border) != lf)
        border <- rep(border, length.out = lf)
    if(missing(miny))
        miny <- min(y)
    interp <- approxfun(x = x, y = y)
    mapply(drawPoly, from = from, to = to, col = col, border = border,
           MoreArgs = list(fun = interp, n = n, miny = miny))
    invisible()
}
#################################################################################

# Colours for shading
cols <- c(rgb(cbar)[1], rgb(cbar)[4], rgb(cbar)[8], rgb(cbar)[12], rgb(cbar)[24])

# Need to add 2013 field season
KI2013 <- as.POSIXct("2013-07-31 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015d <- as.POSIXct("2015-11-06 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2017 <- as.POSIXct("2017-07-15 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

############# Make pdf ################
pdf(file="figures/KI_figure2.pdf",width = 8, height = 5,useDingbats = FALSE)

# Set inner and outer margins
par(oma=c(0,0,0,0),mar=c(1.5,3.2,1,2.5),mfrow=c(2,1))
layout(matrix(c(1,2),nrow=2),heights=c(1,1.5))

# Plot with the polycurve function
with(KI_heat, plot(KI_heat$time,KI_heat$dhw, type="l", 
                   xlab="", ylab="", ylim=c(0,26),cex.axis=1,xaxt='n',yaxt='n',
                   cex.lab=1.2,yaxs="i",xaxs="i",lwd=0.5, col="gray40",
                   panel.first = # Panel first allows ablines to be plotted before polycurve, looks nicer.
                     c(abline(v=KI2013,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2014,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2015a,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2015b,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2015c,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2015d,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2016a,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2016b,col="darkgray",
                              lwd=2,lty=2),
                       abline(v=KI2017,col="darkgray",
                              lwd=2,lty=2),
                       polyCurve(KI_heat$time, 
                                 KI_heat$dhw, from = from, 
                                 to = to, miny = 0, 
                                 col = dhw.cc.rgb)
                     )))
Y <- c(0,5,10,15,20,25)
axis(side=2,at=Y,cex.axis=0.93,tck=0.02, 
     lwd.ticks=1.5, las=2,hadj=0)

par(new=T) # To add the temperature data to the same plot
plot(KI_sat,type='l',ylim=c(25,31.5),
     xlim=c(startdate,enddate),
     xlab="",ylab="",xaxs="i",xaxt='n',yaxt="n") # Plot the satellite temperature data
par(new=T) # To add the temperature data to the same plot
plot(KI_insitu,type='l',
     ylim=c(25,31.5),xlim=c(startdate,enddate),
     xlab="",ylab="",xaxs="i",
     xaxt='n',yaxt="n",col="blue") # Plot the in situ temperature data
abline(29,0,col=cols[5],lwd=2) # Bleaching threshold
abline(28,0,col="black") # Mean Monthly Maximum - MMM

title(ylab="Degree Heating Weeks", line=1.2, cex.lab=1) # Label y axis
# Add in time axes (multiple axes added to allow for customization)
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,tck=0.05,
             lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,
             at=seq(KI_heat$time[1],KI_heat$time[1644],
                    by="month"),labels=FALSE,
             tck=0.03,cex.axis=0.93,lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,
             tck=0,padj=-1.5)
Z <- c(26,27,28,29,30,31) # To be used as temperature y-axis values
axis(side=4,at=Z,cex.axis=0.93,tck=0.02, 
     lwd.ticks=1.5, las=2,hadj=0.95)
mtext("A",side=2, line=2.5,cex=1.2,las=2,
      padj=-5.5,font=2) # Add label for figure, specify size
mtext("B",side=2, line=2.5,cex=1.2,las=2,
      padj=8.3, font=2) # Add label for figure, specify size
mtext("i",side=2, line=-0.5, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("ii",side=2, line=-8.8, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("iii",side=2, line=-11.7, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("iv",side=2, line=-14.0, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("v",side=2, line=-15.6, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("vi",side=2, line=-17.8, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("vii",side=2, line=-20.8, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("viii",side=2, line=-25.4, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("ix",side=2, line=-30.7, cex=1, las=2,
      padj=-5) # Add sampling date label
mtext("Temperature (°C)",side=4, cex=1,line=1.1)
mtext("Mean Monthly Max.",side=2,line=-47.7,
      cex=0.72,las=2,padj=0.2) # Label MMM line
mtext("Bleaching Threshold",side=2,line=-47.7,
      cex=0.72,las=2,padj=-2.5) # Label bleaching threshold line


par(mar=c(0.5,0,0.05,0.75))
heatmap <- load.image('figures/ElNino_heatmap-4-1-01.jpg')
heatmap.crop <- autocrop(heatmap)
plot(heatmap.crop, axes=F)

# Close file and clear graphical parameters
dev.off()

