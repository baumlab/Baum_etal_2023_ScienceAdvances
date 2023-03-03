library(ggplot2)
library(gridExtra)

load("data/NOAA_5km_bleachingbysite_mean_historical.RData")

firstglobalbleaching_start <- as.POSIXct("1997/4/1 00:00:00",
                                   format="%Y/%m/%d %H:%M:%S",
                                   tz="Pacific/Kiritimati")

firstglobalbleaching_end <- as.POSIXct("1998/4/30 00:00:00",
                                         format="%Y/%m/%d %H:%M:%S",
                                         tz="Pacific/Kiritimati")

secondglobalbleaching_start <- as.POSIXct("2009/6/1 00:00:00",
                                         format="%Y/%m/%d %H:%M:%S",
                                         tz="Pacific/Kiritimati")

secondglobalbleaching_end <- as.POSIXct("2010/4/30 00:00:00",
                                       format="%Y/%m/%d %H:%M:%S",
                                       tz="Pacific/Kiritimati")

thirdglobalbleaching_start <- as.POSIXct("2014/1/1 00:00:00",
                                         format="%Y/%m/%d %H:%M:%S",
                                         tz="Pacific/Kiritimati")

thirdglobalbleaching_end <- as.POSIXct("2017/12/31 00:00:00",
                                       format="%Y/%m/%d %H:%M:%S",
                                       tz="Pacific/Kiritimati")


historical_dhw <- ggplot(data=dhw_bleachingsites_mean)+ 
  theme_classic()+
  annotate("rect", xmin = firstglobalbleaching_start, 
           xmax = firstglobalbleaching_end, 
           ymin = 0, ymax = Inf,
           alpha = .5)+
  annotate("rect", xmin = secondglobalbleaching_start, 
           xmax = secondglobalbleaching_end, 
           ymin = 0, ymax = Inf,
           alpha = .5)+
  annotate("rect", xmin = thirdglobalbleaching_start, 
           xmax = thirdglobalbleaching_end, 
           ymin = 0, ymax = Inf,
           alpha = .5)+
  geom_line(aes(x=date,y=mean_dhw),color="black",size=1.5)+
  geom_hline(yintercept = 4,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 8,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 12,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 16,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 20,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 24,linetype="dashed",color="darkgray")+
  geom_hline(yintercept = 28,linetype="dashed",color="darkgray")+
  scale_x_datetime(name="Year", expand=c(0,0), 
                   date_labels = "%Y",date_breaks = "1 year")+
  scale_y_continuous(name="Degree heating weeks (°C-weeks)",
                     limits=c(0,29),expand=c(0,0), 
                     breaks = c(0,4,8,12,16,20,24,28))
historical_dhw

pdf(file = "figures/KI_historical_dhw_FigS6.pdf", width = 12, height = 4, useDingbats = FALSE)
historical_dhw
dev.off()

jpeg(filename = "figures/KI_historical_dhw_FigS6.jpg", width = 12, height = 4, units="in", res=300)
historical_dhw
dev.off()

###############
start_8788_EN <- as.POSIXct("1987/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_8788_EN <- as.POSIXct("1988/12/31 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
EN_8788 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_8788_EN,]
EN_8788 <- EN_8788[EN_8788$date<end_8788_EN,]
max(EN_8788$dhw)

start_9293_EN <- as.POSIXct("1992/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_9293_EN <- as.POSIXct("1993/12/31 00:00:00",
                          format="%Y/%m/%d %H:%M:%S",
                          tz="Pacific/Kiritimati")
EN_9293 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_9293_EN,]
EN_9293 <- EN_9293[EN_9293$date<end_9293_EN,]
max(EN_9293$dhw)

start_9798_EN <- as.POSIXct("1997/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_9798_EN <- as.POSIXct("1998/12/31 00:00:00",
                          format="%Y/%m/%d %H:%M:%S",
                          tz="Pacific/Kiritimati")
EN_9798 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_9798_EN,]
EN_9798 <- EN_9798[EN_9798$date<end_9798_EN,]
max(EN_9798$dhw)

start_0203_EN <- as.POSIXct("2002/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_0203_EN <- as.POSIXct("2003/12/31 00:00:00",
                          format="%Y/%m/%d %H:%M:%S",
                          tz="Pacific/Kiritimati")
EN_0203 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_0203_EN,]
EN_0203 <- EN_0203[EN_0203$date<end_0203_EN,]
max(EN_0203$dhw)


start_0910_EN <- as.POSIXct("2009/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_0910_EN <- as.POSIXct("2010/12/31 00:00:00",
                          format="%Y/%m/%d %H:%M:%S",
                          tz="Pacific/Kiritimati")
EN_0910 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_0910_EN,]
EN_0910 <- EN_0910[EN_0910$date<end_0910_EN,]
max(EN_0910$dhw)

start_1417_EN <- as.POSIXct("2014/1/1 00:00:00",
                            format="%Y/%m/%d %H:%M:%S",
                            tz="Pacific/Kiritimati")
end_1417_EN <- as.POSIXct("2017/12/31 00:00:00",
                          format="%Y/%m/%d %H:%M:%S",
                          tz="Pacific/Kiritimati")
EN_1417 <- dhw_bleachingsites_mean[dhw_bleachingsites_mean$date>start_1417_EN,]
EN_1417 <- EN_1417[EN_1417$date<end_1417_EN,]
max(EN_1417$dhw)

EN_max <- t(data.frame("EN_1987_1988"=max(EN_8788$dhw),
                     "EN_1992_1993"=max(EN_9293$dhw),
                     "EN_1997_1998"=max(EN_9798$dhw),
                     "EN_2002_2003"=max(EN_0203$dhw),
                     "EN_2009_2010"=max(EN_0910$dhw),
                     "EN_2015_2016"=max(EN_1417$dhw)))
colnames(EN_max) <- "dhw"
