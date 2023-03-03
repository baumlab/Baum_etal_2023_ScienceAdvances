# Load necessary packages
library(ggplot2)
library(gridExtra)
library(colorRamps)
library(here)
library(dplyr)
library(tidyr)

# Load necessary data
load("data/NOAA_5km_bysite.RData") 

# Set x limits
xlim1 <- as.POSIXct("2011/6/1 00:00:00",
                    format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim2 <- as.POSIXct("2017/1/1 00:00:00",
                    format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim3 <- as.POSIXct("2015/2/1 00:00:00",
                    format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim4 <- as.POSIXct("2016/9/1 00:00:00",
                    format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")

# cols <- colorRamps::primary.colors(n=19)
cols <- viridis::viridis(n = 19,direction = -1)

dhw_allsites_long <- dhw_allsites %>% 
  gather(site, dhw, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, -date) %>% filter(site %in% c("X6","X14","X8","X35","X34","X9","X32","X27","X30","X26","X25","X3","X38","X1","X23","X20","X15","X19")) 
head(dhw_allsites_long)


# Plot
NOAA_DHW <- ggplot(aes(x=date,y=dhw,color=site),data=dhw_allsites_long)+ 
  theme_classic()+
  theme(legend.position = "right")+
  geom_hline(yintercept = 4,linetype="dashed")+
  geom_hline(yintercept = 8,linetype="dashed")+
  geom_hline(yintercept = 12,linetype="dashed")+
  geom_hline(yintercept = 16,linetype="dashed")+
  geom_hline(yintercept = 20,linetype="dashed")+
  geom_hline(yintercept = 24,linetype="dashed")+
  geom_hline(yintercept = 28,linetype="dashed")+
  geom_line()+
  # geom_line(aes(x=date,y=X6),color=cols[2])+
  # geom_line(aes(x=date,y=X14),color=cols[3])+
  # geom_line(aes(x=date,y=X8),color=cols[4])+
  # geom_line(aes(x=date,y=X35),color=cols[5])+
  # geom_line(aes(x=date,y=X34),color=cols[6])+
  # geom_line(aes(x=date,y=X9),color=cols[7])+
  # geom_line(aes(x=date,y=X32),color=cols[8])+
  # geom_line(aes(x=date,y=X27),color=cols[9])+
  # geom_line(aes(x=date,y=X30),color=cols[10])+
  # geom_line(aes(x=date,y=X26),color=cols[11])+
  # geom_line(aes(x=date,y=X25),color=cols[12])+
  # geom_line(aes(x=date,y=X3),color=cols[13])+
  # geom_line(aes(x=date,y=X38),color=cols[14])+
  # geom_line(aes(x=date,y=X1),color=cols[15])+
  # geom_line(aes(x=date,y=X23),color=cols[16])+
  # geom_line(aes(x=date,y=X20),color=cols[17])+
  # geom_line(aes(x=date,y=X15),color=cols[18])+
  # geom_line(aes(x=date,y=X19),color=cols[19])+
  scale_x_datetime(name=NULL, expand=c(0,0), 
                   limits=c(xlim3,xlim4),
                   date_labels = "%b-%Y",
                   date_breaks = "3 months")+
  scale_y_continuous(name="DHW (ºC-week)",
                     limits=c(0,26),expand=c(0,0), 
                     breaks = c(0,4,8,12,16,20,24))+
  scale_color_manual(values=cols[c(1:19)], 
                     breaks=c('X5', 'X6', 'X14', 'X8', 'X35', 'X34', 
                              'X9', 'X32', 'X27', 'X30', 'X26',
                              'X25', 'X3', 'X38', 'X1',
                              'X23', 'X20', 'X15', 'X19'),
                     labels=c("VL3", "M6", "M4", "M1", "M2", "M3",
                              "L4", "VH2", "VH1", "VH3", "H2",
                              "M5", "L1", "L2", "M10",
                              "L5", "VL5", "VL1", "VL2"))

NOAA_DHW

# Make pdf
pdf(file = "figures/Figure_S5.pdf", 
    width = 5.5, height = 5, useDingbats = FALSE)
NOAA_DHW
dev.off()

# Make jpg
jpeg(file = "figures/Figure_S5.jpeg", 
    width = 5.5, height = 5, units="in", res=300)
NOAA_DHW
dev.off()
