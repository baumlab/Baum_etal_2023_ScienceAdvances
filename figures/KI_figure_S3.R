####This R script is to test for relationships between environmental variables and disturbance;

# Load necessary libraries
library(readxl)
library(tidyverse)
library(cowplot)
library(dplyr)
library(stringr)
require(gridExtra)


# Load data
env<-read_excel("data/sites/KI_env_all.xlsx")
site_info<-read_csv("data/sites/KI_Monitoring_SiteData_Oct2020.csv")
site_info$site<-as.character(site_info$site)
str(site_info)
coral.data.master <- read.csv("data/spq/Coral_Data_Master_forS3figure.csv", stringsAsFactors = FALSE)
microbe<-read_csv("data/microbial_counts/MI_counts.csv")

##color by disturbance group
colors <- c("Very high" = "#A60021", "High" = "#FFAD73",  "Medium" = "#ABF8FF", "Low" = "#40A1FF", "Very low" = "#2A0BD9")

####################################################################
####################SEDIMENT COVER (Panels A and B)#################
####################################################################

## Clean EN timepoint names
unique(coral.data.master$Field.Season.Cat)
# See which field seasons correspond to each time point
aggregate(Field.Season ~ Field.Season.Cat, coral.data.master, FUN = unique)
# Aggregate "...EN" timepoints into "During"
coral.data.master$Field.Season.Cat[coral.data.master$Field.Season.Cat == "EarlyEN"] <- "During"
coral.data.master$Field.Season.Cat[coral.data.master$Field.Season.Cat == "MidEN"] <- "During"
coral.data.master$Field.Season.Cat[coral.data.master$Field.Season.Cat == "LateEN"] <- "During"

## Change 'heat' variable for site 38
# Remove 2018 data
coral.data.master <- coral.data.master[!grepl("2018", coral.data.master$Field.Season), ]
# Change 2016a to 'After'
coral.data.master$Field.Season.Cat[coral.data.master$Site == "site38" & 
                                     coral.data.master$Field.Season == "KI2016a"] <- "After"

## Calculate percent cover
# Subset the data to get only sediment/sand
percent.cover <- coral.data.master %>% subset(coralnet.tag == "Sediment"|coralnet.tag == "Sand") %>% subset(Field.Season.Cat=="Before") %>% 
  group_by(Site, coralnet.tag, Field.Season, Field.Season.Cat, adj.total.points.site) %>%
  summarise(sum.cover = sum(ID.points))
# Calculate percent cover for each site/field season
percent.cover$p.cover <- (percent.cover$sum.cover/percent.cover$adj.total.points.site) * 100

#Create new "site" column that matches metadata with disturbance 
percent.cover$site <-gsub( "site", "", percent.cover$Site)

percent.cover_sediment<-left_join(percent.cover, site_info, by = "site")
sed <- percent.cover_sediment 

##Sediment only
sed_true<-subset(sed, coralnet.tag=="Sediment") %>% 
  group_by(site) %>%
  summarise(percent.sed=mean(p.cover))

##Sand only
sed_sand<-subset(sed, coralnet.tag=="Sand")   %>% group_by(site) %>%
  summarise(percent.sed=mean(p.cover))

##Sediment and sand together
sed_comb<-sed %>% group_by(site, Field.Season) %>%
  summarise(percent.sed=sum(p.cover)) 
sed_comb<-sed_comb %>% group_by(site) %>%
  summarise(percent.sed=mean(percent.sed))

##Join with dataset that has disturbance levels
sed_comb<-left_join(sed_comb, site_info, by = "site")
sed_true<-left_join(sed_true,  site_info, by = "site")


## Plot 
sed1<-ggplot(sed_comb, aes(x = sqrt(continous.pressure.2km), y = percent.sed, color = pressure.group))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot()+
  xlab("Human Disturbance")+
  ylab("Percent cover sediment") + ylim(0,60) +
  scale_color_manual(values=colors) + 
  guides(color="none") 

sed1

###Linear model for combined
z<-lm(sqrt(continous.pressure.2km)~percent.sed, data = sed_comb)
summary(z)

sed2<-ggplot(sed_true, aes(x = sqrt(continous.pressure.2km), y = percent.sed, color = pressure.group))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot() + xlab("Human Disturbance")+
  ylab("Percent cover sediment") + ylim(0,60) +
  scale_color_manual(values=colors) + 
  guides(color="none") 

sed2

##Linear model for sediment only
z<-lm(sqrt(continous.pressure.2km)~percent.sed, data = sed_true)
summary(z)


####################################################################
####################VISIBILITY Panel (Panel C)######################
####################################################################

# Change to numeric 
env$visibility_m<-env$visibility_m %>% as.numeric
head(env)

# Summarize env parameters by site
env_avg<-env %>% 
  group_by(site) %>%
  summarise(vis=mean(visibility_m, na.rm=TRUE), 
  )
env_avg$site <- as.character(env_avg$site)
head(env_avg)
head(site_info)
dat<-left_join(env_avg, site_info, by = "site")

vis<-ggplot(dat, aes(x = sqrt(continous.pressure.2km), y = vis, color = pressure.group))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot()+
  xlab("Human Disturbance")+
  ylab("Visibility") + ylim(0,40)+
  scale_color_manual(values=colors) +
  labs(color = "Human Disturbance") +
  theme(legend.position = "bottom")

vis #This is the visibility panel

#Linear model for visibility
z<-lm(sqrt(continous.pressure.2km)~vis, data = dat)
summary(z)

####################################################
#############MICROBIAL PANEL - Panel D##############
####################################################


microbe$Site<-as.character(microbe$Site)
microbe_sum<-microbe %>% group_by(Site, human_disturbance) %>% 
  summarize(Cells = mean(cells_per_ml), sd = sd(cells_per_ml))

##color by disturbance group
colors.vhvl <- c("Very High" = "#A60021", "Very Low" = "#2A0BD9")

MI<-ggplot(microbe_sum, aes(x=Site, y=Cells, group=human_disturbance, color=human_disturbance)) + 
  geom_pointrange(aes(ymin=Cells-sd, ymax=Cells+sd), cex=0.7, show.legend=FALSE)+
  theme_cowplot()+xlab("Site")+ylab("Cells per mL") +
  scale_color_manual(values=colors.vhvl) + 
  guides(color="none") 

MI ##This is the final panel

aov(cells_per_ml~as.factor(Site), data=microbe) %>% TukeyHSD()


####################################################
#############Combine all panels  ##############
####################################################

## Make legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- g_legend(vis)


###Make paneled figure

#pdf(file = "figures/disturbance_figure_colored.pdf", width = 10, height = 8)

grid.arrange(arrangeGrob(sed2, sed1, vis + theme(legend.position = "none"), MI, legend,
                         layout_matrix = rbind(c(1,1,2,2),
                                               c(3,3,4,4),
                                               c(NA,5,5,NA)),
                         heights=rbind((unit(3, "in")),(unit(3, "in")), (unit(1, "in")))))
#a
grid.text("A", x = unit(0.015, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize=18, fontface = "bold" ))
grid.text(expression(paste(italic("P"), " < 0.001")), x = unit(0.423, "npc"), y = unit(0.68, "npc"), gp = gpar(fontsize=14 ))
grid.text(expression("R"^2~" = 0.5041"), x = unit(0.43, "npc"), y = unit(0.66, "npc"), gp = gpar(fontsize=14 ))

#b
grid.text("B", x = unit(0.515, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize=18, fontface = "bold" ))
grid.text(expression(paste(italic("P"), " = 0.001")), x = unit(0.893, "npc"), y = unit(0.68, "npc"), gp = gpar(fontsize=14 ))
grid.text(expression("R"^2~" = 0.4421"), x = unit(0.9, "npc"), y = unit(0.66, "npc"), gp = gpar(fontsize=14 ))

#c
grid.text("C", x = unit(0.01, "npc"), y = unit(0.54, "npc"), gp = gpar(fontsize=18, fontface = "bold" ))
grid.text(expression(paste(italic("P"), " = 0.0019")), x = unit(0.427, "npc"), y = unit(0.25, "npc"), gp = gpar(fontsize=14 ))
grid.text(expression("R"^2~" = 0.4496"), x = unit(0.43, "npc"), y = unit(0.23, "npc"), gp = gpar(fontsize=14 ))

#d
grid.text("D", x = unit(0.51, "npc"), y = unit(0.54, "npc"), gp = gpar(fontsize=18, fontface = "bold" ))
grid.text(expression(paste(italic("P"), " < 0.001")), x = unit(0.9, "npc"), y = unit(0.25, "npc"), gp = gpar(fontsize=14 ))
grid.text("a", x = unit(0.658, "npc"), y = unit(0.328, "npc"), gp = gpar(fontsize=15))
grid.text("b", x = unit(0.755, "npc"), y = unit(0.26, "npc"), gp = gpar(fontsize=15))
grid.text("c", x = unit(0.848, "npc"), y = unit(0.51, "npc"), gp = gpar(fontsize=15))
grid.text("c", x = unit(0.943, "npc"), y = unit(0.47, "npc"), gp = gpar(fontsize=15))

#dev.off()


