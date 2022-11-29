####This R script is to test for relationships between environmental variables and disturbance;

# Load necessary libraries
library(here)
library(readxl)
library(tidyverse)
library(cowplot)
library(dplyr)
library(stringr)


# Load data
env<-read_excel("data/KI_env_all.xlsx")
site_info<-read_csv("data/KI_Monitoring_SiteData_Oct2020.csv")
site_info$site<-as.character(site_info$site)
str(site_info)
coral.data.master <- read.csv("data/Coral_Data_Master_forS3figure.csv", stringsAsFactors = FALSE)
microbe<-read_csv("data/MI_counts.csv")

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

sed1<-ggplot(sed_comb, aes(x = sqrt(continous.pressure.2km), y = percent.sed))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot()+
  xlab("Human disturbance")+
  ylab("Percent cover sediment") + ylim(0,60)

###Linear model for combined
z<-lm(sqrt(continous.pressure.2km)~percent.sed, data = sed_comb)
summary(z)

sed2<-ggplot(sed_true, aes(x = sqrt(continous.pressure.2km), y = percent.sed))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot() + xlab("Human disturbance")+
  ylab("Percent cover sediment") + ylim(0,60)

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

vis<-ggplot(dat, aes(x = sqrt(continous.pressure.2km), y = vis))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot()+
  xlab("Human disturbance")+
  ylab("Visibility") + ylim(0,40)

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


MI<-ggplot(microbe_sum, aes(x=Site, y=Cells, group=human_disturbance, color=human_disturbance)) + 
  geom_pointrange(aes(ymin=Cells-sd, ymax=Cells+sd), cex=1.1, show.legend=FALSE)+
  theme_cowplot()+ scale_color_manual(values=c("black", "darkgrey"))+xlab("Site")+ylab("Cells per mL")

aov(cells_per_ml~as.factor(Site), data=microbe) %>% TukeyHSD()

MI ##This is the final panel



###Make paneled figure
plot_grid(sed2, sed1, vis, MI)




