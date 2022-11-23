
#Transformation of coral communities subjected to an unprecedented heatwave is modulated by local disturbance

# Authors:Julia K. Baum [1,2*], Danielle C. Claar [1,3], Kristina L. Tietjen [1], Jennifer M.T. Magel [1,4], 
#         Dominique G. Maucieri [1], Kim M. Cobb [5], Jamie M. McDevitt-Irwin [1,6]
# Institutions:
# [1] Department of Biology, University of Victoria, PO BOX 1700 Station CSC, Victoria, British Columbia, V8W 2Y2, Canada.
# [2] Hawaii Institute of Marine Biology, University of Hawaii, Kaneohe, HI, 96744, USA.
# [3] School of Aquatic and Fisheries Sciences, University of Washington, Seattle, WA, USA.
# [4] Department of Forest and Conservation Sciences, University of British Columbia, 2424 Main Mall, Vancouver, British Columbia, V6T 1Z4, Canada.
# [5] School of Earth and Atmospheric Sciences, Georgia Institute of Technology, Atlanta, GA, USA. 
# [6] Hopkins Marine Station, Stanford University, 120 Ocean View Blvd, CA, 93950, USA. 
# *Corresponding Author: Julia K. Baum, Tel: (250) 858-9349, Email: baum@uvic.ca 


# Script to create figures for the tagged coral data

#<-------------------------------------------------->

# Clear environment
rm(list=ls())

# Load packages
library(here)
library(ggplot2)
library(beyonce)
library(interactions)
library(ggnewscale)
library(lemon)
library(grid)


# Load data
load(file="data/KI_TaggedCoralData.RData")

# Look at dfs
head(tagged.data) #for figure 5a,b,c
head(tagged.lh) # for figure 5d and e
head(species.glm) # for figure 5f and S10
summary(species.glm)  # for figure 5f and S10
head(species.figure) # for figure S9
head(species.figure.bleaching) # for figure S11

#<-------------------------------------------------->

# Figure 5

###############
### panel a ###
###############

# create color palette
ovcolor <-  beyonce_palette(90)

# make function for color transparency
makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

# Plot all corals against local disturbance

fig5a <- ggplot(data=tagged.data, aes(y=status.after.numeric, x=localdisturbance.continuous_rescaled,color=status.after) ) +
  geom_point(cex=7, position = position_jitter(w = 0.15, h =0)) +
  annotate(geom = "text", x = -0.2, y = 0.25, label = "italic(N)==474", parse = TRUE, color = "black", size = 11) +
  annotate(geom = "text", x = -0.2, y = 0.15, label = "italic(P)<0.001", parse = TRUE, color = "black", size =11) +
  theme_classic()+
  #stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  stat_smooth(method="glm", method.args=list(family=binomial), col="black", fullrange = TRUE)  +
  scale_color_manual(values=c(makeTransparent(ovcolor[1]), makeTransparent("grey28")))+ 
  theme(legend.position = "none")+
  ylab("")+
  xlab("") + 
  theme(axis.title = element_text(size = 38), axis.text = element_text(size = 32, color = "black")) + 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0)) + 
  scale_x_continuous(limits = c(-0.55, 2.02), breaks = c(-0.5, 0.0, 0.5, 1.0, 1.5, 2.0))

fig5a


###############
### panel b ###
###############

# select for just stress-tolerant corals
tagged.st <- tagged.data[!(tagged.data$species == "Montipora foliosa" | tagged.data$species == "Pocillopora grandis"), ]
tagged.st <-droplevels(tagged.st)

# create color palette
lhcolors <- beyonce_palette(60)

# Logistic regression plot stress-tolerant corals against local disturbance
fig5b <- ggplot(data=tagged.st, aes(y=status.after.numeric, x=localdisturbance.continuous_rescaled,color=status.after) ) +
  geom_point(cex=7, position = position_jitter(w = 0.1, h =0)) +
  annotate(geom = "text", x = -0.2, y = 0.3, label = "italic(N)==273", parse = TRUE, color = "black", size = 11) +
  annotate(geom = "text", x = -0.2, y = 0.2, label = "italic(P)<0.001", parse = TRUE, color = "black", size =11) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent(lhcolors[9]), makeTransparent("grey28")))+ 
  theme(legend.position = "none")+
  ylab("Survival")+
  xlab("")+ 
  theme(axis.title = element_text(size = 38), axis.text = element_text(size = 32, color = "black")) + 
  scale_y_continuous(breaks = c(0.00, 0.5, 1.00)) +
  scale_x_continuous(limits = c(-0.55, 2.02), breaks = c(-0.5, 0.0, 0.5, 1.0, 1.5, 2.0))

fig5b

###############
### panel c ###
###############

#select for competitive corals
tagged.c <- tagged.data[(tagged.data$species == "Montipora foliosa" | tagged.data$species == "Pocillopora grandis"), ]
tagged.c <-droplevels(tagged.c)

# create color palette
lhcolors <- beyonce_palette(60)

# Logistic regression plot competitive corals against local disturbance
fig5c <-ggplot(data=tagged.c, aes(y=status.after.numeric, x=localdisturbance.continuous_rescaled,color=status.after) ) +
  geom_point(cex=7, position = position_jitter(w = 0.1, h =0)) +
  annotate(geom = "text", x = -0.2, y = 0.3, label = "italic(N)==201", parse = TRUE, color = "black", size = 11) +
  annotate(geom = "text", x = -0.2, y = 0.2, label = "italic(P)==0.355", parse = TRUE, color = "black", size =11) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent(lhcolors[1]), makeTransparent("grey28")))+ 
  theme(legend.position = "none")+
  ylab("")+
  xlab("Human Disturbance")+ 
  theme(axis.title = element_text(size = 38), axis.text = element_text(size = 32, color = "black")) + 
  scale_y_continuous(breaks = c(0.00, 0.5, 1.00)) +
  scale_x_continuous(limits = c(-0.55, 2.02), breaks = c(-0.5, 0.0, 0.5, 1.0, 1.5, 2.0))

fig5c


###############
### panel d ###
###############

# select for just stress-tolerant corals
tagged.lh.st <- tagged.lh[tagged.lh$lifehistory == "Stress tolerant", ]

# create color palette
lhcolors <- beyonce_palette(60)

# plot stress-tolerant corals against categorical local disturbance
fig5d <- ggplot(tagged.lh.st, aes(x = reorder(localdisturbance.categorical, localdisturbance.numerical), y=status.proportion, fill=status.after)) + geom_bar(stat='identity', position='stack', color = "white") +
  labs(x="", y="", fill="") + 
  scale_fill_manual(values = c(lhcolors[9], "grey28"), labels=c("ALIVE"="Alive", "DEAD"="Dead")) + 
  scale_x_discrete(labels=c("VERYLOW"="VL", "LOW"="L", "MED"="M", "VERYHIGH"="VH")) + 
  scale_y_continuous(expand = c(0,0), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
  theme_classic() + theme(axis.title = element_text(size = 34), axis.text = element_text(size = 28, color = "black"), legend.text = element_text(size = 32), legend.position = "top", legend.key.size = unit(2, "line")) 

fig5d


###############
### panel e ###
###############

#select for competitive corals
tagged.lh.c <- tagged.lh[!tagged.lh$lifehistory == "Stress tolerant", ]

# create color palette
lhcolors <- beyonce_palette(60)

# plot competitive corals against categorical local disturbance
fig5e <- ggplot(tagged.lh.c, aes(x = reorder(localdisturbance.categorical, localdisturbance.numerical), y=status.proportion, fill=status.after)) + geom_bar(stat='identity', position='stack', color = "white") +
  labs(x="Human Disturbance", y="", fill="") + 
  scale_fill_manual(values = c(lhcolors[1], "grey28"),  labels=c("ALIVE"="Alive", "DEAD"="Dead")) + 
  scale_x_discrete(labels=c("VERYLOW"="VL", "LOW"="L", "MED"="M", "VERYHIGH"="VH")) +
  scale_y_continuous(expand = c(0,0), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
  theme_classic() + theme(axis.title = element_text(size = 38), axis.text = element_text(size = 28, color = "black"), legend.text = element_text(size = 32), legend.position = "top", legend.key.size = unit(2, "line")) 

fig5e


###############
### panel f ###
###############

# model plot of interaction between species and local disturbance
fig5f <- interact_plot(model = species.glm, pred = disturbcont_x, modx = comspecies, 
                                 interval = TRUE, x.label = "Human Disturbance", y.label = "Survival", legend.main = "", 
                                 vary.lty = TRUE, modx.labels = c("P. lobata" , "Platygyra sp"="P. ryukyuensis", 
                                                                  "Favites pentagona"="F. pentagona", "Favia spp."="Dipsastraea spp.", 
                                                                  "Hydnophora microconos"="H. microconos", 
                                                                  "Pocillopora grandis"="P. grandis",  "Montipora foliose"="M. aequituberculata"), 
                                 colors = c("Porites lobata"="darkred", "Platygyra sp"="tomato2", 
                                            "Favites pentagona"="lightcoral", "Favia matthai"="goldenrod2", "Hydnophora microconos"="sienna1",
                                                                                                                                                                                                                                                                                                                 "Pocillopora grandis"="royalblue3", "Montipora foliosa"="midnightblue"),  line.thickness = 2)

fig5f <- fig5f + theme_classic() +
  # annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1.5) +
  theme(axis.text = element_text(size = 32, color = "black"), axis.title = element_text(size = 36, color = "black"), 
        plot.background=element_blank(), panel.background = element_blank(), legend.position = "bottom", 
        legend.text = element_text(size = 30, color = "black"), legend.key.size = unit(4, "line"))  +
  guides(color = guide_legend(nrow = 4)) +
  scale_y_continuous(expand = c(0.005,0.01), breaks = c(0.0, .25, .5, .75, 1.0), 
                     labels = c("0.0", 0.25, 0.5, 0.75, "1.0"), limits = c(0.0, 1.001)) + scale_x_continuous(expand = c(0.03,0)) 

fig5f


###############
### combine ###
###############

#pdf("figures/Figure5.pdf", width = 27, height = 21)

multiplot(fig5a, fig5b, fig5c, fig5d, fig5e, fig5f, layout = matrix(c(1,1,1,1,1,4,4,4,4,6,6,6,6,6,6,6,
                                                                                 1,1,1,1,1,4,4,4,4,6,6,6,6,6,6,6,
                                                                                 2,2,2,2,2,4,4,4,4,6,6,6,6,6,6,6,
                                                                                 2,2,2,2,2,5,5,5,5,6,6,6,6,6,6,6,
                                                                                 3,3,3,3,3,5,5,5,5,6,6,6,6,6,6,6,
                                                                                 3,3,3,3,3,5,5,5,5,6,6,6,6,6,6,6),nrow=6,byrow = TRUE))

# and y-axis label for panel d and e
grid.text("Proportion of coral colonies by status", x = unit(0.32, "npc"), y = unit(0.5, "npc"), gp = gpar(fontsize=38), rot = 90)

# add panel labels
grid.text("A", x = unit(0.013, "npc"), y = unit(0.985, "npc"), gp = gpar(fontsize=45, fontface = "bold"))
grid.text("B", x = unit(0.013, "npc"), y = unit(0.651, "npc"), gp = gpar(fontsize=45, fontface = "bold"))
grid.text("C", x = unit(0.013, "npc"), y = unit(0.319, "npc"), gp = gpar(fontsize=45, fontface = "bold"))
grid.text("D", x = unit(0.355, "npc"), y = unit(0.985, "npc"), gp = gpar(fontsize=45, fontface = "bold"))
grid.text("E", x = unit(0.355, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize=45, fontface = "bold"))
grid.text("F", x = unit(0.58, "npc"), y = unit(0.985, "npc"), gp = gpar(fontsize=45, fontface = "bold"))

#dev.off()


#<-------------------------------------------------->

# Figure S9


figS9 <- ggplot(species.figure, aes(x = reorder(localdisturbance.categorical, localdisturbance.numerical), y=status.proportion)) + 
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Porites lobata"), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("darkred", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Platygyra sp."), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("tomato2", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Favites pentagona"), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"),values = c("lightcoral", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Dipsastraea spp."), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("goldenrod2", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Hydnophora microconos"), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("sienna1", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Pocillopora grandis"), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("royalblue3", "grey28")) +
  new_scale_fill() +
  geom_bar(stat='identity', position='stack', aes(fill = status.after), data = ~subset(., species == "Montipora aequituberculata"), color = "white") +
  scale_fill_manual(labels=c("ALIVE"="Alive", "DEAD"="Dead"), values = c("midnightblue", "grey28")) +
  facet_rep_wrap(~species, repeat.tick.labels = FALSE)+ 
  
  labs(x="Human Disturbance", y="Proportion of coral colonies by status\n  ", fill="") +
  scale_x_discrete(labels=c("VERYLOW"="VL", "LOW"="L", "MED"="M", "VERYHIGH"="VH")) + 
  scale_y_continuous(expand = c(0,0), labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
  theme_classic() + theme(axis.title = element_text(size = 14), 
                          axis.text = element_text(size = 12, color = "black"), 
                          strip.background = element_blank(), strip.text = element_text(size = 12, face = "italic"),
                          #panel.spacing = unit(1, "lines"), 
                          axis.ticks = element_line(size = 1), axis.ticks.length = unit(0.3, "lines"),
                          legend.text = element_text(size = 12), legend.position = c(.5,.12), 
                          legend.key.size = unit(1, "line")) + 
  guides(fill = guide_legend(byrow=TRUE, ncol = 2)) 

grid.text("A", x = unit(0.06, "npc"), y = unit(0.955, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("B", x = unit(0.395, "npc"), y = unit(0.955, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("C", x = unit(0.71, "npc"), y = unit(0.955, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("D", x = unit(0.06, "npc"), y = unit(0.63, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("E", x = unit(0.395, "npc"), y = unit(0.63, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("F", x = unit(0.71, "npc"), y = unit(0.63, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("G", x = unit(0.06, "npc"), y = unit(0.31, "npc"), gp = gpar(fontsize=14, fontface = "bold"))


#jpeg("figures/figureS9.jpg", res = 300, width = 8, height = 8, units = "in")

figS9 #legends were cleaned up in photoshop

#dev.off()

#<-------------------------------------------------->

# Figure S10

figS10 <- interact_plot(model = species.glm, pred = disturbcont_x, 
                                  modx = comspecies, interval = TRUE, x.label = "Human Disturbance", 
                                  y.label = "Survival", legend.main = "", plot.points = TRUE, 
                                  jitter = c(0.1, .1), vary.lty = FALSE, point.alpha = 0.6, 
                                  facet.modx = TRUE, colors = c("Porites lobata"="darkred", 
                                                                "Platygyra sp"="tomato2", "Favites pentagona"="lightcoral", 
                                                                "Favia matthai"="goldenrod2", "Hydnophora microconos"="sienna1",
                                                                "Pocillopora grandis"="royalblue3", "Montipora foliosa"="midnightblue"))

figS10 <- figS10 + theme_classic() +
  theme(strip.text = element_blank(), axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 14, color = "black"), plot.background=element_blank(), 
        panel.background = element_blank()) + 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), labels = c(0.0, 0.5, 1.0)) + 
  scale_x_continuous(limits = c(-0.55, 2.02), breaks = c(-0.5, 0.0, 0.5, 1.0, 1.5, 2.0))

#jpeg("figures/figureS10.jpg", res = 300, width = 8, height = 6, units = "in")

figS10 # legend added in photoshop

grid.text("A", x = unit(0.035, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("B", x = unit(0.365, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("C", x = unit(0.675, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("D", x = unit(0.035, "npc"), y = unit(0.67, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("E", x = unit(0.365, "npc"), y = unit(0.67, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("F", x = unit(0.675, "npc"), y = unit(0.67, "npc"), gp = gpar(fontsize=14, fontface = "bold"))
grid.text("G", x = unit(0.035, "npc"), y = unit(0.37, "npc"), gp = gpar(fontsize=14, fontface = "bold"))

#dev.off()

#<-------------------------------------------------->

# Figure S11

# Plot as a barplot, assigning different colours to the factor "Bleached" (healthy vs. bleaching coral). Colour the border of the bars black. 
species.labels=c("Platygyra sp."=expression(italic("Platygyra sp.")),"Dipsastraea spp."=expression(italic("Dipsastraea spp.")), 
                 "Favites pentagona"=expression(italic("Favites pentagona")), 
                 "Hydnophora microconos"=expression(italic("Hydnophora microconos")),
                 "Montipora aequituberculata"=expression(italic("Montipora aequituberculata")),
                 "Pocillopora grandis"=expression(italic("Pocillopora grandis")), 
                 "Porites lobata"=expression(italic("Porites lobata")), parse = TRUE)

figS11 <- ggplot(species.figure.bleaching, aes(x=species, y=proportion, group=binary.bleaching.2015c)) + 
  geom_bar(stat="identity", aes(fill=interaction(binary.bleaching.2015c,species)), colour="black") +               
  labs(x="", y="Percent Bleaching and Healthy", colour="", title="") +                                  
  theme_classic() +                                                                   
  theme(strip.background=element_blank(),                                              
        legend.title=element_blank(),                                                  
        panel.spacing = unit(1.5, "lines"),
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),    
        axis.line.x = element_line(color="black", size = 0.5),                         
        axis.line.y = element_line(color="black", size = 0.5),                         
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        axis.text = element_text(size = 12, color = "black"),
        legend.position=c(0.9,0.93)) +  
  scale_x_discrete(labels=species.labels) +     
  scale_fill_manual(
    values=c("white", "tomato2",  #platy
             "white", "darkred", #por
             "white", "sienna1",  #hdyno
             "white","lightcoral",  # fpenta
             "white","goldenrod2",  #dip
             "white", "midnightblue",  #monti
             "white","royalblue3"  #poci
    )) +  
  guides(fill="none") +
  expand_limits(y=c(0, 100))                                        

#jpeg("figures/figureS11.jpg", res = 300, width = 8, height = 6, units = "in")

figS11

#dev.off()
