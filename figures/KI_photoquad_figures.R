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


# Script to create figures for the photoquad data

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
library(cowplot)


# Load data
load(file="data/KI_PhotoQuadData.RData")

# Look at dfs
head(quad.before) #for fig 1C
head(quad.hc.heat) #for fig3A
head(quad.heat.human) #for fig 3E
head(percent.loss) #for fig 6A
head(percent.change) #for fig 6b graph
head(piechart.before) #for fig 6b pie chart before
head(piechart.after) #for fig 6b pie chart after
head(bleaching.heat) #for fig S6a
head(bleaching.heat.human) #for fig S6b
head(bleaching.heat.species) #for fig S6c
head(benthic.cover.heat) #for fig S7a
head(benthic.cover.heat.human) #for fig S7b-f
head(benthic.change.heat) #for fig S7g
head(benthic.change.heat.human) #for fig S7h-l

# Create labels for local disturbance categories
localdisturb.labels=c("Very.High"="Very High", "High"="High", "Medium"="Medium", "Low"="Low", "Very.Low"="Very Low")

# Create labels for the coral species
taxa.labels=c("Rare"="Rare Species", "Platygyra.spp"= expression(italic("Platygyra spp.")),  "Acropora.tabulate" = expression(paste(italic("Acropora spp."), " (tabulate)")), "Favia.matthai"=expression(italic("Dipsastraea matthaii")), 
              "Favia.stelligera"=expression(italic("Goniastrea stelligera ")), "Favites.pentagona"=expression(italic("Favites pentagona")), "Hydnophora.microconos"=expression(italic("Hydnophora microconos")), 
              "Montipora.foliose"=expression(paste(italic("Montipora"), " (foliose)")),"Montipora.encrusting"=expression(paste(italic("Monitopora"), " (encrusting)")), "Pavona.varians"=expression(italic("Pavona varians")),
              "Pocillopora.eydouxi"=expression(italic("Pocillopora grandis")), "Pocillopora.meandrina"=expression(italic("Pocillopora meandrina")), "Porites.spp"=expression(italic("Porites lobata")), "Leptastrea.spp"=expression(italic("Leptastrea spp.")), "Astreopora.spp"=expression(italic("Astreopora spp.")), "Pavona.duerdeni" = expression(italic("Pavona duerdeni")))



taxa.labels.short=c("Rare"="Rare Species", "Platygyra.spp"=expression(italic("Platygyra spp.")),  "Acropora.tabulate" = expression(paste(italic("Acropora"), " tab.")), "Favia.matthai"=expression(italic("D. matthaii")), 
                    "Favia.stelligera"=expression(italic("G. stelligera ")), "Favites.pentagona"=expression(italic("F. pentagona")), "Hydnophora.microconos"=expression(italic("H. microconos")), 
                    "Montipora.foliose"=expression(paste(italic("Montipora"), " fol.")),"Montipora.encrusting"=expression(paste(italic("Monitopora"), " en.")), "Pavona.varians"=expression(italic("P. varians")),
                    "Pocillopora.eydouxi"=expression(italic("P. grandis")), "Pocillopora.meandrina"=expression(italic("P. meandrina")), "Porites.spp"=expression(italic("P. lobata")), "Leptastrea.spp"=expression(italic("Leptastrea spp.")), "Astreopora.spp"=expression(italic("Astreopora spp.")), "Pavona.duerdeni" = expression(italic("P. duerdeni")), parse = TRUE)

# Create labels for heat
field.labels=c("EarlyEN"="Early", "MidEN"="Mid", "LateEN"= "Late")


# Create scale for coral species
taxa.cols=c("Rare"="gray88",
            "Leptastrea.spp"="thistle", 
            "Astreopora.spp"="lemonchiffon1", "Pavona.varians"="peachpuff1", "Platygyra.spp"="tomato2", "Favites.pentagona"="lightcoral", "Favia.matthai"="goldenrod2", "Hydnophora.microconos"="sienna1", "Favia.stelligera"="khaki2",  
            "Porites.spp"="darkred",  "Pavona.duerdeni" ="salmon3",
            "Montipora.encrusting"="blue3", "Pocillopora.meandrina"="steelblue2", "Pocillopora.eydouxi"="royalblue3", "Acropora.tabulate"="paleturquoise", "Montipora.foliose"="midnightblue")

# set color for overall plots
overall.color <-  beyonce_palette(90)


#<-------------------------------------------------->

# Figure 1

###############
### panel c ###
###############

#check orders
levels(quad.before$localdisturbance.categorical)
levels(quad.before$substrate)

#plot 
fig1c<-ggplot(quad.before, aes(x=localdisturbance.categorical, y=percent.cover,  fill=substrate)) +       
  geom_bar(stat="identity",position= "stack") +                                       
  labs(x="Human Disturbance", y="Percent Cover", title="") +                    
  theme_cowplot() +                                                                    
  theme(legend.title=element_blank(),                                                  
        axis.line.x = element_line(color="black", size = 0.5),                          
        axis.line.y = element_line(color="black", size = 0.5),                        
        axis.text = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.position="bottom") + 
  scale_fill_manual(breaks=rev(c("Other", "ABIOTIC.SUBSTRATES", "FLESH.MACRO", "TURF.ALGAE", "CCA.PEY", "SOFT.CORAL", "HARD.CORAL")),
                    values=c("#8660a5", "cadetblue2", "palevioletred1", "limegreen", "green4", "wheat2", "grey40"), 
                    labels=c("Hard Coral", "Soft Coral", "CCA", "Turf", "Macroalgae", "Abiotic", "Other"))+
  scale_x_discrete(labels = localdisturb.labels) + guides(fill = guide_legend(ncol = 5)) + 
  scale_y_continuous(expand = c(0,0))

fig1c


# #Print to pdfs
# pdf(file = "figures/spq/Fig1c_substrate_before.pdf", width = 4.5, height = 6)
# fig1b
# dev.off()



#<-------------------------------------------------->

# Figure 3

###############
### panel a ###
###############

#check order
levels(quad.hc.heat$heat)

#plot
#pdf(file = "figures/spq/Hardcoralcover_before.after.pdf", width = 4, height = 4.5)

ggplot(quad.hc.heat, aes(x = heat, y = percent.cover, fill = heat)) + geom_boxplot(position = "dodge2") +
  labs(x = "", y = "Percent Hard Coral Cover", fill = "") +
  scale_fill_manual(values = c("#654A89", "#C2B0D3"), breaks = c("Before", "After")) +
  theme_classic() + guides(fill = "none") +
  theme(axis.text = element_text(color = "black", size = 16), 
        axis.title.y=element_text(size=20, face="plain"))           

#dev.off()


###############
### panel e ###
###############

#check orders
levels(quad.heat.human$heat)
levels(quad.heat.human$localdisturbance.categorical)
levels(quad.heat.human$species) 

#plot
fig3e<-ggplot(quad.heat.human, aes(x=heat, y=percent.cover)) + 
  geom_bar(stat="identity", aes(fill=species)) + 
  theme_cowplot() + 
  labs(x="", y="Percent of Hard Coral Cover", shape="", title="") + 
  theme(strip.background=element_blank(), 
        legend.title=element_blank(),                                                   
        axis.title.y=element_text( size=26, face="plain"),     
        axis.line.x = element_line(color="black", size = 0.5),          
        legend.position = "bottom",
        axis.line.y = element_line(color="black", size = 0.5),                           
        axis.text = element_text(size = 24),
        strip.text = element_text(size = 24),
        legend.text = element_text(size = 22, hjust = 0),
        legend.key.size = unit(1.3, "line")) +                    
  facet_wrap(~localdisturbance.categorical, nrow=1, scales="free", labeller=labeller(localdisturbance.categorical=localdisturb.labels)) + 
  scale_fill_manual(values=taxa.cols, 
                    labels=taxa.labels.short) +
  scale_y_continuous(expand=c(0,0), limits=c(0,100.1)) +
  guides(fill = guide_legend(override.aes = list(colour = "black")))

fig3e

# # Print to pdf
# pdf(file = "figures/spq/sp_comp_beforeafter.pdf", width = 10.8, height = 8.2)
# fig3e
# dev.off()


#<-------------------------------------------------->

# Figure 6

###############
### panel a ###
###############

taxa.cols.lollipop=c("paleturquoise", "midnightblue", "blue3", "royalblue3", "steelblue2", "peachpuff1", "thistle", 
                      "lemonchiffon1", "sienna1", "goldenrod2",  "khaki2", "lightcoral", "darkred", "salmon3",  "tomato2")

fig6a<-ggplot(percent.loss) +
  geom_segment( aes(x=species, xend=species, y=0, yend=percent.loss.quad), color="grey40") +
  geom_point(aes(x=species, y=percent.loss.quad), color = "black", fill = taxa.cols.lollipop, size=4, shape = 21) + 
  geom_point(aes(x=species, y=percent.loss.tagged), color = "black", fill = taxa.cols.lollipop, size=4, shape = 23) + 
  coord_flip() + theme_light() +
  labs(x = "", y = "Percent Loss") + 
  scale_x_discrete(labels = taxa.labels.short) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,105)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey65"), panel.grid.minor.x = element_line(color = "grey75"),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

fig6a

###############
### panel b ###
###############

#### graph
graph <- ggplot(percent.change, aes(x=species, y=percent.change.quad)) +
  geom_bar(stat='identity', aes(fill = species), color = "black", size = 0.3) +
  labs(x="", y="Percent Change", fill = "") + 
  scale_fill_manual(values=taxa.cols, labels=taxa.labels) + 
  scale_x_discrete(labels = taxa.labels.short) +
  scale_y_continuous(breaks = c(-100, 0, 100, 200, 300), limits = c(-100, 301)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90,  hjust=1, vjust=0.5),
                          axis.text = element_text(color = "black")) + 
  guides(fill = "none") + 
  geom_segment(aes(x = 12, y = 240, xend = 12.5, yend = 240),
               arrow = arrow(length = unit(0.3, "cm")))

graph

#### pie chart before

pie.b <- ggplot(piechart.before, aes(x="", y=proportion, fill=species)) +
  geom_bar(stat="identity", width=1, color="black", size = 0.1) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  #geom_text(aes(y = ypos, label = taxa.labels.short), color = "black", size=2) +
  scale_fill_manual(values=taxa.cols, labels=taxa.labels)

pie.b

#### pie chart after

pie.af <- ggplot(piechart.after, aes(x="", y=proportion, fill=species)) +
  geom_bar(stat="identity", width=1, color="black", size = 0.1) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  #geom_text(aes(y = ypos, label = taxa.labels.short), color = "black", size=2) +
  scale_fill_manual(values=taxa.cols, labels=taxa.labels)

pie.af

#### combine

g2 <- ggplotGrob(pie.b)
g3 <- ggplotGrob(pie.af)
plot.ac <- graph + annotation_custom(grob = g3, xmin=12.5, xmax=15.5, ymin=125, ymax=350)
plot(plot.ac)
plot.abc <- plot.ac + annotation_custom(grob = g2, xmin=9, xmax=12, ymin=125, ymax=350)
plot(plot.abc)
fig6b <- plot.abc
plot(fig6b)


###############
### combine ###
###############

#pdf(file = "figures/spq/speciesfigure.pdf", width = 6, height = 7)

multiplot(fig6a, fig6b, layout = matrix(c(1,1,
                                             1,1,
                                             2,2,
                                             2,2,
                                             2,2), nrow=5, byrow=TRUE))

grid.text("A", x = unit(0.015, "npc"), y = unit(0.983, "npc"), gp = gpar(fontsize=15, fontface = "bold"))
grid.text("B", x = unit(0.015, "npc"), y = unit(0.573, "npc"), gp = gpar(fontsize=15, fontface = "bold"))

#dev.off()



#<-------------------------------------------------->

# Figure S6

###############
### panel a ###
###############

# Plot panel A: Stacked barplot  % cover,  healthy vs. bleaching coral, across three timepoints 
figS6a<-ggplot(bleaching.heat, aes(x=heat, y=percent.cover, group=bleaching)) + 
  geom_bar(stat="identity", aes(fill=factor(bleaching)), colour="black") +      
  geom_errorbar(aes(ymin=lower.sd, ymax=upper.sd),                              
                width=0.05,                                                     
                colour="black" ,                                                
                position=position_dodge(width=0.1)) +                           
  labs(x="", y="Percent Coral Cover", colour="", title="") +                    
  theme_cowplot() +                                                             
  theme(strip.background=element_blank(),                                       
        legend.title=element_blank(),                                           
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),
        axis.line.x = element_line(color="black", size = 0.5),                     
        axis.line.y = element_line(color="black", size = 0.5),                     
        legend.position=c(0.72,0.93)) +           
  panel_border(colour = "black", size = 0.75, linetype = 1, remove = FALSE) + 
  scale_x_discrete(breaks=c("Before", "EarlyEN", "MidEN", "LateEN", "After"),      
                   labels=c("Before", "Early", "Mid", "Late", "After")) +          
  scale_fill_manual(breaks=c("Bleaching", "Healthy"), values=c( "white", overall.color[1])) + 
  expand_limits(y=c(0, 60))    

figS6a

# # Print to pdfs
# pdf(file = "figures/spq/Supp_bleaching_A.pdf", width = 6, height = 4.5)
# figS6a
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Supp_bleaching_A.tiff", width = 6, height = 4.5, units="in",res=300)
# figS6a
# dev.off()

###############
### panel b ###
###############

# Plot Panel B: Stacked barplot  % cover,  healthy vs. bleaching coral, by fishing level 
figS6b<-ggplot(bleaching.heat.human, aes(x=heat, y=percent.cover, group=bleaching)) + 
  geom_bar(stat="identity", aes(fill=interaction(bleaching, localdisturbance.categorical)), colour="black") +              
  geom_errorbar(aes(ymin=lower.sd, ymax=upper.sd),                                     
                width=0.05,                                                            
                colour="black" ,                                                       
                position=position_dodge(width=0.1)) +  
  facet_wrap(~localdisturbance.categorical, nrow=1, scales="fixed", labeller=labeller(localdisturbance.categorical=localdisturb.labels)) + 
  labs(x="", y="Percent Coral Cover", colour="", title="") +                           
  theme_cowplot() +                                                                    
  theme(strip.background=element_blank(),                                              
        legend.title=element_blank(),                                                  
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),    
        axis.line.x = element_line(color="black", size = 0.5),                         
        axis.line.y = element_line(color="black", size = 0.5),                         
        legend.position=c(0.85,0.93),    
        strip.text = element_text(size = 14)) +                                                  
  panel_border(colour = "black", size = 0.75, linetype = 1,                            
               remove = FALSE) + 
  scale_x_discrete(breaks=c("Before", "EarlyEN", "MidEN", "LateEN", "After"),          
                   labels=c("Before", "Early", "Mid", "Late", "After")) +              
  scale_fill_manual(values=c("white", "#2A0BD9",                                       
                             "white", "#40A1FF",                                       
                             "white", "#ABF8FF",                                       
                             "white", "#A60021")) +    
  guides(fill="none") +
  expand_limits(y=c(0, 10))                                                                   

figS6b

# # Print to pdfs
# pdf(file = "figures/spq/Supp_bleaching_B.pdf", width = 13, height = 4)
# figS6b
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Supp_bleaching_B.tiff", width = 13, height = 4, units="in",res=300)
# figS6b
# dev.off()

###############
### panel c ###
###############

# Plot Panel C:Stacked barplot  % cover,  healthy vs. bleaching coral, by taxa
figS6c <- ggplot(bleaching.heat.species, aes(x=species, y=proportion, group=bleaching)) + 
  geom_bar(stat="identity", aes(fill=interaction(bleaching,species)), colour="black") +   
  facet_wrap(~heat, nrow=1, scales="fixed", labeller=labeller(heat=field.labels)) + 
  labs(x="", y="Percent Bleaching and Healthy", colour="", title="") +                    
  theme_cowplot() +                                                                     
  theme(strip.background=element_blank(),                                               
        legend.title=element_blank(),                                                   
        panel.spacing = unit(1.5, "lines"),
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),     
        axis.line.x = element_line(color="black", size = 0.5),                          
        axis.line.y = element_line(color="black", size = 0.5),                          
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position=c(0.9,0.93),   
        strip.text = element_text(size = 14)) +                                                  
  panel_border(colour = "black", size = 0.75, linetype = 1, remove = FALSE) + 
  scale_x_discrete(labels=taxa.labels) +              
  scale_fill_manual(
    values=c("white", "tomato2",
             "white", "darkred",
             "white", "lightcoral",
             "white","khaki2",
             "white","sienna1",
             "white","goldenrod2",
             "white","thistle",
             "white","peachpuff1",
             "white","lemonchiffon",
             "white","salmon3",  #Pavona duerdeni
             "white","steelblue2",
             "white","royalblue3",
             "white","blue3",
             "white","paleturquoise",
             # "white", "turquoise2",  # acropora corymbose
             "white","midnightblue"
    )) +     # Specify colours for bleaching and healthy coral (white for bleaching, black for healthy)
  guides(fill="none") +
  expand_limits(y=c(0, 100))  

figS6c

# # Print to pdfs
# pdf(file = "figures/spq/Supp_bleaching_C.pdf", width = 15, height = 6.3)
# figS6c
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Supp_bleaching_C.tiff", width = 15, height = 6, units="in",res=300)
# figS6c
# dev.off()


#<-------------------------------------------------->

# Figure S7

###############
### panel a ###
###############

figS7a <- ggplot(benthic.cover.heat, aes(x=heat, y=percent.cover,  group=substrate)) +     
  geom_bar(stat="identity", aes(fill=factor(substrate)), position=position_dodge(0.9)) +   
  geom_errorbar(aes(ymin=lower.sd, ymax=upper.sd),                                         
                width=0.05,                                                           
                colour="black" ,                                                      
                position=position_dodge(width=0.9)) +                                 
  labs(x="", y="Percent Cover", colour="", title="white\nthis out") +                 
  theme_cowplot() +                                                                   
  theme(strip.background=element_blank(),                                             
        legend.title=element_blank(),                                                 
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),   
        axis.line.x = element_line(color="black", size = 0.5),                        
        axis.line.y = element_line(color="black", size = 0.5),                        
        legend.position=c(0.8,0.85),  
        plot.title = element_text(color = "white"),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm")) + 
  panel_border(colour = "black", size = 0.75, linetype = 1, remove = FALSE) + 
  scale_fill_manual(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                    values=c("#8660a5", "palevioletred1", "limegreen", "green4", "grey"), 
                    labels=c("Hard Coral", "CCA & Peyssonelia", "Turf Algae", "Fleshy Macroalgae", "Other")) +  
  guides(fill="none") +
  ylim(0, 85) 

figS7a

# # Print to pdfs
# pdf(file = "figures/spq/Fig4A_final.pdf", width = 3, height = 3)
# figS7a
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Fig4A_final.tiff", width = 3, height = 3, units="in",res=300)
# figS7a
# dev.off()

#################
### panel b-f ###
#################

figS7b.f <- ggplot(benthic.cover.heat.human, aes(x=heat, y=percent.cover,  group=substrate)) +      
  geom_bar(stat="identity", aes(fill=factor(substrate)), position=position_dodge(0.9)) +            
  geom_errorbar(aes(ymin=lower.sd, ymax=upper.sd),                                           
                width=0.05,                                                           
                colour="black" ,                                                      
                position=position_dodge(width=0.9)) +                                 
  facet_wrap(~localdisturbance.categorical, nrow=1, scales="fixed", labeller=labeller(localdisturbance.categorical=localdisturb.labels)) +
  labs(x="", y="", colour="", title="") +                                
  theme_cowplot() +                                                                   
  theme(strip.background=element_blank(),                                             
        legend.title=element_blank(),                                                 
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),   
        axis.line.x = element_line(color="black", size = 0.5),                        
        axis.line.y = element_line(color="black", size = 0.5),                        
        legend.position=c(0.8,0.85),   
        plot.margin = unit(c(0, 0.5, 0, 0), "cm")) + 
  panel_border(colour = "black", size = 0.75, linetype = 1, remove = FALSE) + 
  scale_fill_manual(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                    values=c("#8660a5", "palevioletred1", "limegreen", "green4", "grey"), 
                    labels=c("Hard Coral", "CCA & Peyssonelia", "Turf Algae", "Fleshy Macroalgae", "Other")) +  
  guides(fill="none") +
  ylim(0, 85) 

figS7b.f

# # Print to pdfs
# pdf(file = "figures/spq/Fig4C_final.pdf", width = 14, height = 3.5)
# figS7b.f
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Fig4C_final.tiff", width = 14, height = 3.5, units="in",res=300)
# figS7b.f
# dev.off()

###############
### panel g ###
###############

figS7g <- ggplot(benthic.change.heat, aes(x=substrate, y=percent.change,  group=substrate)) +       
  geom_bar(stat="identity", aes(fill=factor(substrate)), position=position_dodge(0.9)) +    
  geom_text(aes(label=ifelse(percent.change>0,ceiling(percent.change),'')),vjust=-0.3) +
  geom_text(aes(label=ifelse(percent.change<0,ceiling(percent.change),'')),vjust=1.3) + 
  labs(x="", y="Percent Change", colour="", title="") +                                 
  theme_cowplot() +                                                                   
  theme(strip.background=element_blank(),  strip.text = element_blank(),              
        legend.title=element_blank(), 
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),   
        axis.line.x = element_line(color="black", size = 0.5),                        
        axis.line.y = element_line(color="black", size = 0.5),                        
        legend.position=c(0.95,0.8), 
        plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +  
  panel_border(colour = "black", size = 0.75, linetype = 1,                           
               remove = FALSE) + 
  scale_fill_manual(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                    values=c("#8660a5", "palevioletred1", "limegreen", "green4", "grey"), 
                    labels=c("Hard Coral", "CCA & Peyssonelia", "Turf Algae", "Fleshy Macroalgae", "Other")) +   
  scale_x_discrete(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                   labels=c("HC", "CCA", "TA", "MA", "OTH")) +
  guides(fill="none") +
  #expand_limits(y=c(-150, 800)
  scale_y_continuous(limits = c(-145, 1060), breaks = c(0, 200, 400, 600, 800, 1000))

figS7g

# # Print to pdfs
# pdf(file = "figures/spq/Fig4B_final.pdf", width = 3, height = 3.17)
# figS7g
# dev.off()
# 
# # Print to tiffs
# tiff(file = "figures/spq/Fig4B_final.tiff", width = 3, height = 3.17, units="in",res=300)
# figS7g
# dev.off()

#################
### panel h-l ###
#################

figS7h.l <- ggplot(benthic.change.heat.human, aes(x=substrate, y=percent.change,  group=substrate)) +     
  geom_bar(stat="identity", aes(fill=factor(substrate)), position=position_dodge(0.9)) +    
  geom_text(aes(label=ifelse((percent.change>0 & percent.change <1000),ceiling(percent.change),'')),vjust=-0.3) +
  geom_text(aes(label=ifelse(percent.change<0,ceiling(percent.change),'')),vjust=1.3) + 
  geom_text(aes(label=ifelse(percent.change>1000, ceiling(percent.change), ""), x =4, y = 1030), hjust = 1.5) +
  facet_wrap(~localdisturbance.categorical, nrow=1, scales="fixed") +
  labs(x="", y="", colour="", title="") +                                 
  theme_cowplot() +                                                       
  theme(strip.background=element_blank(),  strip.text = element_blank(),                
        legend.title=element_blank(), 
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),     
        axis.line.x = element_line(color="black", size = 0.5),                          
        axis.line.y = element_line(color="black", size = 0.5),                          
        legend.position="bottom", 
        plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +  
  panel_border(colour = "black", size = 0.75, linetype = 1,                             
               remove = FALSE) + 
  scale_fill_manual(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                    values=c("#8660a5", "palevioletred1", "limegreen", "green4", "grey"), 
                    labels=c("Hard Coral", "CCA & Peyssonelia", "Turf Algae", "Fleshy Macroalgae", "Other")) +   
  scale_x_discrete(breaks=c("HARD.CORAL", "CCA.PEY", "TURF.ALGAE", "FLESH.MACRO", "Other"),
                   labels=c("HC", "CCA", "TA", "MA", "OTH")) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000)) +
  coord_cartesian(ylim=c(-145, 1060))

figS7h.l

# # Print to pdfs
# pdf(file = "figures/spq/Fig4D_final.pdf", width = 12, height = 3.9)
# figS7h.l
# dev.off()
# 
#  # Print to tiffs
# tiff(file = "figures/spq/Fig4D_final.tiff", width = 12, height = 3,units="in",res=300)
# figS7h.l
# dev.off()
