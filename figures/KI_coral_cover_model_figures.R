
# Transformation of coral communities subjected to an unprecedented heatwave is modulated by local disturbance

# Authors: Julia K. Baum [1,2], Danielle C. Claar [1,3], Kristina L. Tietjen [1], Jennifer M.T. Magel [1,4],
# Dominique G. Maucieri [1], Kim M. Cobb [5], Jamie M. McDevitt-Irwin [1,6]

# [1] Department of Biology, University of Victoria, Victoria, BC V8P 5C2, Canada
# [2] Hawai'i Institute of Marine Biology, Kane'ohe, HI 96744, USA
# [3] School of Aquatic and Fisheries Sciences, University of Washington, Seattle, WA 98105, USA
# [4] Department of Forest and Conservation Sciences, University of British Columbia, Vancouver, BC V6T 1Z4, Canada
# [5] School of Earth and Atmospheric Sciences, Georgia Institute of Technology, Atlanta, GA 30318, USA
# [6] Hopkins Marine Station, Stanford University, Pacific Grove, CA 93950, USA

# Corresponding author: Julia K. Baum, email: baum@uvic.ca

# Script to create figures using data from the coral cover models


########################################################################################################################

## GENERAL SET-UP

## Load necessary packages
library(dplyr)
library(arm)
library(glmmTMB)
library(MuMIn)
library(performance)
library(DHARMa)
library(here)
library(forcats)
library(ggplot2)
library(cowplot)
library(beyonce)
library(ggeffects)

## Load the data
load(file = "../data/KI_CoralCoverData.RData")
load(file = "../data/KI_CoralCoverData_LH.RData")

## Clean the data
source("../analyses/KI_coral_cover_clean.R")

## Set plotting parameters
theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
            theme(axis.text = element_text(size = 16, color = "black"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 18, margin = margin(20,0,0,0)),
                  plot.title = element_text(size = 22, margin = margin(0,0,15,0)),
                  plot.margin = margin(10,10,10,20), 
                  strip.background = element_rect(fill = "black"),
                  strip.text = element_text(colour = "white", size = 12),
                  panel.border = element_rect(color = "black", fill = NA, size = 1),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank()))


########################################################################################################################

###############
## FIGURE 1B ##
###############

# Run the model
model1 <- glmmTMB(mean.cover ~ local_dist_z + poly(npp_max_z, 2) + mmm_temp_z + exposure, 
                  data = p.cover.b.avg, family = beta_family(link = "logit"))

# Output model coefficients
model1.coef <- data.frame(confint(model1, full = TRUE))
names(model1.coef)[names(model1.coef) == "X2.5.."] <- "LowerCI"
names(model1.coef)[names(model1.coef) == "X97.5.."] <- "UpperCI"

# Rename coefficients
model1.coef <- model1.coef[-c(1,7), ]
rownames(model1.coef)[1] <- "Dist."
rownames(model1.coef)[2] <- "NPP (l)"
rownames(model1.coef)[3] <- "NPP (q)"
rownames(model1.coef)[4] <- "Temp."
rownames(model1.coef)[5] <- "Expo."

model1.coef$Variable <- rownames(model1.coef)
model1.coef$Variable <- as.factor(model1.coef$Variable)

# Reorder variables
model1.coef <- model1.coef[c(5,4,3,2,1), ]

# Create plot
fig1b <- ggplot(model1.coef, aes(x = fct_inorder(Variable), y = Estimate)) + geom_hline(yintercept = 0, 
                                                                                        color = gray(1/2), lty = 2)
fig1b <- fig1b + geom_pointrange(aes(x = fct_inorder(Variable), y = Estimate, ymin = LowerCI, ymax = UpperCI),
                           position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1, fill = "black")
fig1b <- fig1b + coord_flip() + theme_cowplot() + xlab("") +  theme(axis.text = element_text(size = 18, color = "black"),
                                                              axis.title.y = element_blank(),
                                                              axis.title.x = element_text(size = 20, 
                                                                                          margin = margin(20,0,0,0)))
fig1b


## Print to a jpg
# jpeg(filename = "../figures/spq_model1_fig1b.jpg", width = 4.5, height = 6, units = "in", res = 600)
# fig1b
# dev.off()


########################################################################################################################

###############
## FIGURE 3B ##
###############

# Run the model
model2 <- glmmTMB(mean.cover ~ Heat * local_dist_z + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                  data = p.cover.ba.avg, family = beta_family(link = "logit"))

# Output model predictions
pred.2 <- ggpredict(model2, c("local_dist_z [all]", "Heat"))
names(pred.2)[names(pred.2) == "group"] <- "Heat"

# Set colour palette for plot
col.2 <- c("#654A89", "#C2B0D3")

# Create plot with predicted and observed values
fig3b <- ggplot() + 
  geom_line(data = pred.2, aes(x = x, y = predicted*100, color = Heat), size = 1) + 
  geom_ribbon(data = pred.2, aes(x = x, y = predicted*100, ymin = conf.low*100, ymax = conf.high*100, fill = Heat), alpha = 0.4) +
  xlab("Human Disturbance") + ylab("Percent Hard Coral Cover") +
  scale_fill_manual("Heat", values = col.2) + 
  scale_color_manual("Heat", values = col.2) +
  theme_classic() + theme(legend.position = c(0.83, 0.89)) +
  annotate(geom = "text", x = 1.195, y = 49, label = "italic(P)<0.001", parse = TRUE, color = "black", size = 7) +
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20), legend.title = element_text(size = 20))
fig3b <- fig3b + geom_point(data = p.cover.ba.avg, aes(x = local_dist_z, y = mean.cover*100, colour = Heat, fill = Heat), 
                            shape = 21, cex = 5, alpha = 0.7)
fig3b

## Print to a jpg
# jpeg(filename = "../figures/spq_model2_fig3b.jpg", width = 8, height = 6, units = "in", res = 400)
# fig3b
# dev.off()


########################################################################################################################

###############
## FIGURE 3C ##
###############

# Run the models
model2c <- glmmTMB(mean.cover ~ Heat * local_dist_z + poly(dhw_max_z, 2) + npp_max_z + (1|Site),
                   data = p.cover.comp.avg, ziformula = ~Heat, family = beta_family(link = "logit"))
model2s <- glmmTMB(mean.cover ~ Heat * local_dist_z + dhw_max_z + (1|Site),
                   data = p.cover.st.avg, family = beta_family(link = "logit"))

# Output model predictions
pred.2c <- ggpredict(model2c, c("local_dist_z [all]", "Heat"))
pred.2s <- ggpredict(model2s, c("local_dist_z [all]", "Heat"))

# Add columns for life history type
pred.2c$lh.type <- "Competitive"
pred.2s$lh.type <- "Stress-tolerant"

# Combine predictions into a single dataframe
pred.2cs <- rbind(pred.2c, pred.2s)
pred.2cs$heat.lh <- paste(pred.2cs$group, pred.2cs$lh.type, sep = ".")

# Set colour palette for plot
lhcolors <- beyonce_palette(60)
col.2cs <- lhcolors[c(1,9)]
col.2cs2 <- lhcolors[c(1,9,1,9)]
line.2cs <- c("solid", "dotted")

# Create plot with predicted values
fig3c <- ggplot(pred.2cs, aes(x = x, y = predicted*100, ymin = conf.low*100, ymax = conf.high*100)) + 
  geom_line(aes(linetype = group, color = lh.type), size = 1) + 
  geom_ribbon(aes(fill = heat.lh, color = NA), alpha = 0.3) +
  xlab("Human Disturbance") + ylab("Percent Hard Coral Cover") +
  scale_linetype_manual("Heat", values = line.2cs) +
  scale_color_manual("Life history", labels = c("Comp.", "Stress"), values = col.2cs) + 
  scale_fill_manual("Life history", values = col.2cs2) +
  theme_classic() + guides(color = guide_legend(), fill = "none") +
  theme(legend.position = c(0.85, 0.75)) + 
  annotate(geom = "text", x = 1.12, y = 18.5, label = "Comp: italic(P)<0.001", parse = TRUE, color = "black", size = 7) +
  annotate(geom = "text", x = 1.115, y = 16, label = expression(paste("Stress: ", italic('P'), " = 0.011")), 
           parse = TRUE, color = "black", size = 7) +
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20), legend.title = element_text(size = 20))
fig3c

## Print to a jpg
# jpeg(filename = "../figures/spq_model2cs_fig3c.jpg", width = 8, height = 6, units = "in", res = 400)
# fig3c
# dev.off()


########################################################################################################################

###############
## FIGURE S8 ##
###############

# Run the model
model3 <- glmmTMB(mean.cover ~ Heat * life.hist + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                  data = p.cover.ba.avg, family = beta_family(link = "logit"))

# Output predicted values
pred.3 <- ggpredict(model3, c("life.hist [all]", "Heat"))
names(pred.3)[names(pred.3) == "group"] <- "Heat"

# Create plot with predicted and observed values
figs8 <- ggplot(pred.3, aes(x = x, y = predicted*100)) +
  geom_point(data = p.cover.ba.avg, aes(x = life.hist, y = mean.cover*100, color = Heat, fill = Heat), 
             shape = 21, alpha = 0.6, cex = 3, 
             position = position_jitterdodge(dodge.width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low*100, ymax = conf.high*100, group = Heat), 
                width = 0, size = 0.7, position = position_dodge(0.5)) +
  geom_point(aes(fill = Heat), shape = 21, cex = 6, position = position_dodge(0.5)) +
  xlab("Life History") + ylab("Percent Hard Coral Cover") +
  scale_fill_manual("Heat", values = col.2) + scale_color_manual("Heat", values = col.2) +
  scale_x_discrete(labels = c("Stress-tolerant", "Mixed", "Competitive")) +
  theme_classic() +
  theme(axis.title = element_text(size = 22), axis.text = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18), legend.title = element_text(size = 18))
figs8

## Print to a jpg
# jpeg(filename = "../figures/spq_model3_figs8.jpg", width = 8, height = 6, units = "in", res = 400)
# figs8
# dev.off()

