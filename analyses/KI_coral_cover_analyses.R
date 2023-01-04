
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

# Script to fit three sets of models examining the impacts of heat stress, local human disturbance, and other factors 
# on levels of hard coral cover around Kiritimati:
# A) A generalized linear model (GLM) examining influences on hard coral cover prior to the 2015-2016 El Niño
# B) Generalized linear mixed-effects models (GLMMs) testing the interacting effects of heat stress and local human 
# disturbance on hard coral cover
# C) A GLMM testing the interacting effect of heat stress and dominant life history type on hard coral cover


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
library(Rmisc)
library(reshape2)
library(emmeans)

## Load the data
load(file = "../data/KI_CoralCoverData.RData")
load(file = "../data/KI_CoralCoverData_LH.RData")

## Clean the data
source("../analyses/KI_coral_cover_clean.R")


########################################################################################################################

### MODEL SET A

# Generalized linear model testing the impact of local human disturbance and environmental factors on hard coral cover 
# on Kiritimati prior to the 2015-2016 El Niño. 

## Model specifications:

# Expeditions: Four expeditions conducted prior to the heatwave (2013-2015b)
# Fixed effects: local human disturbance ('local_dist_z'; continuous), net primary productivity ('npp_max_z'; continuous), 
# site-level maximum mean monthly temperature ('mmm_temp_z'; continuous), and site exposure ('exposure'; categorical, 
# windward/sheltered)
# Random effects: none


##################################

### MODEL1

# Fit model and output results
model1 <- glmmTMB(mean.cover ~ local_dist_z + poly(npp_max_z, 2) + mmm_temp_z + exposure,
                  data = p.cover.b.avg, family = beta_family(link = "logit"))
summary(model1)

# Assess model performance
model_performance(model1)
check_model(model1, check = c("pp_check", "vif"))

# Calculate and plot scaled residuals vs. fitted values
model1.resid <- simulateResiduals(fittedModel = model1)
plot(model1.resid)

# Plot residuals against predictor variables
plotResiduals(model1.resid, p.cover.b.avg$local_dist_z)
plotResiduals(model1.resid, p.cover.b.avg$npp_max_z)
plotResiduals(model1.resid, p.cover.b.avg$mmm_temp_z)
plotResiduals(model1.resid, p.cover.b.avg$exposure)


########################################################################################################################

### MODEL SET B

# Generalized linear mixed-effects models (GLMMs) testing the impact of heat stress (the 2015-2016 El Niño), local human 
# disturbance, and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Four expeditions conducted prior to the heatwave (2013-2015) and two conducted after the heatwave (2016b, 2017)
# Fixed effects: heatwave period ('heat'; categorical, before/after), local human disturbance ('local_dist_z'; continuous), 
# site-level maximum degree heating weeks ('dhw_max_z'; continuous), net primary productivity ('npp_max_z'; continuous)  
# Random effects: site


##################################

### MODEL2

# Fit model and output results
model2 <- glmmTMB(mean.cover ~ Heat * local_dist_z + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                  data = p.cover.ba.avg, family = beta_family(link = "logit"))
summary(model2)

# Assess model performance
model_performance(model2)
check_model(model2, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2.resid <- simulateResiduals(fittedModel = model2)
plot(model2.resid)

# Plot residuals against predictor values
plotResiduals(model2.resid, p.cover.ba.avg$Heat)
plotResiduals(model2.resid, p.cover.ba.avg$local_dist_z)
plotResiduals(model2.resid, p.cover.ba.avg$dhw_max_z)
plotResiduals(model2.resid, p.cover.ba.avg$npp_max_z)


##################################

### MODEL2c (competitive corals)

# Fit model and output results
# Including a zero inflation parameter to deal with zeroes after the heat stress
model2c <- glmmTMB(mean.cover ~ Heat * local_dist_z + poly(dhw_max_z, 2) + npp_max_z + (1|Site),
                   data = p.cover.comp.avg, ziformula = ~Heat, family = beta_family(link = "logit"))
summary(model2c)

# Assess model performance
model_performance(model2c)
check_model(model2c, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2c.resid <- simulateResiduals(fittedModel = model2c)
plot(model2c.resid)

# Plot residuals against predictor values
plotResiduals(model2c.resid, p.cover.comp.avg$Heat)
plotResiduals(model2c.resid, p.cover.comp.avg$local_dist_z)
plotResiduals(model2c.resid, p.cover.comp.avg$dhw_max_z)
plotResiduals(model2c.resid, p.cover.comp.avg$npp_max_z)


##################################

### MODEL2s (stress-tolerant corals)

# Fit model and output results
model2s <- glmmTMB(mean.cover ~ Heat * local_dist_z + dhw_max_z + (1|Site),
                   data = p.cover.st.avg, family = beta_family(link = "logit"))
summary(model2s)

# Assess model performance
model_performance(model2s)
check_model(model2s, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2s.resid <- simulateResiduals(fittedModel = model2s)
plot(model2s.resid)

# Plot residuals against predictor values
plotResiduals(model2s.resid, p.cover.st.avg$Heat)
plotResiduals(model2s.resid, p.cover.st.avg$local_dist_z)
plotResiduals(model2s.resid, p.cover.st.avg$dhw_max_z)


########################################################################################################################

### MODEL SET C

# Generalized linear mixed-effects model (GLMM) testing the impact of heat stress (the 2015-2016 El Niño), dominant life
# history type, and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Four expeditions conducted prior to the heatwave (2013-2015) and two conducted after the heatwave (2016b, 2017)
# Fixed effects: heatwave period ('heat'; categorical, before/after), dominant life history type ('life_hist'; categorical,
# stress-tolerant/mixed/competitive), site-level maximum degree heating weeks ('dhw_max_z'; continuous), site-level 
# maximum net primary productivity ('npp_max_z'; continuous)    
# Random effects: site


##################################

### MODEL3

model3 <- glmmTMB(mean.cover ~ Heat * life.hist + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                  data = p.cover.ba.avg, family = beta_family(link = "logit"))
summary(model3)

# Assess model performance
model_performance(model3)
check_model(model3, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model3.resid <- simulateResiduals(fittedModel = model3)
plot(model3.resid)

# Plot residuals against predictor values
plotResiduals(model3.resid, p.cover.ba.avg$Heat)
plotResiduals(model3.resid, p.cover.ba.avg$life.hist)
plotResiduals(model3.resid, p.cover.ba.avg$dhw_max_z)
plotResiduals(model3.resid, p.cover.ba.avg$npp_max_z)


########################################################################################################################

### ADDITIONAL CALCULATIONS

# Calculating model predictions, percent change, and other values for use in the manuscript text.

##################################

### MODEL 2

## MODEL PREDICTIONS

# Output model predictions
model2.pred <- predict(model2, p.cover.ba.avg, type = c("response"))

# Create new data frame with coral cover data and model predictions
p.cover.ba.avg.pred <- cbind(p.cover.ba.avg, model2.pred)
colnames(p.cover.ba.avg.pred)[15] <- "model2.pred"

# Subset the data (eliminate unnecessary variables, select 'very low' and 'very high' disturbance sites)
p.cover.ba.avg.pred.sub <- subset(p.cover.ba.avg.pred, 
                                  select = c("f.pressure", "Site", "Heat", "local_dist_z", "model2.pred"))
p.cover.ba.avg.pred.sub <- p.cover.ba.avg.pred.sub[(p.cover.ba.avg.pred.sub$f.pressure == "Very low" |
                                                      p.cover.ba.avg.pred.sub$f.pressure == "Very high"), ]
p.cover.ba.avg.pred.sub <- droplevels(p.cover.ba.avg.pred.sub)

# Calculate mean coral cover values
summarySE(p.cover.ba.avg.pred.sub, measurevar = "model2.pred", groupvars = c("f.pressure", "Heat"))

##################

## PERCENT CHANGE

## Calculate percent change from model predictions
# Average across expeditions within each heatwave period
pchange.model2 <- summarySE(p.cover.ba.avg.pred.sub, measurevar = "model2.pred", 
                            groupvars = c("f.pressure", "Site", "Heat", "local_dist_z"))

# Widen data
pchange.model2.w <- dcast(pchange.model2, f.pressure + Site + local_dist_z ~ Heat, 
                          value.var = "model2.pred", FUN = sum, fill = NA)

# Create a column for percent change and calculate as [(% substrate cover AFTER)-(% substrate cover BEFORE)]/(% substrate cover BEFORE)*100
pchange.model2.w$percent.change <- ((pchange.model2.w$After-pchange.model2.w$Before)/pchange.model2.w$Before)*100

# Remove site 5 (only sampled in the 'after' timepoint, so percent change is NA)
pchange.model2.w <- pchange.model2.w[!pchange.model2.w$Site == "site5", ]

# Calculate percent change
pchange.model2.mean <- summarySE(pchange.model2.w, measurevar = "percent.change", groupvars = "f.pressure")
pchange.model2.mean

##################

## T-TEST

# Test for significant difference in percent change for 'very low' and 'very high' sites
t.test(percent.change ~ f.pressure, pchange.model2.w)

##################

## SLOPE

# Calculate slope of the relationship between coral cover and disturbance for 'before' and 'after' categories
emtrends(model2, ~Heat, var = "local_dist_z")


##################################

### MODEL 2C

## MODEL PREDICTIONS

# Output model predictions
model2c.pred <- predict(model2c, p.cover.comp.avg, type = c("response"))

# Create new data frame with coral cover data and model predictions
p.cover.comp.avg.pred <- cbind(p.cover.comp.avg, model2c.pred)
colnames(p.cover.comp.avg.pred)[14] <- "model2c.pred"

# Subset the data (eliminate unnecessary variables)
p.cover.comp.avg.pred.sub <- subset(p.cover.comp.avg.pred, 
                                    select = c("Site", "f.pressure", "Heat", "local_dist_z", "model2c.pred"))

# Calculate mean coral cover values
model2c.sitemean <- summarySE(p.cover.comp.avg.pred.sub, measurevar = "model2c.pred", 
                              groupvars =  c("Heat", "f.pressure"))
summarySE(model2c.sitemean, measurevar = "model2c.pred", groupvars =  "Heat")

##################

## PERCENT CHANGE

## Calculate percent change from model predictions
# Average across expeditions within each heatwave period
pchange.model2c <- summarySE(p.cover.comp.avg.pred.sub, measurevar = "model2c.pred", 
                             groupvars = c("f.pressure", "Site", "Heat", "local_dist_z"))

# Widen data
pchange.model2c.w <- dcast(pchange.model2c, f.pressure + Site + local_dist_z ~ Heat, 
                           value.var = "model2c.pred", FUN = sum, fill = NA)

# Create a column for percent change and calculate as [(% substrate cover AFTER)-(% substrate cover BEFORE)]/(% substrate cover BEFORE)*100
pchange.model2c.w$percent.change <- ((pchange.model2c.w$After - pchange.model2c.w$Before)/pchange.model2c.w$Before)*100

# Remove site 5 (only sampled in the 'after' timepoint, so percent change is NA)
pchange.model2c.w <- pchange.model2c.w[!pchange.model2c.w$Site == "site5", ]

# Calculate percent change
pchange.model2c.mean <- summarySE(pchange.model2c.w, measurevar = "percent.change")
pchange.model2c.mean


##################################

### MODEL 2S

## MODEL PREDICTIONS

# Output model predictions
model2s.pred <- predict(model2s, p.cover.st.avg, type = c("response"))

# Create new data frame with coral cover data and model predictions
p.cover.st.avg.pred <- cbind(p.cover.st.avg, model2s.pred)
colnames(p.cover.st.avg.pred)[14] <- "model2s.pred"

# Subset the data 
p.cover.st.avg.pred.sub <- subset(p.cover.st.avg.pred, 
                                  select = c("f.pressure", "Site", "Heat", "local_dist_z", "model2s.pred"))

# Calculate mean coral cover values
model2s.sitemean <- summarySE(p.cover.st.avg.pred.sub, measurevar = "model2s.pred", 
                              groupvars =  c("Heat", "f.pressure"))
summarySE(model2s.sitemean, measurevar = "model2s.pred", groupvars =  "Heat")

##################

## PERCENT CHANGE

## Calculate percent change from model predictions
# Average across time points within each heatwave period
pchange.model2s <- summarySE(p.cover.st.avg.pred.sub, measurevar = "model2s.pred", 
                             groupvars = c("f.pressure", "Site", "Heat", "local_dist_z"))

# Widen data
pchange.model2s.w <- dcast(pchange.model2s, f.pressure + Site + local_dist_z ~ Heat, 
                           value.var = "model2s.pred", FUN = sum, fill = NA)

# Create a column for percent change and calculate as [(% substrate cover AFTER)-(% substrate cover BEFORE)]/(% substrate cover BEFORE)*100
pchange.model2s.w$percent.change <- ((pchange.model2s.w$After - pchange.model2s.w$Before)/pchange.model2s.w$Before)*100

# Remove site 5 (only sampled in the 'after' timepoint, so percent change is NA)
pchange.model2s.w <- pchange.model2s.w[!pchange.model2s.w$Site == "site5", ]

# Calculate percent change
pchange.model2s.mean <- summarySE(pchange.model2s.w, measurevar = "percent.change")
pchange.model2s.mean

##################

## T-TEST

## Create new data frame with percent change values for competitive and stress-tolerant corals
# Remove unnecessary variables
pchange.model2c.w <- pchange.model2c.w[,-c(4,5)]

# Create combined 'f.pressure' and 'site' variable
pchange.model2c.w$fp.site <- paste(pchange.model2c.w$f.pressure, pchange.model2c.w$Site, sep = ".")
pchange.model2s.w$fp.site <- paste(pchange.model2s.w$f.pressure, pchange.model2s.w$Site, sep = ".")

# Create new data frame
pchange.lh <- pchange.model2c.w
pchange.lh$st.pc <- pchange.model2s.w$percent.change[match(pchange.lh$fp.site, pchange.model2s.w$fp.site)]
pchange.lh <- pchange.lh[,-5]
names(pchange.lh) <- c("f.pressure", "Site", "local_dist_z", "Comp.pc", "Stress.pc")
pchange.lh.l <- melt(pchange.lh, value.name = "percentchange", id.vars = c("f.pressure", "Site", "local_dist_z"))

## Test to see if percent change for competitive and stress-tolerant corals is significantly different
t.test(percentchange ~ variable, pchange.lh.l)

