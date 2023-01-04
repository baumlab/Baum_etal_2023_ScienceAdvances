
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

## SUPPLEMENTARY MODELS

# Script to fit three sets of models examining the impacts of heat stress, local human disturbance, and other factors on
# levels of hard coral cover around Kiritimati:
# A) A generalized linear model (GLM) and generalized linear mixed-effects model (GLMM) examining influences on hard coral
# cover prior to the 2015-2016 El Niño
# B) GLMMs testing the interacting effects of heat stress and local human disturbance on hard coral cover
# C) GLMMs testing the interacting effects of heat stress and dominant life history type on hard coral cover

# Unlike the models in the main manuscript (which were fit using coral cover data averaged across expeditions within each
# heatwave period), these models were fit using one of two datasets:
# a) Data from only the largest sampling event in the 'before' and 'after' heatwave periods (before = July 2013, 
# after = July 2017)
# b) All data points from the 'before' and 'after' heatwave periods (i.e., data was not averaged across expeditions)


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
source("../analyses/KI_coral_cover_clean_supp.R")


########################################################################################################################

### MODEL SET A

# Generalized linear model testing the impact of local human disturbance and environmental factors on hard coral cover 
# on Kiritimati prior to the 2015-2016 El Niño. 

## Model specifications:

# Expeditions: Depends on the dataset (see above)
# Fixed effects: local human disturbance ('local_dist_z'; continuous), net primary productivity ('npp_max_z'; continuous), 
# site-level maximum mean monthly temperature ('mmm_temp_z'; continuous), and site exposure ('exposure'; categorical, 
# windward/sheltered)
# Random effects: site (for 'all data points' model)


##################################

### MODEL1

## 2013 data

# Fit model and output results
model1a <- glmmTMB(p.cover.beta ~ local_dist_z + poly(npp_max_z, 2) + mmm_temp_z + exposure,
                  data = p.cover.b.13, family = beta_family(link = "logit"))
summary(model1a)

# Assess model performance
model_performance(model1a)
check_model(model1a, check = c("pp_check", "vif"))

# Calculate and plot scaled residuals vs. fitted values
model1a.resid <- simulateResiduals(fittedModel = model1a)
plot(model1a.resid)

# Plot residuals against predictor variables
plotResiduals(model1a.resid, p.cover.b.13$local_dist_z)
plotResiduals(model1a.resid, p.cover.b.13$npp_max_z)
plotResiduals(model1a.resid, p.cover.b.13$mmm_temp_z)
plotResiduals(model1a.resid, p.cover.b.13$exposure)


###################

## All data points

# Fit the model and output results
model1b <- glmmTMB(p.cover.beta ~ local_dist_z + poly(npp_max_z, 2) + mmm_temp_z + exposure + (1|Site) + (1|Field.Season),
                   data = p.cover.b, family = beta_family(link = "logit"))
summary(model1b)

# Assess model performance
model_performance(model1b)
check_model(model1b, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model1b.resid <- simulateResiduals(fittedModel = model1b)
plot(model1b.resid)

# Plot residuals against predictor variables
plotResiduals(model1b.resid, p.cover.b$local_dist_z)
plotResiduals(model1b.resid, p.cover.b$npp_max_z)
plotResiduals(model1b.resid, p.cover.b$mmm_temp_z)
plotResiduals(model1b.resid, p.cover.b$exposure)


########################################################################################################################

### MODEL SET B

# Generalized linear mixed-effects models (GLMMs) testing the impact of heat stress (the 2015-2016 El Niño), local human 
# disturbance, and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Depends on the dataset (see above)
# Fixed effects: heatwave period ('heat'; categorical, before/after), local human disturbance ('local_dist_z'; continuous), 
# site-level maximum degree heating weeks ('dhw_max_z'; continuous), net primary productivity ('npp_max_z'; continuous)  
# Random effects: site (for all models), expedition (for 'all data points' models)


##################################

### MODEL2

## 2013/2017 data

# Fit model and output results
model2a <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                   data = p.cover.ba.1317, family = beta_family(link = "logit"))
summary(model2a)

# Assess model performance
model_performance(model2a)
check_model(model2a, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2a.resid <- simulateResiduals(fittedModel = model2a)
plot(model2a.resid)

# Plot residuals against predictor values
plotResiduals(model2a.resid, p.cover.ba.1317$Heat)
plotResiduals(model2a.resid, p.cover.ba.1317$local_dist_z)
plotResiduals(model2a.resid, p.cover.ba.1317$dhw_max_z)
plotResiduals(model2a.resid, p.cover.ba.1317$npp_max_z)


###################

## All data points

# Fit model and output results
model2b <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + dhw_max_z + poly(npp_max_z, 2) + (1|Site) + (1|Field.Season),
                   data = p.cover.ba, family = beta_family(link = "logit"))
summary(model2b)

# Assess model performance
model_performance(model2b)
check_model(model2b, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2b.resid <- simulateResiduals(fittedModel = model2b)
plot(model2b.resid)

# Plot residuals against predictor values
plotResiduals(model2b.resid, p.cover.ba$Heat)
plotResiduals(model2b.resid, p.cover.ba$local_dist_z)
plotResiduals(model2b.resid, p.cover.ba$dhw_max_z)
plotResiduals(model2b.resid, p.cover.ba$npp_max_z)


##################################

### MODEL2c (competitive corals)

## 2013/2017 data

# Fit model and output results
# Including a zero inflation parameter to deal with zeroes after the heat stress
model2ca <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + poly(dhw_max_z, 2) + npp_max_z + (1|Site),
                    data = p.cover.comp.1317, ziformula = ~Heat, family = beta_family(link = "logit"))
summary(model2ca)

# Assess model performance
model_performance(model2ca)
check_model(model2ca, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2ca.resid <- simulateResiduals(fittedModel = model2ca)
plot(model2ca.resid)

# Plot residuals against predictor values
plotResiduals(model2ca.resid, p.cover.comp.1317$Heat)
plotResiduals(model2ca.resid, p.cover.comp.1317$local_dist_z)
plotResiduals(model2ca.resid, p.cover.comp.1317$dhw_max_z)
plotResiduals(model2ca.resid, p.cover.comp.1317$npp_max_z)


###################

## All data points

# Model fit without (1|Field.Season), as inclusion of this random effect led to convergence problems.

# Fit model and output results
model2cb <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + poly(dhw_max_z, 2) + npp_max_z + (1|Site),
                    data = p.cover.comp, ziformula = ~Heat, family = beta_family(link = "logit"))
summary(model2cb)

# Assess model performance
model_performance(model2cb)
check_model(model2cb, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2cb.resid <- simulateResiduals(fittedModel = model2cb)
plot(model2cb.resid)

# Plot residuals against predictor values
plotResiduals(model2cb.resid, p.cover.comp$Heat)
plotResiduals(model2cb.resid, p.cover.comp$local_dist_z)
plotResiduals(model2cb.resid, p.cover.comp$dhw_max_z)
plotResiduals(model2cb.resid, p.cover.comp$npp_max_z)


##################################

### MODEL2s (stress-tolerant corals)

## 2013/2017 data

# Fit model and output results
model2sa <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + dhw_max_z + (1|Site),
                    data = p.cover.st.1317, family = beta_family(link = "logit"))
summary(model2sa)

# Assess model performance
model_performance(model2sa)
check_model(model2sa, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2sa.resid <- simulateResiduals(fittedModel = model2sa)
plot(model2sa.resid)

# Plot residuals against predictor values
plotResiduals(model2sa.resid, p.cover.st.1317$Heat)
plotResiduals(model2sa.resid, p.cover.st.1317$local_dist_z)
plotResiduals(model2sa.resid, p.cover.st.1317$dhw_max_z)


###################

## All data points

# Fit model and output results
model2sb <- glmmTMB(p.cover.beta ~ Heat * local_dist_z + dhw_max_z + (1|Site) + (1|Field.Season),
                    data = p.cover.st, family = beta_family(link = "logit"))
summary(model2sb)

# Assess model performance
model_performance(model2sb)
check_model(model2sb, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model2sb.resid <- simulateResiduals(fittedModel = model2sb)
plot(model2sb.resid)

# Plot residuals against predictor values
plotResiduals(model2sb.resid, p.cover.st$Heat)
plotResiduals(model2sb.resid, p.cover.st$local_dist_z)
plotResiduals(model2sb.resid, p.cover.st$dhw_max_z)


########################################################################################################################

### MODEL SET C

# Generalized linear mixed-effects models (GLMMs) testing the impact of heat stress (the 2015-2016 El Niño), dominant life
# history type, and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Depends on the dataset (see above)
# Fixed effects: heatwave period ('heat'; categorical, before/after), dominant life history type ('life_hist'; categorical,
# stress-tolerant/mixed/competitive), site-level maximum degree heating weeks ('dhw_max_z'; continuous), site-level 
# maximum net primary productivity ('npp_max_z'; continuous)    
# Random effects: site (for both models), expedition (for 'all data points' model)


##################################

### MODEL3

## 2013/2017 data

# Fit model and output results
model3a <- glmmTMB(p.cover.beta ~ Heat * life.hist + dhw_max_z + poly(npp_max_z, 2) + (1|Site),
                   data = p.cover.ba.1317, family = beta_family(link = "logit"))
summary(model3a)

# Assess model performance
model_performance(model3a)
check_model(model3a, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model3a.resid <- simulateResiduals(fittedModel = model3a)
plot(model3a.resid)

# Plot residuals against predictor values
plotResiduals(model3a.resid, p.cover.ba.1317$Heat)
plotResiduals(model3a.resid, p.cover.ba.1317$life.hist)
plotResiduals(model3a.resid, p.cover.ba.1317$dhw_max_z)
plotResiduals(model3a.resid, p.cover.ba.1317$npp_max_z)


###################

## All data points

# Fit model and output results
model3b <- glmmTMB(p.cover.beta ~ Heat * life.hist + dhw_max_z + poly(npp_max_z, 2) + (1|Site) + (1|Field.Season),
                   data = p.cover.ba, family = beta_family(link = "logit"))
summary(model3b)

# Assess model performance
model_performance(model3b)
check_model(model3b, check = c("pp_check", "vif", "reqq"))

# Calculate and plot scaled residuals vs. fitted values
model3b.resid <- simulateResiduals(fittedModel = model3b)
plot(model3b.resid)

# Plot residuals against predictor values
plotResiduals(model3b.resid, p.cover.ba$Heat)
plotResiduals(model3b.resid, p.cover.ba$life.hist)
plotResiduals(model3b.resid, p.cover.ba$dhw_max_z)
plotResiduals(model3b.resid, p.cover.ba$npp_max_z)

