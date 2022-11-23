
# Transformation of coral communities subjected to an unprecedented heatwave is modulated by local disturbance

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


# Script to analyze tagged coral data

#<-------------------------------------------------->

# Clear environment
rm(list=ls())

# Load packages
library(here)
library(DHARMa)
library(performance)


# Load data
load(file="for_public_repo/data/KI_TaggedCoralData.RData")

# Look at dfs
head(tagged.data)
head(tagged.bleaching)

#<-------------------------------------------------->



# calculate mortality of P. grandis and M. aequituberculata at all sites

tagged.calc <- tagged.data[(tagged.data$species == "Pocillopora grandis" | tagged.data$species == "Montipora foliosa"),]
tagged.calc <- droplevels(tagged.calc)
head(tagged.calc)

competitive.status <-as.data.frame(table(tagged.calc$species, tagged.calc$status.after))
competitive.status

## by species
competitive.status$perc.mort <- NA
competitive.status$perc.mort <- ifelse(competitive.status$Var1 == "Pocillopora grandis", (competitive.status$Freq/100)*100, competitive.status$perc.mort)
competitive.status$perc.mort <- ifelse(competitive.status$Var1 == "Montipora foliosa", (competitive.status$Freq/101)*100, competitive.status$perc.mort)
competitive.status

## overall for competitive life history
competitive.status.calc <-as.data.frame(table(tagged.calc$status.after))
competitive.status.calc
competitive.status.calc$perc.mort <- (competitive.status.calc$Freq/201)*100
competitive.status.calc



#<-------------------------------------------------->

# Fit glm models for tagged corals across the disturbance gradient


###################
### All corals ###
###################


overall <- glm(status.after.numeric ~ localdisturbance.continuous_rescaled, data=tagged.data, family=binomial(link = "logit"))
summary(overall) 

### diagnostics

testDispersion(overall)

# Calculate and plot scaled residuals vs. fitted values (DHARMa)
overall.resid <- simulateResiduals(fittedModel = overall, plot = F)
plot(overall.resid)
hist(overall.resid)
testResiduals(overall.resid)
testZeroInflation(overall.resid)

# Plot residuals against other predictor values (DHARMa)
# Predictors in the model
plotResiduals(overall.resid, tagged.data$localdisturbance.continuous_rescaled)
# Predictors not in the model
plotResiduals(overall.resid, tagged.data$lifehistory)


# Plot residuals against predictor values (base)
plot(tagged.data$localdisturbance.continuous_rescaled, overall.resid$scaledResiduals)


#########################################
### Just stress-tolerant life history ###
#########################################

tagged.st <- tagged.data[tagged.data$lifehistory == "Stress tolerant", ]
tagged.st <-droplevels(tagged.st)

glm.st <- glm(status.after.numeric ~ localdisturbance.continuous_rescaled, data = tagged.st, family = quasibinomial(link = "logit"))
summary(glm.st)

#####################################
### Just competitive life history ###
#####################################

tagged.c <- tagged.data[!tagged.data$lifehistory == "Stress tolerant", ]
tagged.c <-droplevels(tagged.c)

glm.c <- glm(status.after.numeric ~ localdisturbance.continuous_rescaled, data = tagged.c, family = quasibinomial(link = "logit"))
summary(glm.c)

#######################
### Include species ###
#######################

species.glm <- glm(status.after.numeric ~ localdisturbance.continuous_rescaled * species, data=tagged.data, family=binomial(link = "logit"))
summary(species.glm)

### diagnostics

check_collinearity(species.glm)

testDispersion(species.glm)

# Calculate and plot scaled residuals vs. fitted values (DHARMa)
spmodel.resid <- simulateResiduals(fittedModel = species.glm, plot = F)
plot(spmodel.resid)
#residuals(bl.model.resid)
hist(spmodel.resid)
testResiduals(spmodel.resid)
testZeroInflation(spmodel.resid)

# Plot residuals against other predictor values (DHARMa)
# Predictors in the model
plotResiduals(spmodel.resid, tagged.data$localdisturbance.continuous_rescaled)
plotResiduals(spmodel.resid, tagged.data$species)

# Plot residuals against predictor values (base)
plot(tagged.data$localdisturbance.continuous_rescaled, spmodel.resid$scaledResiduals)


### Calculate model predictions

#### Platygyra
platygyra.range <- data.frame(localdisturbance.continuous_rescaled = c(-0.3589357, 1.8235100), species = c("Platygyra sp", "Platygyra sp"))
predict(species.glm, platygyra.range, type = "response")

#### Porites
porites.range <- data.frame(localdisturbance.continuous_rescaled = c(-0.3589357, 1.8235100), species = c("Porites lobata", "Porites lobata"))
predict(species.glm, porites.range, type = "response")

#### Hydnophora
hydnophora.range <- data.frame(localdisturbance.continuous_rescaled = c(-0.3589357, 1.8235100), species = c("Hydnophora microconos", "Hydnophora microconos"))
predict(species.glm, hydnophora.range, type = "response")




#<-------------------------------------------------->

# Fit a model to asses if early bleaching (July 2015) was a predictor of outcome

##############################
### Binary Bleaching Model ###
##############################

bleaching.model <- glm(status.after.numeric ~ species + binary.bleaching.2015c + island.region + localdisturbance.continuous_rescaled + species*binary.bleaching.2015c, data=tagged.bleaching, family=binomial(link = "logit"))
summary(bleaching.model)

### diagnostics

check_collinearity(bleaching.model)

testDispersion(bleaching.model)

# Calculate and plot scaled residuals vs. fitted values (DHARMa)
bl.model.resid <- simulateResiduals(fittedModel = bleaching.model, plot = F)
plot(bl.model.resid)
#residuals(bl.model.resid)
hist(bl.model.resid)
testResiduals(bl.model.resid)
testZeroInflation(bl.model.resid)

# Plot residuals against other predictor values (DHARMa)
# Predictors in the model
plotResiduals(bl.model.resid, tagged.bleaching$localdisturbance.continuous_rescaled)
plotResiduals(bl.model.resid, tagged.bleaching$species)
plotResiduals(bl.model.resid, tagged.bleaching$binary.bleaching.2015c)
plotResiduals(bl.model.resid, tagged.bleaching$island.region)

# Plot residuals against predictor values (base)
plot(tagged.bleaching$localdisturbance.continuous_rescaled, bl.model.resid$scaledResiduals)
