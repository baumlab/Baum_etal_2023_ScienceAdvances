
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

# Script to prepare coral cover data for use in supplementary models (fit using the KI_coral_cover_analyses_supp.R script)
# percent.cover = dataset used to fit models in model sets A, B, and C (models 1, 2, and 3)
# percent.cover.lh = dataset used to fit models in model set B (models 2c/s)

# Two different datasets are created for the supplementary models:
# a) Data from only the largest sampling event in the 'before' and 'after' heatwave periods (before = Jul 2013, after = 
# Jul 2017)
# b) All data points in the 'before' and 'after' heatwave periods (i.e., data was not averaged across expeditions; before =
# Jul 2013, Aug 2014, Jan 2015, May 2015; after = Nov 2016, Jul 2017)


########################################################################################################################

## GENERAL DATA CLEANING

# Rename data frames
p.cover <- percent.cover
p.cover.lh <- percent.cover.lh

# Re-order local disturbance variable
p.cover$f.pressure <- factor(p.cover$f.pressure, 
                                   levels = c("Very low", "Low", "Medium", "High", "Very high"))
p.cover.lh$f.pressure <- factor(p.cover.lh$f.pressure, 
                                      levels = c("Very low", "Low", "Medium", "High", "Very high"))

# Re-order life history variable so that 'stress-tolerant' is the default
p.cover$life.hist <- factor(p.cover$life.hist, 
                                  levels = c("Stress_tolerant", "Mixed", "Competitive"))

########################################################################################################################

## MODEL SET A

# Models testing the impact of local human disturbance and environmental factors on hard coral cover on Kiritimati prior 
# to the 2015-2016 El Niño.

## Model specifications:

# Expeditions: Depends on the dataset (see above)
# Fixed effects: local human disturbance ('local_dist_z'; continuous), net primary productivity ('npp_max_z'; continuous), 
# site-level maximum mean monthly temperature ('mmm_temp_z'; continuous), and site exposure ('exposure'; categorical, 
# windward/sheltered)
# Random effects: none

##############

## Subset the data to only 'before' data points
p.cover.b <- p.cover[p.cover$Heat == "Before", ] # 39 data points

## Create separate data frame with only 2013 data
p.cover.b.13 <- p.cover.b[p.cover.b$Field.Season == "KI2013", ] # 17 data points

##############

## Standardize continuous variables
# 2013/2017 data
p.cover.b.13$local_dist_z <- rescale(p.cover.b.13$fp.cont)
p.cover.b.13$npp_max_z <- rescale(p.cover.b.13$npp_max)
p.cover.b.13$mmm_temp_z <- rescale(p.cover.b.13$mmm_temp)
p.cover.b.13$dhw_max_z <- rescale(p.cover.b.13$dhw_max)

# All data points
p.cover.b$local_dist_z <- rescale(p.cover.b$fp.cont)
p.cover.b$npp_max_z <- rescale(p.cover.b$npp_max)
p.cover.b$mmm_temp_z <- rescale(p.cover.b$mmm_temp)
p.cover.b$dhw_max_z <- rescale(p.cover.b$dhw_max)


########################################################################################################################

## MODEL SETS B & C

# Models testing the impact of heat stress (the 2015-2016 El Niño), local human disturbance (or dominant life history type),
# and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Depends on the dataset (see above)
# Fixed effects: heatwave period ('heat'; categorical, before/after), local human disturbance ('local_dist_z'; continuous), 
# dominant life history type ('life_hist'; categorical, competitive/stress-tolerant/mixed), site-level maximum degree heating 
# weeks ('dhw_max_z'; continuous), net primary productivity ('npp_max_z'; continuous)  
# Random effects: site, expedition (for 'all data points' models)

##############

## Subset the data
# Data frame with only 2013 and 2017 surveys
ba.1317 <- c("KI2013", "KI2017")
p.cover.ba.1317 <- p.cover[p.cover$Field.Season %in% ba.1317, ] # 35 data points
p.cover.lh.ba.1317 <- p.cover.lh[p.cover.lh$Field.Season %in% ba.1317, ] # 105 data points

# Data frame with 'before' and 'after' field seasons
heat.ba <- c("Before", "After")
p.cover.ba <- p.cover[p.cover$Heat %in% heat.ba, ] # 67 data points
p.cover.lh.ba <- p.cover.lh[p.cover.lh$Heat %in% heat.ba, ] # 201 data points

## Drop unnecessary factor levels
p.cover.ba.1317$Heat <- droplevels(p.cover.ba.1317$Heat)
p.cover.ba.1317$Site <- droplevels(p.cover.ba.1317$Site)
p.cover.ba.1317$Field.Season <- droplevels(p.cover.ba.1317$Field.Season)
p.cover.lh.ba.1317$Heat <- droplevels(p.cover.lh.ba.1317$Heat)
p.cover.lh.ba.1317$Site <- droplevels(p.cover.lh.ba.1317$Site)
p.cover.lh.ba.1317$Field.Season <- droplevels(p.cover.lh.ba.1317$Field.Season)

p.cover.ba$Heat <- droplevels(p.cover.ba$Heat)
p.cover.ba$Site <- droplevels(p.cover.ba$Site)
p.cover.ba$Field.Season <- droplevels(p.cover.ba$Field.Season)
p.cover.lh.ba$Heat <- droplevels(p.cover.lh.ba$Heat)
p.cover.lh.ba$Site <- droplevels(p.cover.lh.ba$Site)
p.cover.lh.ba$Field.Season <- droplevels(p.cover.lh.ba$Field.Season)

# Separate data frames for competitive and stress-tolerant corals
p.cover.comp <- subset(p.cover.lh.ba, lh.type == "Competitive") # 67 data points
p.cover.st <- subset(p.cover.lh.ba, lh.type == "Stress_tolerant") # 67 data points

p.cover.comp.1317 <- subset(p.cover.lh.ba.1317, lh.type == "Competitive") # 35 data points
p.cover.st.1317 <- subset(p.cover.lh.ba.1317, lh.type == "Stress_tolerant") # 35 data points


##############

## Standardize continuous variables
# 2013/2017 data
p.cover.ba.1317$local_dist_z <- rescale(p.cover.ba.1317$fp.cont)
p.cover.ba.1317$dhw_max_z <- rescale(p.cover.ba.1317$dhw_max)
p.cover.ba.1317$npp_max_z <- rescale(p.cover.ba.1317$npp_max)
p.cover.ba.1317$mmm_temp_z <- rescale(p.cover.ba.1317$mmm_temp)

p.cover.comp.1317$local_dist_z <- rescale(p.cover.comp.1317$fp.cont)
p.cover.comp.1317$dhw_max_z <- rescale(p.cover.comp.1317$dhw_max)
p.cover.comp.1317$npp_max_z <- rescale(p.cover.comp.1317$npp_max)
p.cover.comp.1317$mmm_temp_z <- rescale(p.cover.comp.1317$mmm_temp)

p.cover.st.1317$local_dist_z <- rescale(p.cover.st.1317$fp.cont)
p.cover.st.1317$dhw_max_z <- rescale(p.cover.st.1317$dhw_max)
p.cover.st.1317$npp_max_z <- rescale(p.cover.st.1317$npp_max)
p.cover.st.1317$mmm_temp_z <- rescale(p.cover.st.1317$mmm_temp)

# All data points
p.cover.ba$local_dist_z <- rescale(p.cover.ba$fp.cont)
p.cover.ba$dhw_max_z <- rescale(p.cover.ba$dhw_max)
p.cover.ba$npp_max_z <- rescale(p.cover.ba$npp_max)
p.cover.ba$mmm_temp_z <- rescale(p.cover.ba$mmm_temp)

p.cover.comp$local_dist_z <- rescale(p.cover.comp$fp.cont)
p.cover.comp$dhw_max_z <- rescale(p.cover.comp$dhw_max)
p.cover.comp$npp_max_z <- rescale(p.cover.comp$npp_max)
p.cover.comp$mmm_temp_z <- rescale(p.cover.comp$mmm_temp)

p.cover.st$local_dist_z <- rescale(p.cover.st$fp.cont)
p.cover.st$dhw_max_z <- rescale(p.cover.st$dhw_max)
p.cover.st$npp_max_z <- rescale(p.cover.st$npp_max)
p.cover.st$mmm_temp_z <- rescale(p.cover.st$mmm_temp)
