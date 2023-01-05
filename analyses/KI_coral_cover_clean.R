
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

# Script to prepare coral cover data for use in final models (fit using the KI_coral_cover_analyses.R script)
# percent.cover = dataset used to fit models in model sets A, B, and C (models 1, 2, and 3)
# percent.cover.lh = dataset used to fit models in model set B (models 2c/s)


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

# Expeditions: Four expeditions conducted prior to the heatwave (2013-2015b)
# Fixed effects: local human disturbance ('local_dist_z'; continuous), net primary productivity ('npp_max_z'; continuous), 
# site-level maximum mean monthly temperature ('mmm_temp_z'; continuous), and site exposure ('exposure'; categorical, 
# windward/sheltered)
# Random effects: none

##############

## Subset the data to only 'before' data points
p.cover.b <- p.cover[p.cover$Heat == "Before", ] # 39 data points

## Create separate data frame with one point per site (values averaged across expeditions)
p.cover.b.avg <- p.cover.b %>% dplyr::group_by(Site, fp.cont, f.pressure, dhw_max, npp_max, mmm_temp, exposure) %>%
  dplyr::summarise(mean.cover = mean(p.cover.beta)) # 19 data points
  
##############

## Standardize continuous variables
p.cover.b.avg$local_dist_z <- rescale(p.cover.b.avg$fp.cont)
p.cover.b.avg$dhw_max_z <- rescale(p.cover.b.avg$dhw_max)
p.cover.b.avg$npp_max_z <- rescale(p.cover.b.avg$npp_max)
p.cover.b.avg$mmm_temp_z <- rescale(p.cover.b.avg$mmm_temp)

########################################################################################################################

## MODEL SETS B & C

# Models testing the impact of heat stress (the 2015-2016 El Niño), local human disturbance (or dominant life history type),
# and other environmental factors on hard coral cover on Kiritimati.

## Model specifications:

# Expeditions: Four expeditions conducted prior to the heatwave (2013-2015) and two conducted after the heatwave (2016b, 2017)
# Fixed effects: heatwave period ('heat'; categorical, before/after), local human disturbance ('local_dist_z'; continuous), 
# dominant life history type ('life_hist'; categorical, competitive/stress-tolerant/mixed), site-level maximum degree heating 
# weeks ('dhw_max_z'; continuous), net primary productivity ('npp_max_z'; continuous)  
# Random effects: site, expedition (for models fit with all expeditions)

##############

## Subset the data to 'before' and 'after' data points
heat.ba <- c("Before", "After")
p.cover.ba <- p.cover[p.cover$Heat %in% heat.ba, ] # 67 data points
p.cover.lh.ba <- p.cover.lh[p.cover.lh$Heat %in% heat.ba, ] ## 201 data points

## Drop unnecessary factor levels
p.cover.ba$Heat <- droplevels(p.cover.ba$Heat)
p.cover.ba$Site <- droplevels(p.cover.ba$Site)
p.cover.ba$Field.Season <- droplevels(p.cover.ba$Field.Season)
p.cover.lh.ba$Heat <- droplevels(p.cover.lh.ba$Heat)
p.cover.lh.ba$Site <- droplevels(p.cover.lh.ba$Site)
p.cover.lh.ba$Field.Season <- droplevels(p.cover.lh.ba$Field.Season)

## Create separate data frame with one point per site (values averaged across expeditions)
p.cover.ba.avg <- p.cover.ba %>% dplyr::group_by(Site, Heat, fp.cont, f.pressure, life.hist, dhw_max, npp_max, mmm_temp, exposure) %>%
  dplyr::summarise(mean.cover = mean(p.cover.beta)) # 38 data points

## Separate data frames for competitive and stress-tolerant corals
p.cover.comp <- subset(p.cover.lh.ba, lh.type == "Competitive") # 67 data points
p.cover.st <- subset(p.cover.lh.ba, lh.type == "Stress_tolerant") # 67 data points

# One point per site
p.cover.comp.avg <- p.cover.comp %>% dplyr::group_by(Site, Heat, fp.cont, f.pressure, dhw_max, npp_max, mmm_temp, exposure) %>%
  dplyr::summarise(mean.cover = mean(p.cover.beta)) # 38 data points
p.cover.st.avg <- p.cover.st %>% dplyr::group_by(Site, Heat, fp.cont, f.pressure, dhw_max, npp_max, mmm_temp, exposure) %>%
  dplyr::summarise(mean.cover = mean(p.cover.beta)) # 38 data points


##############

## Standardize continuous variables
p.cover.ba.avg$local_dist_z <- rescale(p.cover.ba.avg$fp.cont)
p.cover.ba.avg$dhw_max_z <- rescale(p.cover.ba.avg$dhw_max)
p.cover.ba.avg$npp_max_z <- rescale(p.cover.ba.avg$npp_max)
p.cover.ba.avg$mmm_temp_z <- rescale(p.cover.ba.avg$mmm_temp)

p.cover.comp.avg$local_dist_z <- rescale(p.cover.comp.avg$fp.cont)
p.cover.comp.avg$dhw_max_z <- rescale(p.cover.comp.avg$dhw_max)
p.cover.comp.avg$npp_max_z <- rescale(p.cover.comp.avg$npp_max)
p.cover.comp.avg$mmm_temp_z <- rescale(p.cover.comp.avg$mmm_temp)

p.cover.st.avg$local_dist_z <- rescale(p.cover.st.avg$fp.cont)
p.cover.st.avg$dhw_max_z <- rescale(p.cover.st.avg$dhw_max)
p.cover.st.avg$npp_max_z <- rescale(p.cover.st.avg$npp_max)
p.cover.st.avg$mmm_temp_z <- rescale(p.cover.st.avg$mmm_temp)

