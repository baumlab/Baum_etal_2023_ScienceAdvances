
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

# Script to perform multivariate analysis of the coral cover data (testing for differences in community structure across
# the local disturbance gradient and following the heatwave)


########################################################################################################################

## GENERAL SETUP and DATA CLEANING

# Load necessary packages
library(vegan)

# Load the data
load("../data/KI_MultivariateData.RData")
load("../data/KI_MultivariateMetaData.RData")

# Drop levels for low/high disturbance sites
site.sp.all.meta$Site_Number <- droplevels(site.sp.all.meta$Site_Number)

# Divide datasets into disturbance levels for betadisper
site.sp.all.vl <- site.sp.all[c(3,4,11:16), ]
site.sp.all.m <- site.sp.all[c(1,2,5:10,17,18,25:28), ]
site.sp.all.vh <- site.sp.all[c(19:24), ]

site.sp.all.meta.vl <- site.sp.all.meta[c(3,4,11:16), ]
site.sp.all.meta.m <- site.sp.all.meta[c(1,2,5:10,17,18,25:28), ]
site.sp.all.meta.vh <- site.sp.all.meta[c(19:24), ]

# Set seed for random processes
set.seed(9)


########################################################################################################################

## PERMANOVA

adonis(site.sp.all ~ Heat * f.pressure, data = site.sp.all.meta, permutations = 999, method = "bray", 
       strata = site.sp.all.meta$Site_Number)

