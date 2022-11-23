
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

# Script to produce PCoA plot (Fig. 3D) showing differences in coral community composition before/after the heatwave
# and across the local disturbance gradient


########################################################################################################################

## GENERAL SET-UP

# Load necessary packages
library(vegan)
library(here)

# Load the data
load(file = "../data/KI_MultivariateData.RData")
load(file = "../data/KI_MultivariateMetaData.RData")

# Create heat x disturbance variables
site.sp.all.meta$heat.dist <- paste(site.sp.all.meta$Heat, site.sp.all.meta$f.pressure, sep = ".")
site.sp.all.meta$heat.dist <- factor(site.sp.all.meta$heat.dist, 
                                        levels = c("Before.Very low", "After.Very low", "Before.Medium",
                                                   "After.Medium", "Before.Very high", "After.Very high"))

# Set seed for random processes
set.seed(9)

# Set colours for time points/disturbance levels
timecols <- c("#2A0BD9", "#BCD2EE", "#86F3FC", "#D7FCFC", "#A60021", "#EEB4B4")
palette(timecols)


########################################################################################################################

## PCoA PLOT

# Compute Bray-Curtis dissimilarity indices using the site x species matrix
dist.m <- vegdist(site.sp.all, method = "bray")

# Calculate multivariate dispersions
PCoA.m <- betadisper(d = dist.m, group = site.sp.all.meta$heat.dist, type = "centroid")

# Plot PCoA ordination
# jpeg(filename = "figure3D.jpg",
#      width = 150, height = 165, units = "mm", res = 600)

plot(PCoA.m, hull = F, label = F, cex = 1.5,
     main = NULL, col = timecols, pch = c(15,15,17,17,16,16),
     xaxt = "n", yaxt = "n", xlab = NULL, ylab = NULL, sub = "")
ordihull(PCoA.m, site.sp.all.meta$heat.dist,
         draw = c("polygon"), col = timecols, alpha = 0.6)
legend("topright", legend = c("B/VL", "B/M", "B/VH", "A/VL", "A/M", "A/VH"),
       col = c("#2A0BD9", "#86F3FC", "#A60021", "#BCD2EE", "#D7FCFC", "#EEB4B4"),
       pch = c(15,17,16,15,17,16), bty = "n", ncol = 2, cex = 1.2)

# dev.off()

