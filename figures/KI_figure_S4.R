# this script is to create figure S4


# Load necessary libraries
library(here)
library(ggplot2)

# Load data
sitedata <- read.csv(here::here("data","KI_Monitoring_SiteData_Oct2020.csv")) 

#pdf(file = "figures/S4.pdf", width = 7, height = 5)

ggplot(sitedata, aes(x = Intersect_Pop_2km, y = Fishing_Buffer_10levels_Standardized)) + 
  geom_point(stat = "identity") + 
  labs(x = "Population within 2 km", y = "Subsistence fishing pressure") + 
  theme_classic()

#dev.off()