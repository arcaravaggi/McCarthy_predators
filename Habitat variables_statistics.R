#### McCarthy et al., 2020 ####
#### Predator Habitat Variables statistics ####
## April 2020 ##

# Clear R's memory
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load in packages
library(dplyr)
library(ggplot2)

# Load in data
pred_veg <- read.csv("Predator vegetation survey data_cleaned.csv")


# For each of the following sections, tidy the data, then merge into one at the end:
# Species cover
# Vegetation height
# Moisture
# Tree distance & height
# VOR
# Aerial visibility

# At the end, merge all vegetation data by site and station (10m, 50m, 100m)

names(pred_veg)

#### Species Cover ####
# Select the relevant columns
spp_cov <- pred_veg[,c(2:3,8:11)]

# Delete rows with NAs
spp_cov <- spp_cov[complete.cases(spp_cov),]

# Make new column of distance, direction and species merged
spp_cov$dist_spp <- paste(spp_cov[,5], spp_cov[,3], spp_cov[,4], sep="_")

# Delete distance, direction and species columns
spp_cov <- spp_cov[,-c(3:5)]

# Make those species as column headings and rotate the % cover values into the right cells
# Pivot table it with excel and fill in the missing columns
# Write the csv to do this
#write.csv(spp_cov, "spp_cov.csv")

## Read in the csv of reconfigured data
# Obtained averages of each species in excel
# Original columns still there
spp_cov_re <- read.csv("spp_cov_reconfig.csv")

# Filter out the columns with just the averages
names(spp_cov_re)
spp_cov_av <- spp_cov_re[,c(1,2,15,28,41,54,67,80,93,106,119,132,145,158,171,184,197,210,223,236,249)]

# Create composite columns
names(spp_cov_av)
spp_cov_av$gra.for <- spp_cov_av$Gr_fo + spp_cov_av$Greater_willowherb + spp_cov_av$Rosebay_willowherb +
  spp_cov_av$Tormentil
spp_cov_av$bar.mos <- spp_cov_av$Bare_ground + spp_cov_av$Moss
spp_cov_av$hea.bil <- spp_cov_av$Heather + spp_cov_av$Bilberry
spp_cov_av$gor.bra <- spp_cov_av$Bramble + spp_cov_av$Fuscia + spp_cov_av$Gorse + spp_cov_av$Holly
spp_cov_av$rus.woo <- spp_cov_av$Rushes + spp_cov_av$Woodrush
spp_cov_av$con <- spp_cov_av$Lodgepole + spp_cov_av$Sitka

# Ferns, willow and dead wood left over as on their own
# Delete individual columns that have been combined
names(spp_cov_av)
spp_cov_av <- spp_cov_av[,-c(3:5,8:19,21)]


#### Vegetation Height ####

# Select relevant columns to height
spp_hgt <- pred_veg[, c(2:3,8,15)]
# Delete rows with NAs
spp_hgt <- spp_hgt[complete.cases(spp_hgt),]
# Calculate means across all columns for each site & station combo
spp_hgt <- spp_hgt %>% group_by(Site, Station) %>% summarise_all(funs(mean, sd))


#### Moisture ####
# Select relevant columns
moi <- pred_veg[, c(2:3,12)]
# Delete NAs
moi <- moi[complete.cases(moi),]
# Obtain mean and sd for each site/station combo
moi <- moi %>% group_by(Site, Station) %>% summarise_all(funs(mean, sd))
# Header names set as "mean" and "sd", so I'll have to add vor to it
names(moi) <- c("Site", "Station","moi_mean", "moi_sd")


#### Tree Distance & Height ####
# Select relevant columns
tr_dis_hgt <- pred_veg[,c(2:3,17,19)]
# Delete NAs
tr_dis_hgt <- tr_dis_hgt[complete.cases(tr_dis_hgt),]
# Obtain mean and sd for each site/station combo
tr_dis_hgt <- tr_dis_hgt %>% group_by(Site, Station) %>% summarise_all(funs(mean, sd))


#### VOR ####
# Select relevant columns
vor <- pred_veg[,c(2:3,24)]
# Delete NAs
vor <- vor[complete.cases(vor),]
# Obtain mean and sd for each site/station combo
vor <- vor %>% group_by(Site, Station) %>% summarise_all(funs(mean, sd))
# Header names set as "mean" and "sd", so add vor to it
names(vor) <- c("Site", "Station","vor_mean", "vor_sd")


#### Aerial Visibility ####
# Select relevant columns
aer <- pred_veg[,c(2:3, 26)]
# Delete NAs
aer <- aer[complete.cases(aer),]
# Obtain mean and sd for each site/station combo
aer <- aer %>% group_by(Site, Station) %>% summarise_all(funs(mean, sd))
# Header names set as "mean" and "sd", so add vor to it
names(aer) <- c("Site", "Station","aer_mean", "aer_sd")



#### Merging together ####

# Merge dataframes by specific columns
pred_veg.a <- merge(spp_hgt,tr_dis_hgt, by = c("Site", "Station"))
pred_veg.b <- merge(moi, aer, by = c("Site", "Station"))
pred_veg.c <- merge(pred_veg.a, pred_veg.b, by = c("Site", "Station"))
pred_veg.d <- merge(pred_veg.c, vor, by = c("Site", "Station"))
pred_veg.e <- merge(pred_veg.d, spp_cov_av, by = c("Site", "Station"))

# Remove SD columns for PCA
# And the distance column
names(pred_veg.e)
pred_veg_complete <- pred_veg.e[,-c(3,5:6,9:10,12,14,16)]
names(pred_veg_complete)

# Delete rows of two sites that are excluded from the camera trap data
# due to camera failure at 50m stations
# BG1 and BT1
pred_veg_complete <- pred_veg_complete[!(pred_veg_complete$Site %in% c("BG1", "BT1")),]

# Moisture mean is the same for every station when BG1 and BT1 are removed
# This can be removed
pred_veg_complete$moi_mean <- NULL




#### Statistical Comparisons ####
## Check to see if any of the habitat variables differ between distance bands ##

# Load package for Kruskal-Wallis test
library(PMCMRplus)



## Vegetation height
hist(pred_veg_complete$Height_mean)
# Transform
pred_veg_complete$log_Height_mean <- log(pred_veg_complete$Height_mean)
hist(pred_veg_complete$log_Height_mean)
shapiro.test(pred_veg_complete$log_Height_mean)
# ANOVA
aov_height <- aov(log_Height_mean ~ Station, data = pred_veg_complete)
summary(aov_height)



## Distance to nearest tree
hist(pred_veg_complete$Dist...tree._mean)
# Transform
pred_veg_complete$log_dist_tree_mean <- log(pred_veg_complete$Dist...tree._mean)
hist(pred_veg_complete$log_dist_tree_mean)
shapiro.test(pred_veg_complete$log_dist_tree_mean)
# Data are still not normally distributed
# Use non-parametric test instead
kruskal.test(Dist...tree._mean ~ Station, data = pred_veg_complete)



## Tree height
hist(pred_veg_complete$Tree.hgt_mean)
shapiro.test(pred_veg_complete$Tree.hgt_mean)
# Transform
pred_veg_complete$log_Tree.hgt_mean <- log(pred_veg_complete$Tree.hgt_mean)
shapiro.test(pred_veg_complete$log_Tree.hgt_mean)
aov_tree_height <- aov(log_Tree.hgt_mean ~ Station, data = pred_veg_complete)
summary(aov_tree_height)



## Aerial visibility
hist(pred_veg_complete$aer_mean)
shapiro.test(pred_veg_complete$aer_mean)
# Transform
pred_veg_complete$log_aer_mean <- log(pred_veg_complete$aer_mean)
hist(pred_veg_complete$log_aer_mean)
shapiro.test(pred_veg_complete$log_aer_mean)
aov_aer <- aov(log_aer_mean ~ Station, data = pred_veg_complete)
summary(aov_aer)



## VOR
hist(pred_veg_complete$vor_mean)
# Transform
pred_veg_complete$log_vor_mean <- log(pred_veg_complete$vor_mean)
hist(pred_veg_complete$log_vor_mean)
shapiro.test(pred_veg_complete$log_vor_mean)
# ANOVA
aov_vor <- aov(log_vor_mean ~ Station, data = pred_veg_complete)
summary(aov_vor)



## Dead Wood
hist(pred_veg_complete$Dead_wood)
# Can't transform satisfactorily
# Use Kruskal-Wallis
kruskal.test(Dead_wood ~ Station, data = pred_veg_complete)



## Ferns
hist(pred_veg_complete$Ferns)
# Can't transform satisfactorily
# Use Kruskal-Wallis
kruskal.test(Ferns ~ Station, data = pred_veg_complete)



## Willow
hist(pred_veg_complete$Willow)
# Can't transform satisfactorily
# Use Kruskal-Wallis
kruskal.test(Willow ~ Station, data = pred_veg_complete)



## Grasses and forbs
hist(pred_veg_complete$gra.for)
# Transform
pred_veg_complete$sqrt_gra.for <- sqrt(pred_veg_complete$gra.for)
hist(pred_veg_complete$sqrt_gra.for)
shapiro.test(pred_veg_complete$sqrt_gra.for)
# ANOVA
aov_gra.for <- aov(sqrt_gra.for ~ Station, data = pred_veg_complete)
summary(aov_gra.for)



## Bare ground and moss
hist(pred_veg_complete$bar.mos)
# Can't transform satisfactorily
# Use Kruskal-Wallis
kruskal.test(bar.mos ~ Station, data = pred_veg_complete)



## Heather and Bilberry
hist(pred_veg_complete$hea.bil)
shapiro.test(pred_veg_complete$hea.bil)
# ANOVA
aov_hea.bil <- aov(hea.bil ~ Station, data = pred_veg_complete)
summary(aov_hea.bil)



## Gorse and bramble
hist(pred_veg_complete$gor.bra)
# Can't transform satisfactorily
# Use Kruskal-Wallis
kruskal.test(gor.bra ~ Station, data = pred_veg_complete)



## Rushes and woodrush
hist(pred_veg_complete$rus.woo)
# Log transformation works but throws up inf results
# Therefore Kruskal-Wallis it
kruskal.test(rus.woo ~ Station, data = pred_veg_complete)



# Conifers
hist(pred_veg_complete$con)
# Log transformation works but throws up inf results
# Therefore Kruskal-Wallis it
kruskal.test(con ~ Station, data = pred_veg_complete)

# These tests showed that there are NO vegetation/habitat differences between distance bands



