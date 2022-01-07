#### McCarthy et al., 2021 ####
## December 2021 ##
## Habitat data ##

# Clear R's memory
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load in packages
library(dplyr)
library(ggplot2)

# Load in data
pred_veg <- read.csv("Predator vegetation survey data_cleaned_v2.csv")

# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
pred_veg <- pred_veg[!pred_veg$Site == "BG1",]
pred_veg <- pred_veg[!pred_veg$Site == "BT1",]

names(pred_veg)

#### Species Cover ####
# Select the relevant columns
spp_cov <- pred_veg[,c(2:3,7:10)]

# Delete rows with NAs
spp_cov <- spp_cov[complete.cases(spp_cov),]

# Make new column of site, station, distance and direction merged so that when dataframe is pivotted,
# all the raw data remains and nothing is lost
names(spp_cov)
spp_cov$ssdd <- paste(spp_cov[,1], spp_cov[,2], spp_cov[,3], spp_cov[,4], sep="_")

# Make base data column to merge back in later
names(spp_cov)
base_data <- spp_cov[,c(1,2,7),]

# Delete distance, direction and species columns
names(spp_cov)
spp_cov <- spp_cov[,-c(1:4)]

library(reshape2)
# Pivot the dataframe so that species values are set as column values, X..cover are set as the cell values
# and each row is based on ssdd
spp_cov_ref <- dcast(spp_cov, ssdd ~ Species, value.var = "X..cover")

# Merge back in the site and station columns, by ssdd column
spp_cov_ref <- merge(base_data,spp_cov_ref, by = c("ssdd"))
spp_cov_ref <- spp_cov_ref[!duplicated(spp_cov_ref$ssdd),]


# Set all NAs to 0 values
spp_cov_ref[is.na(spp_cov_ref)] <- 0

# Add up each row, they should all equal 100
names(spp_cov_ref)
spp_cov_ref$total <- rowSums(spp_cov_ref[,c(4:22)])
spp_cov_ref$total == 100

# Create composite columns
names(spp_cov_ref)
spp_cov_ref$gra.for <- spp_cov_ref$`Gr & fo` + spp_cov_ref$`Greater Willowherb` + spp_cov_ref$`Rosebay Willowherb` +
  spp_cov_ref$Tormentil
spp_cov_ref$bar.mos <- spp_cov_ref$`Bare ground` + spp_cov_ref$Moss
spp_cov_ref$hea.bil <- spp_cov_ref$Heather + spp_cov_ref$Bilberry
spp_cov_ref$gor.bra <- spp_cov_ref$Bramble + spp_cov_ref$Fuscia + spp_cov_ref$Gorse + spp_cov_ref$Holly
spp_cov_ref$rus.woo <- spp_cov_ref$Rushes + spp_cov_ref$Woodrush
spp_cov_ref$con <- spp_cov_ref$Lodgepole + spp_cov_ref$Sitka

# Ferns, willow and dead wood left over as on their own
# Delete individual columns that have been combined
names(spp_cov_ref)
spp_cov_ref <- spp_cov_ref[,-c(4:6,9:20,22:23)]
names(spp_cov_ref)
spp_cov_ref$total <- rowSums(spp_cov_ref[,c(4:12)])
spp_cov_ref$total == 100

names(spp_cov_ref)
spp_cov_ref$Station <- as.character(spp_cov_ref$Station)

# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
spp_cov_ref <- spp_cov_ref[!spp_cov_ref$Site == "BG1",]
spp_cov_ref <- spp_cov_ref[!spp_cov_ref$Site == "BT1",]

# Look at correlation between different habitat categories
# Correlation matrix
names(spp_cov_ref)
cor(spp_cov_ref[c(4:12)])
str(spp_cov_ref)

# Get mean per site/station
spp_cov_ref_sum <- spp_cov_ref %>%
  group_by(Site, Station) %>%
  summarise_all("mean")

cor(spp_cov_ref_sum[c(4:12)])

##########
## Add in vegetation height, tree height, VOR and AES to the dataframe

#### Vegetation Height ####
names(pred_veg)
# Select relevant columns to height
spp_hgt <- pred_veg[, c(2:3,14)]
# Delete rows with NAs
spp_hgt <- spp_hgt[complete.cases(spp_hgt),]

# Get mean per site/station
spp_hgt_mean <- spp_hgt %>%
  group_by(Site, Station) %>%
  summarise_all("mean")


#### Tree Height & Distance ####
# Select relevant columns
names(pred_veg)
tr_hgt <- pred_veg[,c(2:3,18)]
# Delete NAs
tr_hgt <- tr_hgt[complete.cases(tr_hgt),]

# Get mean per site/station
tr_hgt_mean <- tr_hgt %>%
  group_by(Site, Station) %>%
  summarise_all("mean")


#### VOR ####
# Select relevant columns
names(pred_veg)
vor <- pred_veg[,c(2:3,23)]
# Delete NAs
vor <- vor[complete.cases(vor),]

# Get mean per site/station
vor_mean <- vor %>%
  group_by(Site, Station) %>%
  summarise_all("mean")


#### Aerial Visibility ####
# Select relevant columns
names(pred_veg)
aer <- pred_veg[,c(2:3, 25)]
# Delete NAs
aer <- aer[complete.cases(aer),]


# Get mean per site/station
aer_mean <- aer %>%
  group_by(Site, Station) %>%
  summarise_all("mean")


## Merge these measures with species cover data
extra_measures <- merge(spp_hgt_mean, tr_hgt_mean, by = c("Site", "Station"))
extra_measures <- merge(extra_measures, vor_mean, by = c("Site", "Station"))
extra_measures <- merge(extra_measures, aer_mean, by = c("Site", "Station"))

spp_cov_ref_sum <- merge(spp_cov_ref_sum, extra_measures, by = c("Site", "Station"))
names(spp_cov_ref_sum)

# Correlation test
cor(spp_cov_ref_sum[c(4:12,14:17)])
# No two variables are very correlated


#############################################################################
######################
#### PCA Analysis ####

# Run the PCA
library(factoextra)
names(spp_cov_ref_sum)
h.pca <- prcomp(spp_cov_ref_sum[c(4:12,14:17)], scale = TRUE)
fviz_eig(h.pca)
# Eigenvalues
eig.val_v <- get_eigenvalue(h.pca)
# Results for Variables
v.var <- get_pca_var(h.pca)
v.var$coord          # Coordinates
v.var$contrib        # Contributions to the PCs
v.var$cos2           # Quality of representation 
# Results for individuals
v.ind <- get_pca_ind(h.pca)
v.ind$coord          # Coordinates
v.ind$contrib        # Contributions to the PCs
v.ind$cos2           # Quality of representation

hab <- v.ind$coord[,c(1:4)]

# Build a dataframe
names(spp_cov_ref)
ref_data <- spp_cov_ref[, c(1:3)]
hab_pc <- data.frame(spp_cov_ref_sum[, c(1:2)], hab)
names(hab_pc)
names(hab_pc)[3:6] <- c("PC1", "PC2", "PC3", "PC4")
names(hab_pc)

# Write as csv
#write.csv(hab_pc, "fine_scale_hab_pca.csv")
#write.csv(v.var$coord, "fine_scale_hab_loadings.csv")

