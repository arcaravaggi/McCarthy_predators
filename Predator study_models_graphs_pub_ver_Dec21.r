#### McCarthy et al., 2021 ####
## December 2021 ##
## Models and graphs ##

# Clear R's memory
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dplyr package
library(dplyr)

# Three sections to this script
# Data organising
# Models and statistics
# Graphs

#### Camera Data ####

cam_dat_raw <- read.csv("Camera_trap_data_30_v2.csv")
# Filter out rows to include
cam_dat_raw <- cam_dat_raw[cam_dat_raw$Include == "Y",]
# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
cam_dat_raw <- cam_dat_raw[!cam_dat_raw$Site == "BG1",]
cam_dat_raw <- cam_dat_raw[!cam_dat_raw$Site == "BT1",]


#### 24 Hour Activity Patterns ####

# Filter out first detection of each encounter
# Merge Site,Station and encounter into one new column
names(cam_dat_raw)
cam_dat_raw$ss <- paste(cam_dat_raw[,2], cam_dat_raw[,3], sep = "_")
cam_dat_raw$sse <- paste(cam_dat_raw[,2], cam_dat_raw[,3], cam_dat_raw[,13], sep = "_")
cam_dat_firsts <- cam_dat_raw[!duplicated(cam_dat_raw$sse),]
# Remove rows of NAs for 24 hour activity graph
cam_dat_firsts_g <- na.omit(cam_dat_firsts)

# Encounters of each species per distance from edge
# 10m
# Delete columns not needed
names(cam_dat_firsts_g)
cam_dat_firsts_10 <- cam_dat_firsts_g[,-c(1:2,4:9,11:13,16:21)]
names(cam_dat_firsts_10)
cam_dat_firsts_10 <- cam_dat_firsts_10[cam_dat_firsts_10$Station == "10",]
table(cam_dat_firsts_10$Species)
#FX: 11, HC: 11, PM:9,BG:1,DG:1,JY:1, ST:1

# 50m
cam_dat_firsts_50 <- cam_dat_firsts_g[,-c(1:2,4:9,11:13,16:21)]
cam_dat_firsts_50 <- cam_dat_firsts_50[cam_dat_firsts_50$Station == "50",]
table(cam_dat_firsts_50$Species)
# PM:8,HC:5,FX:3,ST:2,AM:1

# 100m
cam_dat_firsts_100 <- cam_dat_firsts_g[,-c(1:2,4:9,11:13,16:21)]
cam_dat_firsts_100 <- cam_dat_firsts_100[cam_dat_firsts_100$Station == "100",]
table(cam_dat_firsts_100$Species)
# HC:8, FX: 7, BG:4, PM:3, ST:2, MG:1


# Delete columns not needed
names(cam_dat_firsts_g)
cam_dat_firsts_g <- cam_dat_firsts_g[,-c(1:9,11:13,16:21)]

## FULL SUMMARY OF ENCOUNTERS ##
table(cam_dat_firsts_g$Species)
# Total of 79 encounters
# 26 avian encounters
# 53 mammalian encounters
# 24 hooded crow
# 21 fox
# 20 pine marten
# 5 badger
# 5 stoat
# 1 American mink
# 1 dog
# 1 jay
# 1 magpie

# Make a table of the number of encounters during each capture hour for each species group
# Note the below line of code doesn't appear to work if plyr package is loaded
# Use it later to produce graph of predator activity by species group over 24 hours
cam_dat_firsts_g <- cam_dat_firsts_g %>% 
  group_by(Capture_hr) %>% 
  count(Sp_gr)






#### Predator Encounters ####

## Total Encounters ##

library(plyr)
# Create columns showing the total count of encounter records for each site/station combination
tot_enc <- plyr::ddply(cam_dat_firsts, .(Site, Station), transform, count = length(Pred_enc_30.mins))

# Get ride of duplicated rows of site/station so that there's only one row with each
# unique site/station combination
tot_enc <- tot_enc[!duplicated(tot_enc$ss),]
# This worked except it counts "0" encounters as 1, as 0 is a count value

# Delete unecessary columns
names(tot_enc)
tot_enc <- tot_enc[,-c(1,4:12,14:21)]

# Where pred_enc is 0, make count = 0
tot_enc$count[tot_enc$Pred_enc_30.mins == "0"] <- "0"
# Delete unecessary column
tot_enc <- tot_enc[,-c(3)]



## Avian predators ##

# Make base dataframe with all sites and stations
base_data <- tot_enc[,-c(3)]
# Now add onto this the relevant data
av_1 <- cam_dat_firsts[cam_dat_firsts$Sp_gr == "AV",]
# Remove NA rows
av_1 <- na.omit(av_1)
# Delete unneeded columns
names(av_1)
av_1 <- av_1[,-c(1,4:12,14:19)]
# Get a count of encounters at each site/station combination
av_enc <- plyr::ddply(av_1, .(Site, Station), transform, count = length(Pred_enc_30.mins))
# Get rid of duplicates
av_enc <- av_enc[!duplicated(av_enc$ss),]
# Avian predators were detected at 9 stations

# Delete uneeded columns
av_enc <- av_enc[,-c(3:5)]

# Merge this back in with the base data
# The all argument at the end keeps everything even when there is no merge possible due to NAs
av_enc <- merge(base_data, av_enc, by = c("Site", "Station"), all = TRUE)
# Replace NAs with 0
av_enc[is.na(av_enc)] <- 0



## Mammalian Predators ##

# Create mammal subset dataframe
ma_1 <- cam_dat_firsts[cam_dat_firsts$Sp_gr == "MA",]
# Remove NA rows
ma_1 <- na.omit(ma_1)

# Separate out foxes and pine martens
ma_1_fx <- ma_1[ma_1$Species == "FX",]
ma_1_pm <- ma_1[ma_1$Species == "PM",]

# Fox encounters
# Delete colmns not needed
names(ma_1_fx)
ma_1_fx <- ma_1_fx[,-c(1,4:12,15:19)]
# Get a count of encounters at each site/station combination
ma_fx_enc <- plyr::ddply(ma_1_fx, .(Site, Station), transform, count = length(Pred_enc_30.mins))
# Get rid of duplicates
ma_fx_enc <- ma_fx_enc[!duplicated(ma_fx_enc$ss),]
# Foxes detected at 10 stations
# Delete uneeded columns
names(ma_fx_enc)
ma_fx_enc <- ma_fx_enc[,-c(3,5,6)]


# Pine marten encounters
# Delete colmns not needed
names(ma_1_pm)
ma_1_pm <- ma_1_pm[,-c(1,4:12,15:19)]
# Get a count of encounters at each site/station combination
ma_pm_enc <- plyr::ddply(ma_1_pm, .(Site, Station), transform, count = length(Pred_enc_30.mins))
# Get rid of duplicates
ma_pm_enc <- ma_pm_enc[!duplicated(ma_pm_enc$ss),]
# Pine marten detected at 7 stations
# Delete uneeded columns
ma_pm_enc <- ma_pm_enc[,-c(3,5,6)]


# All mammalian predators combined
# Delete unneeded columns
names(ma_1)
ma_1 <- ma_1[,-c(1,4:12,14:19)]
# Get a count of encounters at each site/station combination
ma_all_enc <- plyr::ddply(ma_1, .(Site, Station), transform, count = length(Pred_enc_30.mins))
# Get rid of duplicates
ma_all_enc <- ma_all_enc[!duplicated(ma_all_enc$ss),]
# Mammalian predators were detected at 17 stations
# Delete uneeded columns
ma_all_enc <- ma_all_enc[,-c(3:5)]


# Merge FX, PM and all mammalian pred encs dataframes together
ma_enc <- merge(ma_fx_enc, ma_pm_enc, by = c("Site", "Station"), all = TRUE)
names(ma_enc)
# Rename count columns
colnames(ma_enc)[4] <- "fx_enc"
colnames(ma_enc)[6] <- "pm_enc"
# Delete columns not needed
ma_enc <- ma_enc[,-c(3,5)]
# Replace NAs with 0
ma_enc[is.na(ma_enc)] <- 0

# Now merge overall mammalian encs data
ma_enc <- merge(ma_enc, ma_all_enc, by = c("Site", "Station"), all = TRUE)
# Rename Count column to All_count 
names(ma_enc)
colnames(ma_enc)[5] <- "ma_enc"
# Replace NAs with 0
ma_enc[is.na(ma_enc)] <- 0


# Merge this back in with the base data
# The all argument at the end keeps everything even when there is no merge possible due to NAs
ma_enc <- merge(base_data, ma_enc, by = c("Site", "Station"), all = TRUE)
# Replace NAs with 0
ma_enc[is.na(ma_enc)] <- 0

# Create a new column which is {All enc- (FX+PM enc)}
ma_enc$other_ma_enc <- ma_enc$ma_enc - (ma_enc$fx_enc + ma_enc$pm_enc)
names(ma_enc)


## Merge Total, Avian and Mammalian encounters into one dataframe ##
# First change the names of the count columns as they are all called "count" at the moment
names(tot_enc) <- c("Site", "Station", "tot_enc")
names(tot_enc)
names(av_enc) <- c("Site", "Station", "av_enc")
names(av_enc)

all_enc <- merge(tot_enc, av_enc, by = c("Site", "Station"))
all_enc <- merge(all_enc, ma_enc, by = c("Site", "Station"))

# Make another copy of the all_enc dataframe for graphing
all_enc_g <- all_enc





#### Latency to first detection (daily survival) ####

## All Predators- first detection ##

# Filter out the first detection at each station
cam_dat_lat <- cam_dat_raw[cam_dat_raw$Pred_enc_30.mins == "1",]

# There are still many records from the same first encounters
# So get rid of these duplicates leaving only the first encounter for each site/station combination
cam_dat_lat <- cam_dat_lat[!duplicated(cam_dat_lat$ss),]
names(cam_dat_lat)
cam_dat_lat <- cam_dat_lat[,-c(1,5,7,10:21)]
names(cam_dat_lat)

cam_dat_lat$start <- paste(cam_dat_lat[,3], cam_dat_lat[,4], sep = "-")
names(cam_dat_lat)
cam_dat_lat$capture <- paste(cam_dat_lat[,5], cam_dat_lat[,6], sep = "-")
names(cam_dat_lat)

cam_dat_lat <- cam_dat_raw[cam_dat_raw$Pred_enc_30.mins == "1",] # Initial df creation code from above
# Get rid of ss (site/station) duplicates so that the first detection per site/station combo is left over
cam_dat_lat <- cam_dat_lat[!duplicated(cam_dat_lat$ss),]
# Delete columns not needed
cam_dat_lat <- cam_dat_lat[,-c(1,5,7,10:21)]
names(cam_dat_lat)
# Add in start and capture columns of date and time combined
cam_dat_lat$start <- paste(cam_dat_lat[,3], cam_dat_lat[,4], sep = " ") # Space as separator
cam_dat_lat$capture <- paste(cam_dat_lat[,5], cam_dat_lat[,6], sep = " ")
names(cam_dat_lat)

cam_dat_lat$start <- as.POSIXct(cam_dat_lat$start, format="%d/%m/%Y %H:%M:%S") # as.POSIXct conversion
cam_dat_lat$capture  <- as.POSIXct(cam_dat_lat$capture, format="%d/%m/%Y %H:%M:%S")

# Calulate the difference in hours between the two new formatted columns, rounding by 2 decimal places
cam_dat_lat$diff_hours <- round(difftime(cam_dat_lat$capture, cam_dat_lat$start, units = "hours"),2)





## Avian Predators- first detection ##
cam_dat_av <- cam_dat_raw[cam_dat_raw$Sp_gr == "AV",]
cam_dat_av <- cam_dat_av[!duplicated(cam_dat_av$sse),]
# Remove rows of NAs
cam_dat_av <- na.omit(cam_dat_av)
# Get rid of ss (site/station) duplicates so that the first detection per site/station combo is left over
cam_dat_av_lat <- cam_dat_av[!duplicated(cam_dat_av$ss),]
# Get rid of uneeded columns
names(cam_dat_av_lat)
cam_dat_av_lat <- cam_dat_av_lat[,-c(1,5,7,10:14,16:21)]
names(cam_dat_av_lat)
# Add in start and capture columns of date and time combined
cam_dat_av_lat$start <- paste(cam_dat_av_lat[,3], cam_dat_av_lat[,4], sep = " ")
cam_dat_av_lat$capture <- paste(cam_dat_av_lat[,5], cam_dat_av_lat[,6], sep = " ")
# as.POSIXct conversion
cam_dat_av_lat$start <- as.POSIXct(cam_dat_av_lat$start, format = "%d/%m/%Y %H:%M:%S")
cam_dat_av_lat$capture <- as.POSIXct(cam_dat_av_lat$capture, format = "%d/%m/%Y %H:%M:%S")
cam_dat_av_lat$diff_hours <- round(difftime(cam_dat_av_lat$capture, cam_dat_av_lat$start, units = "hours"),2)


## Mammalian Predators- first detection ##
cam_dat_ma <- cam_dat_raw[cam_dat_raw$Sp_gr == "MA",]
cam_dat_ma <- cam_dat_ma[!duplicated(cam_dat_ma$sse),]
# Remove rows of NAs
cam_dat_ma <- na.omit(cam_dat_ma)
# Get rid of ss (site/station) duplicates so that the first detection per site/station combo is left over
cam_dat_ma_lat <- cam_dat_ma[!duplicated(cam_dat_ma$ss),]
# Get rid of uneeded columns
names(cam_dat_ma_lat)
cam_dat_ma_lat <- cam_dat_ma_lat[,-c(1,5,7,10:14,16:21)]
names(cam_dat_ma_lat)
# Add in start and capture columns of date and time combined
cam_dat_ma_lat$start <- paste(cam_dat_ma_lat[,3], cam_dat_ma_lat[,4], sep = " ")
cam_dat_ma_lat$capture <- paste(cam_dat_ma_lat[,5], cam_dat_ma_lat[,6], sep = " ")
# as.POSIXct conversion
cam_dat_ma_lat$start <- as.POSIXct(cam_dat_ma_lat$start, format = "%d/%m/%Y %H:%M:%S")
cam_dat_ma_lat$capture <- as.POSIXct(cam_dat_ma_lat$capture, format = "%d/%m/%Y %H:%M:%S")
cam_dat_ma_lat$diff_hours <- round(difftime(cam_dat_ma_lat$capture, cam_dat_ma_lat$start, units = "hours"),2)


# Foxes only
## Foxes- first detection ##
cam_dat_fx <- cam_dat_raw[cam_dat_raw$Species == "FX",]
cam_dat_fx <- cam_dat_fx[!duplicated(cam_dat_fx$sse),]
# Remove rows of NAs
cam_dat_fx <- na.omit(cam_dat_fx)
# Get rid of ss (site/station) duplicates so that the first detection per site/station combo is left over
cam_dat_fx_lat <- cam_dat_fx[!duplicated(cam_dat_fx$ss),]
# Get rid of uneeded columns
names(cam_dat_fx_lat)
cam_dat_fx_lat <- cam_dat_fx_lat[,-c(1,5,7,10:14,16:21)]
names(cam_dat_fx_lat)
# Add in start and capture columns of date and time combined
cam_dat_fx_lat$start <- paste(cam_dat_fx_lat[,3], cam_dat_fx_lat[,4], sep = " ")
cam_dat_fx_lat$capture <- paste(cam_dat_fx_lat[,5], cam_dat_fx_lat[,6], sep = " ")
# as.POSIXct conversion
cam_dat_fx_lat$start <- as.POSIXct(cam_dat_fx_lat$start, format = "%d/%m/%Y %H:%M:%S")
cam_dat_fx_lat$capture <- as.POSIXct(cam_dat_fx_lat$capture, format = "%d/%m/%Y %H:%M:%S")
cam_dat_fx_lat$diff_hours <- round(difftime(cam_dat_fx_lat$capture, cam_dat_fx_lat$start, units = "hours"),2)


# Pine Marten only
## Pine marten- first detection ##
cam_dat_pm <- cam_dat_raw[cam_dat_raw$Species == "PM",]
cam_dat_pm <- cam_dat_pm[!duplicated(cam_dat_pm$sse),]
# Remove rows of NAs
cam_dat_pm <- na.omit(cam_dat_pm)
# Get rid of ss (site/station) duplicates so that the first detection per site/station combo is left over
cam_dat_pm_lat <- cam_dat_pm[!duplicated(cam_dat_pm$ss),]
# Get rid of uneeded columns
names(cam_dat_pm_lat)
cam_dat_pm_lat <- cam_dat_pm_lat[,-c(1,5,7,10:14,16:21)]
names(cam_dat_pm_lat)
# Add in start and capture columns of date and time combined
cam_dat_pm_lat$start <- paste(cam_dat_pm_lat[,3], cam_dat_pm_lat[,4], sep = " ")
cam_dat_pm_lat$capture <- paste(cam_dat_pm_lat[,5], cam_dat_pm_lat[,6], sep = " ")
# as.POSIXct conversion
cam_dat_pm_lat$start <- as.POSIXct(cam_dat_pm_lat$start, format = "%d/%m/%Y %H:%M:%S")
cam_dat_pm_lat$capture <- as.POSIXct(cam_dat_pm_lat$capture, format = "%d/%m/%Y %H:%M:%S")
cam_dat_pm_lat$diff_hours <- round(difftime(cam_dat_pm_lat$capture, cam_dat_pm_lat$start, units = "hours"),2)

# Create species column so I can merge fox and pine marten LTD dataframe so I can compare LTD between the two species
cam_dat_fx_lat$Species <- "fx"
cam_dat_pm_lat$Species <- "pm"

fx_pm_lat <- rbind(cam_dat_fx_lat, cam_dat_pm_lat)





###########################################################################################################
#### Modelling and Statistics ####

# Load packages
library(plyr)
library(PMCMR)
library(PMCMRplus)
library(glmmTMB)
library(TMB)
library(DHARMa)
library(ggplot2)
library(factoextra)

#########################
## ENCOUNTERS - MODELS ##
#########################

# Vegetation PCs vs. distance band
site_info <- read.csv("site_info_v2.csv")

# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
site_info <- site_info[!site_info$Site == "BG1",]
site_info <- site_info[!site_info$Site == "BT1",]

# Delete duplicated sites 
site_info_p.a <- site_info[!duplicated(site_info$Site),]
hist(site_info_p.a$P.A)

# Write standard error function
ste <- function(x) sd(x)/sqrt(length(x))

# Calculate variation in P:A across the 10 sites
# Mean, SE
p.a_mean <- mean(site_info_p.a$P.A)
# 0.01383309
p.a_se <- ste(site_info_p.a$P.A)
# 0.0007492104

# Calculate coefficient of variation
library(goeveg)
cv(site_info_p.a$P.A)
# 0.1712712

# About coefficient of variation:
# According to Dormann 2013 CV-values below 0.05 (5%) indicate very high precision of the data, 
# values above 0.2 (20%) low precision. However, this is considered as a rule of thumb
# 0.17 shows little variation in the data across the sites

# Read in habitat data
habitat <- read.csv("habitat_data_reformatted.csv")
names(habitat)

# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
habitat <- habitat[!habitat$Site == "BG1",]
habitat <- habitat[!habitat$Site == "BT1",]

# Graph mean cover and SE of each habitat type
# This graph is actually repeated at the end in colour
names(habitat)
habitat_graph <- ggplot(habitat, aes(Habitat, Cover)) +
  stat_summary(fun = mean, geom = "bar", size = 2, position=position_dodge(0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, position=position_dodge(0.5))
habitat_graph

# Total area is 12.7
# Calculate percentage cover of each habitat
habitat$perc <- (habitat$Cover/12.7)*100

# Write standard error function
ste <- function(x) sd(x)/sqrt(length(x))

# Calculate mean cover and SE of each habitat
# Descriptives of habitat around the study sites
names(habitat)
hab_means <- aggregate(habitat[, 4], list(habitat$Habitat), mean)
hab_ste <- aggregate(habitat[, 4], list(habitat$Habitat), ste)
hab_means <- merge(hab_means, hab_ste, by = "Group.1")

# Create new group which is total forestry categories combined
# early_forest, late_forest, prime_forest
forest_only <- habitat[habitat$Habitat %in% c("early_forest","late_forest","prime_forest"),]
# Now calculate the sum of these categories for each site
total_forest <- aggregate(forest_only[,4], list(forest_only$Site), sum)
# Then calculate mean and standard error
mean(total_forest$x)
ste(total_forest$x)

# Rename count columns
colnames(hab_means)[1] <- "Habitat"
colnames(hab_means)[2] <- "Mean"
colnames(hab_means)[3] <- "SE"


## Habitat PCA ##
# Read in habitat data in other format
habitat_ref <- read.csv("habitat_data.csv")
# Exclude BG1 and BT1 due to failed cameras at 50m station at both of these sites
habitat_ref <- habitat_ref[!habitat_ref$site == "BG1",]
habitat_ref <- habitat_ref[!habitat_ref$site == "BT1",]

# Look at correlation between different habitat categories
# Correlation matrix
names(habitat_ref)
cor(habitat_ref[c(5:12)])

# Run the PCA
h.pca <- prcomp(habitat_ref[c(5:12)], scale = TRUE)
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

# Write PCA results
#pca_res <- v.var$coord
#write.csv(pca_res, file = "landscape_PCA_hab_loadings.csv")

# Build dataframe
names(habitat_ref)
names(site_info_p.a)
# Add in start dates for each site
dates <- cam_dat_raw[!duplicated(cam_dat_raw$Site),]
dates <- dates[, c(2:4)]
hab_pc <- data.frame(habitat_ref[c(1)], hab, site_info_p.a[c(4:9)], dates[c(3)])
names(hab_pc)
names(hab_pc)[1:5] <- c("Site", "PC1", "PC2", "PC3", "PC4")

# Format start date column to date
str(hab_pc)
hab_pc$St_date <- as.Date(hab_pc$St_date, format = "%d/%m/%y")

# Top two PCs account for 68.72% of the variation

# Merge site_info with all_enc
all_enc_mod <- merge(all_enc, hab_pc, by = c("Site"))
# Check variables are correct types
str(all_enc_mod)
# Change tot_enc and age to numeric
all_enc_mod$tot_enc <- as.numeric(as.character(all_enc_mod$tot_enc))
all_enc_mod$Age <- as.numeric(as.character(all_enc_mod$Age))
str(all_enc_mod)

# Change station to factor
all_enc_mod$Station <- as.factor(all_enc_mod$Station)
str(all_enc_mod)

# Check distribution of the data
hist(all_enc_mod$tot_enc)
hist(all_enc_mod$av_enc)
hist(all_enc_mod$ma_enc)
hist(all_enc_mod$fx_enc)
hist(all_enc_mod$pm_enc)
hist(all_enc_mod$other_ma_enc)

# These data are zero-inflated
names(all_enc_mod)
str(all_enc_mod)

# Add in fine-scale habitat PCs from PCA
fine_hab <- read.csv("fine_scale_hab_pca.csv")
names(fine_hab)
# Rename PC columns as landscape scale PCs have same names
colnames(fine_hab)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(fine_hab)
# Merge dataframes
all_enc_mod_1 <- merge(fine_hab, all_enc_mod, by = c("Site", "Station"))
hist(all_enc_mod_1$tot_enc)




#### Total Encounters ####
# Zero-inflated model
tot_enc_basic <- glmmTMB(tot_enc ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + (1|Site) + (1|St_date), 
                         data = all_enc_mod_1, family = nbinom2)
summary(tot_enc_basic)
tot_enc_simres_basic <- simulateResiduals(tot_enc_basic)
plot(tot_enc_simres_basic, rank = T)
# All okay

# Conduct additional tests
testUniformity(tot_enc_simres_basic)
testDispersion(tot_enc_simres_basic)
testZeroInflation(tot_enc_simres_basic)

# Write overdispersion function
overdisp_fun <- function(tot_enc_basic) {
  rdf <- df.residual(tot_enc_basic)
  rp <- residuals(tot_enc_basic,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(tot_enc_basic)
# Ratio of 1.32, no overdispersion

library(MuMIn)
# Dredging data
options(na.action = "na.fail")
tot.enc.aic1 <- dredge(tot_enc_basic, rank = "AIC")
options(na.action = "na.omit")
tot.enc.imp <- importance(tot.enc.aic1) # Variable  -importance weights
tot.enc.tset <- subset(tot.enc.aic1, delta <2) # Top set of models (<2 delta AIC)
tot.enc.tmod <- subset(tot.enc.aic1, delta == 0) # Best approximating model

# Top model is not null model

# Average model  
tot.enc.avg <-model.avg(tot.enc.tset)
summary(tot.enc.avg) 
confint(tot.enc.avg,level= 0.95)
importance(tot.enc.avg)
# Nothing significant




#### Avian Encounters ####
# Zero-inflated model
av_enc_basic <- glmmTMB(av_enc ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + (1|Site) + (1|St_date), 
                        data = all_enc_mod_1, family = nbinom2)
summary(av_enc_basic)
av_enc_simres_basic <- simulateResiduals(av_enc_basic)
plot(av_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look okay

# Conduct additional tests for overdispersion
testUniformity(av_enc_simres_basic)
testDispersion(av_enc_simres_basic)
testZeroInflation(av_enc_simres_basic)

overdisp_fun(av_enc_basic)
# Ratio is 0.687
# Overdispersion does not appear to be an issue

# Dredging data
options(na.action = "na.fail")
av.enc.aic1 <- dredge(av_enc_basic, rank = "AIC")
options(na.action = "na.omit")
av.enc.imp <- importance(av.enc.aic1) # Variable  -importance weights
av.enc.tset <- subset(av.enc.aic1, delta <2) # Top set of models (<2 delta AIC)
av.enc.tmod <- subset(av.enc.aic1, delta == 0) # Best approximating model

# Top model is null model

# Average model  
av.enc.avg <-model.avg(av.enc.tset)
summary(av.enc.avg) 
confint(av.enc.avg,level= 0.95)
importance(av.enc.avg)
# Nothing significant





#### Mammalian Encounters ####
# Zero-inflated model
mam_enc_basic <- glmmTMB(ma_enc ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + (1|Site) + (1|St_date), 
                         data = all_enc_mod_1, family = nbinom2,
                         control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
summary(mam_enc_basic)
mam_enc_simres_basic <- simulateResiduals(mam_enc_basic)
plot(mam_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look okay
# PC1_fs is significant and PC2_fs is a strong trend

# Conduct additional tests for overdispersion
testUniformity(mam_enc_simres_basic)
testDispersion(mam_enc_simres_basic)
testZeroInflation(mam_enc_simres_basic)

overdisp_fun(mam_enc_basic)
# Ratio of 1.31, so overdisperion is not an issue

# Dredging data
options(na.action = "na.fail")
ma.enc.aic1 <- dredge(mam_enc_basic, rank = "AIC")
options(na.action = "na.omit")
ma.enc.imp <- importance(ma.enc.aic1) # Variable  -importance weights
ma.enc.tset <- subset(ma.enc.aic1, delta <2) # Top set of models (<2 delta AIC)
ma.enc.tmod <- subset(ma.enc.aic1, delta == 0) # Best approximating model

# Top model is not null model. Null model is in top set

# Average model  
ma.enc.avg <-model.avg(ma.enc.tset)
summary(ma.enc.avg) 
confint(ma.enc.avg,level= 0.95)
importance(ma.enc.avg)
# Strong trends for PCs in top model, not in avaerage model



#### Fox Encounters ####
# Zero-inflated model
fx_enc_basic <- glmmTMB(fx_enc ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + (1|Site) + (1|St_date), 
                        data = all_enc_mod_1, family = nbinom2,
                        control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
summary(fx_enc_basic)
fx_enc_simres_basic <- simulateResiduals(fx_enc_basic)
plot(fx_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look okay

# Conduct additional tests for overdispersion
testUniformity(fx_enc_simres_basic)
testDispersion(fx_enc_simres_basic)
testZeroInflation(fx_enc_simres_basic)

overdisp_fun(fx_enc_basic)
# Ratio of 1.03
# Overdispersion is not an issue

# Dredging data
options(na.action = "na.fail")
fx.enc.aic1 <- dredge(fx_enc_basic, rank = "AIC")
options(na.action = "na.omit")
fx.enc.imp <- importance(fx.enc.aic1) # Variable  -importance weights
fx.enc.tset <- subset(fx.enc.aic1, delta <2) # Top set of models (<2 delta AIC)
fx.enc.tmod <- subset(fx.enc.aic1, delta == 0) # Best approximating model
# Remove false rows due to non-convergence
fx.enc.tset <- fx.enc.tset[-c(17:19),]

# Top model is null model

# Average model  
fx.enc.avg <-model.avg(fx.enc.tset)
summary(fx.enc.avg) 
confint(fx.enc.avg,level= 0.95)
importance(fx.enc.avg)





#### Pine marten Encounters ####
# Zero-inflated model
pm_enc_basic <- glmmTMB(pm_enc ~ Station + PC1_fs + PC2_fs + PC3_fs + PC1 + PC2 + (1|Site) + (1|St_date), 
                        data = all_enc_mod_1, family = nbinom2,
                        control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
# Had to remove PC4_fs due to non-convergence
summary(pm_enc_basic)
pm_enc_simres_basic <- simulateResiduals(pm_enc_basic)
plot(pm_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals not perfect but acceptable

# Conduct additional tests for overdispersion
testUniformity(pm_enc_simres_basic)
testDispersion(pm_enc_simres_basic)
testZeroInflation(pm_enc_simres_basic)

overdisp_fun(pm_enc_basic)
# Ratio of 0.74, so overdispersion is not an issue

# Dredging data
options(na.action = "na.fail")
pm.enc.aic1 <- dredge(pm_enc_basic, rank = "AIC")
options(na.action = "na.omit")
pm.enc.imp <- importance(pm.enc.aic1) # Variable  -importance weights
pm.enc.tset <- subset(pm.enc.aic1, delta <2) # Top set of models (<2 delta AIC)
pm.enc.tmod <- subset(pm.enc.aic1, delta == 0) # Best approximating model

# Top model is not in top set of models

# Average model  
pm.enc.avg <-model.avg(pm.enc.tset)
summary(pm.enc.avg) 
confint(pm.enc.avg,level= 0.95)
importance(pm.enc.avg)





#########################################################
## LATENCY TO FIRST DETECTION/ DAILY SURVIVAL - MODELS ##
#########################################################

# load packages for linear mixed effects model
library(lme4)
library(lubridate)
library(lmerTest)


# Change diff_time to numeric values so that lmer can work
str(cam_dat_lat)
str(cam_dat_ma_lat)
str(cam_dat_av_lat)
str(cam_dat_fx_lat)
str(cam_dat_pm_lat)
str(fx_pm_lat)
cam_dat_lat$diff_hours <- as.numeric(as.character(cam_dat_lat$diff_hours))
cam_dat_ma_lat$diff_hours <- as.numeric(as.character(cam_dat_ma_lat$diff_hours))
cam_dat_av_lat$diff_hours <- as.numeric(as.character(cam_dat_av_lat$diff_hours))
cam_dat_fx_lat$diff_hours <- as.numeric(as.character(cam_dat_fx_lat$diff_hours))
cam_dat_pm_lat$diff_hours <- as.numeric(as.character(cam_dat_pm_lat$diff_hours))
fx_pm_lat$diff_hours <- as.numeric(as.character(fx_pm_lat$diff_hours))

# Ensure stations is a factor and not continuous
str(cam_dat_lat)
cam_dat_lat$Station <- as.factor(cam_dat_lat$Station)
str(cam_dat_ma_lat)
cam_dat_ma_lat$Station <- as.factor(cam_dat_ma_lat$Station)
str(cam_dat_av_lat)
cam_dat_av_lat$Station <- as.factor(cam_dat_av_lat$Station)
str(cam_dat_fx_lat)
cam_dat_fx_lat$Station <- as.factor(cam_dat_fx_lat$Station)
str(cam_dat_pm_lat)
cam_dat_pm_lat$Station <- as.factor(cam_dat_pm_lat$Station)
str(fx_pm_lat)
fx_pm_lat$Station <- as.factor(fx_pm_lat$Station)


#####################
## ADD IN ALL CAMERAS

## ALL PREDATORS ##
# Merge in stations that had no detections
names(all_enc)
baseline_stations <- all_enc[,c(1,2)]
names(baseline_stations)
baseline_stations$ss <- paste(baseline_stations[,1], baseline_stations[,2], sep = "_")
names(baseline_stations)
baseline_stations$diff_hours <- 336

# Create dataframe that has missing stations for all predators group
baseline_stations_all <- subset(baseline_stations, ss %in% c("BG3_50", "BH1_50", "BH1_100", "ML4_50", "ML9_10", "ML9_100",
                                                             "NG2_10"))
baseline_stations_all <- baseline_stations_all[,c(1,2)]
baseline_stations_all$diff_hours <- 336
str(cam_dat_lat)
str(baseline_stations_all)
baseline_stations_all$Station <- as.factor(baseline_stations_all$Station)

ltd_all <- rbind.fill(baseline_stations_all, cam_dat_lat)

# Add in missing start dates
ltd_all$St_date[ltd_all$Site == "BG3"] <- "31/07/2018"
ltd_all$St_date[ltd_all$Site == "BH1"] <- "15/06/2018"
ltd_all$St_date[ltd_all$Site == "ML4"] <- "24/07/2018"
ltd_all$St_date[ltd_all$Site == "ML9"] <- "07/06/2018"
ltd_all$St_date[ltd_all$Site == "BG3"] <- "31/07/2018"
ltd_all$St_date[ltd_all$Site == "NG2"] <- "17/05/2018"


# Calculate daily survival rates
ltd_all$Daily_surv <- ltd_all$diff_hours
ltd_all$Daily_surv <- ltd_all$Daily_surv/24
ltd_all$Daily_surv <- ltd_all$Daily_surv/14

# Convert to Julian days
ltd_all$St_date2 <- lubridate::dmy(ltd_all$St_date) %>%
  as.numeric(format(., format = "%J"))

# Add in habitat PCs from PCA
fine_hab <- read.csv("fine_scale_hab_pca.csv")
ltd_all <- merge(fine_hab, ltd_all, by = c("Site", "Station"))

# Add in landscape scale PCs
names(hab_pc)
names(ltd_all)
colnames(ltd_all)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(ltd_all)

# Merge in the landscape PCA data
ltd_all <- merge(hab_pc, ltd_all, by = c("Site"))
str(ltd_all)
hist(ltd_all$Daily_surv)


## All Predators Model ##
# Run All daily survival model
# glmer gets singular fit, so use glm instead with start date and site as a fixes effects instead of random effect
all_ltd_mod <- glm(Daily_surv ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + Site + St_date2, 
                     data = ltd_all, family = "binomial")

plot(all_ltd_mod)
qqnorm(resid(all_ltd_mod)); qqline(resid(all_ltd_mod))
# QQ plot is good

# Examine the model outputs
summary(all_ltd_mod)

library(MuMIn)

# Dredging data
options(na.action = "na.fail")
a.ltd.aic1 <- dredge(all_ltd_mod, rank = "AIC")
options(na.action = "na.omit")
a.ltd.imp <- importance(a.ltd.aic1) # Variable  -importance weights
a.ltd.tset <- subset(a.ltd.aic1, delta <2) # Top set of models (<2 delta AIC)
a.ltd.tmod <- subset(a.ltd.aic1, delta == 0) # Best approximating model

# Top model is null model

# Average model  
a.ltd.avg <-model.avg(a.ltd.tset)
summary(a.ltd.avg)
confint(a.ltd.avg,level= 0.95)
importance(a.ltd.avg)
# Average model includes PC1 and PC2




## MAMMALIAN PREDATORS ##

# Create dataframe that has missing stations for all predators group
baseline_stations_ma <- subset(baseline_stations, ss %in% c("NG2_10", "NG1_10","NG1_50", "ML9_10", "ML9_100","ML8_50", "ML8_100", 
                                                             "ML4_50","BH1_50", "BH1_100","BG3_50", "BG2_10", "BG2_100"))

baseline_stations_ma <- baseline_stations_ma[,c(1,2)]
baseline_stations_ma$diff_hours <- 336
str(cam_dat_ma_lat)
str(baseline_stations_ma)
baseline_stations_ma$Station <- as.factor(baseline_stations_ma$Station)

ltd_ma <- rbind.fill(baseline_stations_ma, cam_dat_ma_lat)

# Add in missing start dates
ltd_ma$St_date[ltd_ma$Site == "BG2"] <- "31/07/2018"
ltd_ma$St_date[ltd_ma$Site == "BG3"] <- "31/07/2018"
ltd_ma$St_date[ltd_ma$Site == "BH1"] <- "15/06/2018"
ltd_ma$St_date[ltd_ma$Site == "ML4"] <- "24/07/2018"
ltd_ma$St_date[ltd_ma$Site == "ML8"] <- "17/07/2018"
ltd_ma$St_date[ltd_ma$Site == "ML9"] <- "07/06/2018"
ltd_ma$St_date[ltd_ma$Site == "BG3"] <- "31/07/2018"
ltd_ma$St_date[ltd_ma$Site == "NG1"] <- "17/05/2018"
ltd_ma$St_date[ltd_ma$Site == "NG2"] <- "17/05/2018"


# Calculate daily survival rates
ltd_ma$Daily_surv <- ltd_ma$diff_hours
ltd_ma$Daily_surv <- ltd_ma$Daily_surv/24
#ltd_ma$Daily_surv <- round(ltd_ma$Daily_surv, digits = 0)
ltd_ma$Daily_surv <- ltd_ma$Daily_surv/14

# Add in PCs
ltd_ma <- merge(fine_hab, ltd_ma, by = c("Site", "Station"))

# Convert to Julian days
ltd_ma$St_date2 <- lubridate::dmy(ltd_ma$St_date) %>%
  as.numeric(format(., format = "%J"))

# Add in landscape scale PCs
names(hab_pc)
names(ltd_ma)
colnames(ltd_ma)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(ltd_ma)

# Merge in the landscape PCA data
ltd_ma <- merge(hab_pc, ltd_ma, by = c("Site"))
str(ltd_ma)
hist(ltd_ma$Daily_surv)

# Run model
# Model won't converge with both site and date included, both on there own get the same results
ma_ltd_mod <- glm(Daily_surv ~ Station + PC1_fs + PC2_fs + PC3_fs + PC4_fs + PC1 + PC2 + Site + St_date2, 
                  data = ltd_ma, family = binomial())

summary(ma_ltd_mod)

plot(ma_ltd_mod)
qqnorm(resid(ma_ltd_mod)); qqline(resid(ma_ltd_mod))
# QQ plot is okay

# Examine the model outputs
summary(ma_ltd_mod)
anova(ma_ltd_mod)

# Dredging data
options(na.action = "na.fail")
ma.ltd.aic1 <- dredge(ma_ltd_mod, rank = "AIC")
options(na.action = "na.omit")
ma.ltd.imp <- importance(ma.ltd.aic1) # Variable  -importance weights
ma.ltd.tset <- subset(ma.ltd.aic1, delta <2) # Top set of models (<2 delta AIC)
ma.ltd.tmod <- subset(ma.ltd.aic1, delta == 0) # Best approximating model

# Top model is not the null model. Null model is not in top set.

# Average model  
ma.ltd.avg <-model.avg(ma.ltd.tset)
summary(ma.ltd.avg) 
confint(ma.ltd.avg,level= 0.95)
importance(ma.ltd.avg)





## AVIAN PREDATORS ##

# Create dataframe that has missing stations for all predators group
baseline_stations_av <- subset(baseline_stations, ss %in% c("BG2_50", "BG3_10", "BG3_50", "BG3_100", "BH1_10", "BH1_50", "BH1_100",
                                                            "ML2_10", "ML2_50", "ML4_10", "ML4_50", "ML4_100", "ML7_10", "ML7_50", 
                                                            "ML7_100", "ML9_10", "ML9_50", "ML9_100", "NG2_10", "NG2_50", "NG2_100"))

baseline_stations_av <- baseline_stations_av[,c(1,2)]
baseline_stations_av$diff_hours <- 336
str(cam_dat_av_lat)
str(baseline_stations_av)
baseline_stations_av$Station <- as.factor(baseline_stations_av$Station)

ltd_av <- rbind.fill(baseline_stations_av, cam_dat_av_lat)

# Calculate daily survival rates
ltd_av$Daily_surv <- ltd_av$diff_hours
ltd_av$Daily_surv <- ltd_av$Daily_surv/24
#ltd_av$Daily_surv <- round(ltd_av$Daily_surv, digits = 0)
ltd_av$Daily_surv <- ltd_av$Daily_surv/14

# Add in PCs
ltd_av <- merge(fine_hab, ltd_av, by = c("Site", "Station"))
str(ltd_av)
plot(ltd_av$Daily_surv)

# Convert to Julian days
ltd_av$St_date1 <- lubridate::dmy(ltd_av$St_date) %>%
  as.numeric(format(., format = "%J"))

# Add in landscape scale PCs
names(hab_pc)
names(ltd_av)
colnames(ltd_av)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(ltd_av)

# Merge in the landscape PCA data
ltd_av <- merge(hab_pc, ltd_av, by = c("Site"))
str(ltd_av)
hist(ltd_av$Daily_surv)

# Merge in start date
names(ltd_all)
start_date_data <- ltd_all[,c(1,13,27)]
names(start_date_data)

ltd_av <- merge(ltd_av, start_date_data, by = c("Site", "Station"))



# Run model
# Had to remove PCs to get model to converge. Had to remove start date to avoid singular fit
av_ltd_mod <- glm(Daily_surv ~ Station + PC1_fs + PC2_fs + PC1 + PC2 + St_date2, data = ltd_av, family = binomial())

summary(av_ltd_mod)

plot(av_ltd_mod)
qqnorm(resid(av_ltd_mod)); qqline(resid(av_ltd_mod))
# QQ plot is slightly tailed but acceptable

# Dredging data
options(na.action = "na.fail")
av.ltd.aic1 <- dredge(av_ltd_mod, rank = "AIC")
options(na.action = "na.omit")
av.ltd.imp <- importance(av.ltd.aic1) # Variable  -importance weights
av.ltd.tset <- subset(av.ltd.aic1, delta <2) # Top set of models (<2 delta AIC)
av.ltd.tmod <- subset(av.ltd.aic1, delta == 0) # Best approximating model

# Top model is null model

# Average model  
av.ltd.avg <-model.avg(av.ltd.tset)
summary(av.ltd.avg) 
confint(av.ltd.avg,level= 0.95)
importance(av.ltd.avg)




## FOXES ##

# Create dataframe that has missing stations for all predators group
baseline_stations_fx <- subset(baseline_stations, ss %in% c("BG2_10", "BG2_50", "BG2_100", "BG3_50", "BH1_10", "BH1_50", "BH1_100",
                                                            "ML4_50", "ML7_10", "ML7_50", "ML7_100", "ML8_10", "ML8_50", "ML8_100",
                                                            "ML9_10", "ML9_50", "ML9_100", "NG1_10", "NG1_50", "NG2_10"))

baseline_stations_fx <- baseline_stations_fx[,c(1,2)]
baseline_stations_fx$diff_hours <- 336
str(cam_dat_fx_lat)
str(baseline_stations_fx)
baseline_stations_fx$Station <- as.factor(baseline_stations_fx$Station)

ltd_fx <- rbind.fill(baseline_stations_fx, cam_dat_fx_lat)

# Calculate daily survival rates
ltd_fx$Daily_surv <- ltd_fx$diff_hours
ltd_fx$Daily_surv <- ltd_fx$Daily_surv/24
#ltd_fx$Daily_surv <- round(ltd_fx$Daily_surv, digits = 0)
ltd_fx$Daily_surv <- ltd_fx$Daily_surv/14

# Add in PCs
ltd_fx <- merge(fine_hab, ltd_fx, by = c("Site", "Station"))
plot(ltd_fx$Daily_surv)

# Convert to Julian days
ltd_fx$St_date2 <- lubridate::dmy(ltd_fx$St_date) %>%
  as.numeric(format(., format = "%J"))

# Add in landscape scale PCs
names(hab_pc)
names(ltd_fx)
colnames(ltd_fx)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(ltd_fx)

# Merge in the landscape PCA data
ltd_fx <- merge(hab_pc, ltd_fx, by = c("Site"))
str(ltd_fx)
hist(ltd_fx$Daily_surv)

# Run fox model
# Various issues with convergence and singular fits, hence why date, site, PC3/4_fs are excluded
fx_ltd_mod <- glm(Daily_surv ~ Station + PC1_fs + PC2_fs + PC1 + PC2, data = ltd_fx, family = binomial())
summary(fx_ltd_mod)

plot(fx_ltd_mod)
qqnorm(resid(fx_ltd_mod)); qqline(resid(fx_ltd_mod))
# qqplot is fine


# Dredging data
options(na.action = "na.fail")
fx.ltd.aic1 <- dredge(fx_ltd_mod, rank = "AIC")
options(na.action = "na.omit")
fx.ltd.imp <- importance(fx.ltd.aic1) # Variable  -importance weights
fx.ltd.tset <- subset(fx.ltd.aic1, delta <2) # Top set of models (<2 delta AIC)
fx.ltd.tmod <- subset(fx.ltd.aic1, delta == 0) # Best approximating model

# Only one top model
top_fx_mod <- glm(Daily_surv ~ PC1_fs + PC2_fs, data = ltd_fx, family = binomial())
summary(top_fx_mod)
plot(top_fx_mod)
car::vif(top_fx_mod)




## Pine Marten ##

# Create dataframe that has missing stations for all predators group
baseline_stations_pm <- subset(baseline_stations, ss %in% c("BG2_10", "BG2_50", "BG2_100", "BG3_10", "BG3_50", "BG3_100", "BH1_10", 
                                                            "BH1_50", "BH1_100", "ML2_10", "ML2_50", "ML2_100", "ML4_50", "ML8_50",
                                                            "ML8_100", "ML9_10", "ML9_100", "NG1_10", "NG1_50", "NG1_100", "NG2_10",
                                                            "NG2_50", "NG2_100"))

baseline_stations_pm <- baseline_stations_pm[,c(1,2)]
baseline_stations_pm$diff_hours <- 336
str(cam_dat_pm_lat)
str(baseline_stations_pm)
baseline_stations_pm$Station <- as.factor(baseline_stations_pm$Station)

ltd_pm <- rbind.fill(baseline_stations_pm, cam_dat_pm_lat)

# Calculate daily survival rates
ltd_pm$Daily_surv <- ltd_pm$diff_hours
ltd_pm$Daily_surv <- ltd_pm$Daily_surv/24
#ltd_pm$Daily_surv <- round(ltd_pm$Daily_surv, digits = 0)
ltd_pm$Daily_surv <- ltd_pm$Daily_surv/14

# Add in PCs
ltd_pm <- merge(fine_hab, ltd_pm, by = c("Site", "Station"))

# Convert to Julian days
ltd_pm$St_date2 <- lubridate::dmy(ltd_pm$St_date) %>%
  as.numeric(format(., format = "%J"))

# Add in landscape scale PCs
names(hab_pc)
names(ltd_pm)
colnames(ltd_pm)[4:7] <- c("PC1_fs", "PC2_fs", "PC3_fs", "PC4_fs")
names(ltd_pm)

# Merge in the landscape PCA data
ltd_pm <- merge(hab_pc, ltd_pm, by = c("Site"))
str(ltd_pm)
hist(ltd_pm$Daily_surv)

# Run pine marten model
pm_ltd_mod <- glm(Daily_surv ~ Station + PC1_fs + PC2_fs + PC1 + PC2, data = ltd_pm, family = binomial())
summary(pm_ltd_mod)

plot(pm_ltd_mod)
qqnorm(resid(pm_ltd_mod)); qqline(resid(pm_ltd_mod))
# Residuals are a bit tailed but neither sqrt nor log transformation fix it

# Dredging data
options(na.action = "na.fail")
pm.ltd.aic1 <- dredge(pm_ltd_mod, rank = "AIC")
options(na.action = "na.omit")
pm.ltd.imp <- importance(pm.ltd.aic1) # Variable  -importance weights
pm.ltd.tset <- subset(pm.ltd.aic1, delta <2) # Top set of models (<2 delta AIC)
pm.ltd.tmod <- subset(pm.ltd.aic1, delta == 0) # Best approximating model

# Top model is null model

# Average model
pm.ltd.avg <-model.avg(pm.ltd.tset)
summary(pm.ltd.avg) 
confint(pm.ltd.avg,level= 0.95)
importance(pm.ltd.avg)




###################################################################################################
#### PREDATOR STUDY GRAPHS FOR MANUSCRIPT
###################################################################################################

library(ggplot2)

# Set theme for all graphs
theme_ac1 <- function(base_family = "sans", base_size_a = 18, base_size_t = 18){
  theme_bw(base_family = base_family) %+replace%
    theme(
      plot.background = element_blank(),
      plot.title = element_text(size=10),
      panel.grid = element_blank(), 
      axis.text = element_text(size = base_size_a),
      axis.title = element_text(size=base_size_t,face="bold"),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.border = element_rect(fill = NA, colour = "black", size=0),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA)
    )
}
# Make a palette with 2 colours of grey for graphs
greypalette_2 <- c("gray90","grey10")


#################################
#### Predator Activity Graph ####
#################################
detach(package:plyr)
library(dplyr)

# Add in the blank values for where no predator turned up at each respective hour
# Load dataframe of blank values
cam_dat_24hrs_blanks <- read.csv("cam_dat_24_hrs_blanks_v2.csv")
# Check to see if dataframe headers match before binding them together
names(cam_dat_24hrs_blanks)
names(cam_dat_firsts_g)
# Use dplyr function of bind_rows
cam_dat_firsts_g <- bind_rows(cam_dat_24hrs_blanks, cam_dat_firsts_g)

# Now change the species group labels to full titles
cam_dat_firsts_g$Sp_gr <- gsub("AV", "Avian predators", cam_dat_firsts_g$Sp_gr)
cam_dat_firsts_g$Sp_gr <- gsub("MA", "Mammalian predators", cam_dat_firsts_g$Sp_gr)

# Rename from Sp_gr to Species group
names(cam_dat_firsts_g)[2] <- "Species_group"

# Write as csv to create graph in excel
#write.csv(cam_dat_firsts_g, "actvity_data.csv")

# Re-order dataframe before grouping into two hour slots
cam_dat_firsts_g <- cam_dat_firsts_g[order(cam_dat_firsts_g[,2], cam_dat_firsts_g[,1]),]

# Write this and group to two hour time slots in excel
#write.csv(cam_dat_firsts_g, "actvity_data_v2.csv")

# Read it in now it's grouped
cam_dat_firsts_g_v2 <- read.csv("actvity_data_v2.csv")


## ACTIVITY GRAPH FOR MANUSCRIPT ##
pred_act_graph <- ggplot(data = cam_dat_firsts_g_v2, aes(x= Capture_hr, y = n, fill = Species_group)) + 
  geom_bar(position = position_dodge(width = 2), stat = "identity", width = 1.8, colour = "black") + 
  ylab("Detections") + xlab("Hour of Day")  +
  scale_fill_manual(values = greypalette_2) + 
  theme_ac1()
# Produce graph
pred_act_graph

# Save graph at 300dpi resolution
#ggsave(
#  "Fig. 4.tiff",
#  dpi = 300
#)



################################################
#### Encounters at Each Distance Band Graph ####
################################################

str(all_enc_g)
# Make tot_enc numeric and station character
all_enc_g$tot_enc <- as.numeric(as.character(all_enc_g$tot_enc))
all_enc_g$Station <- as.character(as.numeric(all_enc_g$Station))
str(all_enc_g)

## All Encounters ##
# Code for the graph


# Create a group-means data set
all_enc_means <- all_enc_g %>% 
  group_by(Station) %>% 
  summarise(
    mean = mean(tot_enc),
    se = sd(tot_enc)/sqrt(n())
  )


## Re-order dataframe for an overall graph
dput(colnames(all_enc_g))

# Avian
avian_df <- all_enc_g[,c("Site", "Station", "av_enc")]
group <- rep("Avian", length(avian_df))
avian_df <- cbind(avian_df, group)

# Mammalian
mammalian_df <- all_enc_g[,c("Site", "Station", "ma_enc")]
group <- rep("Mammalian", length(mammalian_df))
mammalian_df <- cbind(mammalian_df, group)

# All
all_df <- all_enc_g[,c("Site", "Station","tot_enc")]
group <- rep("All", length(all_df))
all_df <- cbind(all_df, group)

# Fox
fx_df <- all_enc_g[,c("Site", "Station","fx_enc")]
group <- rep("Fox", length(fx_df))
fx_df <- cbind(fx_df, group)

# Pine marten
pm_df <- all_enc_g[,c("Site", "Station","pm_enc")]
group <- rep("Pine Marten", length(pm_df))
pm_df <- cbind(pm_df, group)



# Rename column names to "encounters"
names(mammalian_df)
names(avian_df)
names(all_df)
names(fx_df)
names(pm_df)
colnames(mammalian_df)[colnames(mammalian_df)=="ma_enc"] <- "encounters"
colnames(avian_df)[colnames(avian_df)=="av_enc"] <- "encounters"
colnames(all_df)[colnames(all_df)=="tot_enc"] <- "encounters"
colnames(fx_df)[colnames(fx_df)=="fx_enc"] <- "encounters"
colnames(pm_df)[colnames(pm_df)=="pm_enc"] <- "encounters"


# Now combine into one dataframe
av_ma_enc_graph_df <- rbind(avian_df, mammalian_df)
fx_pm_enc_graph_df <- rbind(fx_df, pm_df)
amfp_enc_graph_df <- rbind(av_ma_enc_graph_df, fx_pm_enc_graph_df)
av_ma_all_enc_graph_df <- rbind(amfp_enc_graph_df, all_df)

# Rename count columns
names(av_ma_all_enc_graph_df)
colnames(av_ma_all_enc_graph_df)[4] <- "Group"


## GRAPH FOR MANUSCRIPT ##
# Re-order stations in the dataframe so it's in order when graphed
av_ma_all_enc_graph_df$Station <- factor(av_ma_all_enc_graph_df$Station, levels = c("10", "50", "100"))
# Rename Pine Marten to Pine marten to get rid of capitalisation of marten
av_ma_all_enc_graph_df$Group <- gsub("Pine Marten", "Pine marten", av_ma_all_enc_graph_df$Group)

# Make a palette with 3 colours of grey for graphs
greypalette_3 <- c("gray97", "gray65","grey35")


# This is the latest version
enc_graph_2 <- ggplot(av_ma_all_enc_graph_df, aes(Group, encounters, group = Station, color = Station, fill = Station)) +
  stat_summary(fun = mean, geom = "bar", position= "dodge", width = 0.8, colour = "black") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position=position_dodge(0.8), width = 0.4, colour = "black") +
  theme_ac1() + 
  expand_limits(y = 0:5) +
  scale_fill_manual(values=greypalette_3) +
  scale_x_discrete(limits = c("All", "Mammalian", "Avian", "Fox", "Pine marten"),
                   labels = c("All", "Mammalian", "Avian", "Fox", "Pine marten")) +
  labs(x = "",
       y = "Detections")

# Graph
enc_graph_2

# Save graph at 100dpi resolution
#ggsave(
#  "Fig. 3a.tiff",
#  dpi = 300
#)


#### Descriptives for manuscript ####
names(av_ma_all_enc_graph_df)
encounter_means <- av_ma_all_enc_graph_df %>%
  group_by(Group, Station) %>%
  summarise(
    mean = mean(encounters),
    se = sd(encounters)/sqrt(n())
  )


####################################################
#### Daily Survival at Each Distance Band Graph ####
####################################################

# Merge all the ltd (daily survival) dataframes
# Need to keep station, site, PCs, daily survival
# Need to add species group column

# All predators group
names(ltd_all)
ltd_all_1 <- ltd_all[,c(1,13,2,3,15:18,26:27)]
names(ltd_all_1)
ltd_all_1$sp.group <- "All"
names(ltd_all_1)

# Avian predators group
names(ltd_av)
ltd_av_1 <- ltd_av[,c(1:4,15:18,27,29)]
names(ltd_av_1)
ltd_av_1$sp.group <- "Avian"
names(ltd_av_1)

# Mammalian predators group
names(ltd_ma)
ltd_ma_1 <- ltd_ma[,c(1,13,2,3,15:18,27:28)]
names(ltd_ma_1)
ltd_ma_1$sp.group <- "Mammalian"

# Fox group
names(ltd_fx)
ltd_fx_1 <- ltd_fx[,c(1,13,2:3,15:18,28:29)]
names(ltd_fx_1)
ltd_fx_1$sp.group <- "Fox"

# Pine marten group
names(ltd_pm)
ltd_pm_1 <- ltd_pm[,c(1,13,2,3,15:18,28:29)]
names(ltd_pm_1)
ltd_pm_1$sp.group <- "Pine marten"

# Check all columns are the same
names(ltd_all_1)
names(ltd_ma_1)
names(ltd_av_1)
names(ltd_fx_1)
names(ltd_pm_1)

LTD_df <- rbind(ltd_all_1, ltd_ma_1, ltd_av_1, ltd_fx_1, ltd_pm_1)
str(LTD_df)
LTD_df$Station <- as.factor((LTD_df$Station))

# Daily survival descriptives
LTD_df_means <- LTD_df %>%
  group_by(sp.group, Station) %>%
  summarise(
    mean = mean(Daily_surv),
    se = sd(Daily_surv)/sqrt(n())
  )

# Make this table as a csv
#write.csv(LTD_df_means, "Daily_surv_means.csv")


# Re-order stations in the dataframe so it's in order when graphed
LTD_df$Station <- factor(LTD_df$Station, levels = c("10", "50", "100"))

## GRAPH FOR MANUSCRIPT ##
LTD_graph_new <- ggplot(LTD_df, aes(sp.group, Daily_surv, group = Station, color = Station, fill = Station)) +
  stat_summary(fun = mean, geom = "bar", position= "dodge", width = 0.8, colour = "black") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position=position_dodge(0.8), width = 0.4, colour = "black") +
  theme_ac1() + 
  scale_fill_manual(values= greypalette_3) +
  scale_x_discrete(limits = c("All", "Mammalian", "Avian", "Fox", "Pine marten"),
                   labels = c("All", "Mammalian", "Avian", "Fox", "Pine Marten")) +
  labs(x = "",
       y = "Daily survival")

LTD_graph_new

# Save graph at 300dpi resolution
#ggsave(
#  "Fig. 3b.tiff",
#  dpi = 300
#)





