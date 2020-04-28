#### McCarthy et al., 2020 ####
## April 2020 ##
## Models and graphs ##

# Clear R's memory
rm(list=ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load dplyr package
library(dplyr)

# Three sections to this
# Data organising
# Models and statistics
# Graphs

#### Camera Data ####

cam_dat_raw <- read.csv("Camera_trap_data_30.csv")
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
summary(cam_dat_firsts_10)
#FX: 11, HC: 11, PM:9,BG:1,DG:1,JY:1, ST:1

# 50m
cam_dat_firsts_50 <- cam_dat_firsts_g[,-c(1:2,4:9,11:13,16:21)]
cam_dat_firsts_50 <- cam_dat_firsts_50[cam_dat_firsts_50$Station == "50",]
summary(cam_dat_firsts_50)
# PM:8,HC:5,FX:3,ST:2,AM:1

# 100m
cam_dat_firsts_100 <- cam_dat_firsts_g[,-c(1:2,4:9,11:13,16:21)]
cam_dat_firsts_100 <- cam_dat_firsts_100[cam_dat_firsts_100$Station == "100",]
summary(cam_dat_firsts_100)
# HC:8, FX: 7, BG:4, PM:3, ST:2, MG:1


# Delete columns not needed
names(cam_dat_firsts_g)
cam_dat_firsts_g <- cam_dat_firsts_g[,-c(1:9,11:13,16:21)]

## FULL SUMMARY OF ENCOUNTERS ##
summary(cam_dat_firsts_g)
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
# Note the below line of code doesn't work if plyr package is loaded
# Use it later to produce graph of predator activity by species group over 24 hours
cam_dat_firsts_g <- cam_dat_firsts_g %>% group_by(Capture_hr) %>% count(Sp_gr)






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

# Now add onto this the relevant data
ma_1 <- cam_dat_firsts[cam_dat_firsts$Sp_gr == "MA",]
# Remove NA rows
ma_1 <- na.omit(ma_1)
# Delete unneeded columns
names(ma_1)
ma_1 <- ma_1[,-c(1,4:12,14:19)]
# Get a count of encounters at each site/station combination
ma_enc <- plyr::ddply(ma_1, .(Site, Station), transform, count = length(Pred_enc_30.mins))
# Get rid of duplicates
ma_enc <- ma_enc[!duplicated(ma_enc$ss),]
# Mammalian predators were detected at 17 stations

# Delete uneeded columns
ma_enc <- ma_enc[,-c(3:5)]

# Merge this back in with the base data
# The all argument at the end keeps everything even when there is no merge possible due to NAs
ma_enc <- merge(base_data, ma_enc, by = c("Site", "Station"), all = TRUE)
# Replace NAs with 0
ma_enc[is.na(ma_enc)] <- 0


## Merge Total, Avian and Mammalian encounters into one dataframe ##
# First change the names of the count columns as they are all called "count" at the moment
names(tot_enc) <- c("Site", "Station", "tot_enc")
names(tot_enc)
names(av_enc) <- c("Site", "Station", "av_enc")
names(av_enc)
names(ma_enc) <- c("Site", "Station", "ma_enc")
names(ma_enc)

all_enc <- merge(tot_enc, av_enc, by = c("Site", "Station"))
all_enc <- merge(all_enc, ma_enc, by = c("Site", "Station"))

# Make another copy of the all_enc dataframe for graphing
all_enc_g <- all_enc





#### Latency to first detection ####

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








###########################################################################################################
#### Modelling and Statistics ####


# Load packages
library(plyr)
library(PMCMR)
library(PMCMRplus)
library(glmmTMB)
#install.packages("TMB", type = "source")
library(TMB)
library(DHARMa)


#########################
## ENCOUNTERS - MODELS ##
#########################


# Vegetation PCs vs. distance band
site_info <- read.csv("site_info.csv")

# Merge site_info with all_enc
all_enc_mod <- merge(all_enc, site_info, by = c("Site", "Station"))

# Check variables are correct types
str(all_enc_mod)
# Chance tot_enc to numeric
all_enc_mod$tot_enc <- as.numeric(as.character(all_enc_mod$tot_enc))
# Change station to factor
all_enc_mod$Station <- as.factor(all_enc_mod$Station)


# Check distribution of the data
hist(all_enc_mod$tot_enc)
hist(all_enc_mod$av_enc)
hist(all_enc_mod$ma_enc)
# Transforming these data will not normalise them
# These data are zero-inflated



#### Total Encounters ####
# Zero-inflated model
tot_enc_basic <- glmmTMB(tot_enc ~ Station + (1|Site) + (1|Date), data = all_enc_mod, family = nbinom2)
summary(tot_enc_basic)
tot_enc_simres_basic <- simulateResiduals(tot_enc_basic)
plot(tot_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look okay
# Overdispersion is 2.67
# No significant difference between distance bands

# Conduct additional tests
testUniformity(tot_enc_simres_basic)
testDispersion(tot_enc_simres_basic)
testZeroInflation(tot_enc_simres_basic)

overdisp_fun <- function(tot_enc_basic) {
  rdf <- df.residual(tot_enc_basic)
  rp <- residuals(tot_enc_basic,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(tot_enc_basic)
# Ratio of 1.04 is near 1 which is fine




#### Avian Encounters ####
# Zero-inflated model
# Site has been excluded as a random factor as including it creates an overdispersion issue
av_enc_basic <- glmmTMB(av_enc ~ Station + (1|Date), data = all_enc_mod, family = nbinom2)
summary(av_enc_basic)
av_enc_simres_basic <- simulateResiduals(av_enc_basic)
plot(av_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look acceptable
# Overdispersion is 0.903
# No significant difference between distance bands

# Conduct additional tests for overdispersion
testUniformity(av_enc_simres_basic)
testDispersion(av_enc_simres_basic)
testZeroInflation(av_enc_simres_basic)

overdisp_fun <- function(av_enc_basic) {
  rdf <- df.residual(av_enc_basic)
  rp <- residuals(av_enc_basic,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(av_enc_basic)
# Ratio is 0.538




#### Mammalian Encounters ####
# Zero-inflated model
mam_enc_basic <- glmmTMB(ma_enc ~ Station + (1|Site) + (1|Date), data = all_enc_mod, family = nbinom2)
summary(mam_enc_basic)
mam_enc_simres_basic <- simulateResiduals(mam_enc_basic)
plot(mam_enc_simres_basic, rank = T)
# QQ plot looks okay
# Simulated residuals look okay
# Overdispersion is 0.759 so it's okay
# No statistically significant difference between distance bands

# Conduct additional tests for overdispersion
testUniformity(mam_enc_simres_basic)
testDispersion(mam_enc_simres_basic)
testZeroInflation(mam_enc_simres_basic)

overdisp_fun <- function(mam_enc_basic) {
  rdf <- df.residual(mam_enc_basic)
  rp <- residuals(mam_enc_basic,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(mam_enc_basic)
# Ratio of 0.94, so overdisperion is not an issue






#########################################
## LATENCY TO FIRST DETECTION - MODELS ##
#########################################

# load packages for linear mixed effects model
library(lme4)
library(lubridate)
library(lmerTest)

# Change diff_time to numeric values so that lmer can work
str(cam_dat_lat)
str(cam_dat_ma_lat)
str(cam_dat_av_lat)
cam_dat_lat$diff_hours <- as.numeric(as.character(cam_dat_lat$diff_hours))
cam_dat_ma_lat$diff_hours <- as.numeric(as.character(cam_dat_ma_lat$diff_hours))
cam_dat_av_lat$diff_hours <- as.numeric(as.character(cam_dat_av_lat$diff_hours))

# Ensure stations is a factor and not continuous
str(cam_dat_lat)
cam_dat_lat$Station <- as.factor(cam_dat_lat$Station)
str(cam_dat_ma_lat)
cam_dat_ma_lat$Station <- as.factor(cam_dat_ma_lat$Station)
str(cam_dat_av_lat)
cam_dat_av_lat$Station <- as.factor(cam_dat_av_lat$Station)

# Histograms to check for distribution
hist(cam_dat_lat$diff_hours)
hist(cam_dat_ma_lat$diff_hours)
hist(cam_dat_av_lat$diff_hours)

# Test normality of distribution
shapiro.test(cam_dat_lat$diff_hours)
shapiro.test(cam_dat_ma_lat$diff_hours)
shapiro.test(cam_dat_av_lat$diff_hours)

# cam_dat_lat and cam_dat_ma_lat are not normally distributed
# Log transform data to normalise
cam_dat_lat$diff_hours_l <- log(cam_dat_lat$diff_hours)
hist(cam_dat_lat$diff_hours_l)
shapiro.test(cam_dat_lat$diff_hours_l)
# The transformed data are now normally distributed

# Log transform data to normalise
cam_dat_ma_lat$diff_hours_l <- log(cam_dat_ma_lat$diff_hours)
hist(cam_dat_ma_lat$diff_hours_l)
shapiro.test(cam_dat_ma_lat$diff_hours_l)
# The transformed data are now normally distributed



## All LTD Model ##
# Build the model
all_LTD_m <- lmer(diff_hours_l ~ Station + (1 | Site) + (1| Capture_date), data = cam_dat_lat, REML = TRUE)
# Singular fit due to Site as a random effect, inclusion/exclusion of site has no affect on any outputs

# Check the model assumptions
plot(all_LTD_m)
qqnorm(resid(all_LTD_m)); qqline(resid(all_LTD_m))
# Slightly tailed but acceptable

# Examine the model outputs
summary(all_LTD_m)
anova(all_LTD_m)

# Post-hoc test between stations
difflsmeans(all_LTD_m, test.effs = "Station")
# Significant difference between 10m and 50m




## Mammalian LTD Model ##
# Build the model
ma_LTD_m <- lmer(diff_hours_l ~ Station + (1 | Site) + (1| Capture_date), data = cam_dat_ma_lat, REML = TRUE)
# Singular fit due to Site as a random effect, inclusion/exclusion of site has no affect on any outputs

# Check the model assumptions
plot(ma_LTD_m)
qqnorm(resid(ma_LTD_m)); qqline(resid(ma_LTD_m))
# Slightly tailed but acceptable given the samll sample size

# Examine the model outputs
summary(ma_LTD_m)
anova(ma_LTD_m)

# Post-hoc test between stations
difflsmeans(ma_LTD_m, test.effs = "Station")
# Significant difference between 10m and 50m



## Avian LTD Model ##
# Build the model
av_LTD_m <- lmer(diff_hours ~ Station + (1 | Site) + (1| Capture_date), data = cam_dat_av_lat, REML = TRUE)

# Check the model assumptions
plot(av_LTD_m)
qqnorm(resid(av_LTD_m)); qqline(resid(av_LTD_m))

# Examine the model outputs
summary(av_LTD_m)
anova(av_LTD_m)
# Residuals not normal

# Use non-parametric test
kruskal.test(diff_hours ~ Station, data = cam_dat_av_lat)
# Post-hoc test
posthoc.kruskal.dunn.test(diff_hours ~ Station, data = cam_dat_av_lat, p.adjust="BH")
# No differences between distance bands





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
# Run the code from line 1 to line 90 and nothing in between for the below to run

# Add in the blank values for where no predator turned up at each respective hour
# Load dataframe of blank values
cam_dat_24hrs_blanks <- read.csv("cam_dat_24_hrs_blanks.csv")
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

# Alternatively, make graph with code:
pred_act_graph <- ggplot(data = cam_dat_firsts_g, aes(x= Capture_hr, y = n, fill = Species_group)) + 
  geom_bar(position = position_dodge(width = 1), stat = "identity", width = .8, colour = "black") + 
  ylab("Encounters") + xlab("Hour of Day")  +
  scale_fill_manual(values = greypalette_2) + 
  theme_ac1()
# Produce graph
pred_act_graph

# Save graph at 300dpi resolution
ggsave(
  "activity_graph.tiff",
  dpi = 300
)



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
# Avian
dput(colnames(all_enc_g))

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

# Rename column names to "encounters"
names(mammalian_df)
names(avian_df)
names(all_df)
colnames(mammalian_df)[colnames(mammalian_df)=="ma_enc"] <- "encounters"
colnames(avian_df)[colnames(avian_df)=="av_enc"] <- "encounters"
colnames(all_df)[colnames(all_df)=="tot_enc"] <- "encounters"

# Now combine into one dataframe
av_ma_enc_graph_df <- rbind(avian_df, mammalian_df)
av_ma_all_enc_graph_df <- rbind(av_ma_enc_graph_df, all_df)

all_av_enc_graph_df <- rbind(all_df, avian_df)
av_ma_all_enc_graph_df <- rbind(all_av_enc_graph_df, mammalian_df)



## GRAPH FOR MANUSCRIPT ##
# Avian, Mammalian and total encounters in one graph
av_ma_enc_graph_v1 <- ggplot(av_ma_all_enc_graph_df, aes(Station, encounters, group = group, color = group)) +
  #  geom_jitter(position = position_jitter(width = 0.5)) +
  stat_summary(fun.y = mean, geom = "point", size = 5, position=position_dodge(0.5)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, position=position_dodge(0.5)) +
  geom_point(size = 3, shape = 20, position = position_jitterdodge(jitter.width=0.2,jitter.height=0.25),aes(group=group),alpha=0.5) + 
  theme_ac1() + 
  expand_limits(y = 0:10) +
  scale_color_manual(name = "Species group", values=c("red", "blue", "black")) +
  scale_x_discrete(limits = c("10", "50", "100"),
                   labels = c("10m", "50m", "100m")) +
  labs(x = "",
       y = "Detections")

# Note that jitter makes points change slightly each time code is run
av_ma_enc_graph_v1

# Save graph at 300dpi resolution
ggsave(
  "encounters_graph.tiff",
  dpi = 300
)


#### Descriptives for manuscript ####
# Write standard error function
ste <- function(x) sd(x)/sqrt(length(x))

# Predator groups #
all_preds <- av_ma_all_enc_graph_df[av_ma_all_enc_graph_df$group == "All",]
avian_preds <- av_ma_all_enc_graph_df[av_ma_all_enc_graph_df$group == "Avian",]
mammalian_preds <- av_ma_all_enc_graph_df[av_ma_all_enc_graph_df$group == "Mammalian",]

# 10m mean/SE number of detections of each group
all_preds_10 <- all_preds[all_preds$Station == "10",]
mean(all_preds_10$encounters)
# 3.5
ste(all_preds_10$encounters)
# 0.87

avian_preds_10 <- avian_preds[avian_preds$Station == "10",]
mean(avian_preds_10$encounters)
# 1.2
ste(avian_preds_10$encounters)
# 0.73

mammalian_preds_10 <- mammalian_preds[mammalian_preds$Station == "10",]
mean(mammalian_preds_10$encounters)
# 2.3
ste(mammalian_preds_10$encounters)
# 0.9




################################################################
#### Latency to First Detection at Each Distance Band Graph ####
################################################################

## Avian and Mammalian predators combined ##
# Rename both avian and mammalian datasets
cam_dat_av_lat_g <- cam_dat_av_lat
cam_dat_ma_lat_g <- cam_dat_ma_lat
cam_dat_lat_g <- cam_dat_lat
# Rename species for each dataset
cam_dat_av_lat_g$sp.group <- "Avian"
cam_dat_ma_lat_g$sp.group <- "Mammalian"
cam_dat_lat_g$sp.group <- "All"
# Make coumns the same
names(cam_dat_ma_lat_g)
names(cam_dat_av_lat_g)
cam_dat_ma_lat_g$diff_hours_l <- NULL

# Combine datasets
avma <- rbind(cam_dat_av_lat_g, cam_dat_ma_lat_g)
names(avma)
names(cam_dat_lat_g)
cam_dat_lat_g$start <- NULL
library(plyr)
df <- rbind.fill(avma, cam_dat_lat_g)


# Latency to First Detection Graph
## GRAPH FOR MANUSCRIPT ##
LTD_graph <- ggplot(df, aes(Station, diff_hours, group = sp.group, color = sp.group)) +
  geom_point(size = 3, shape = 20, position = position_dodge(width = 0.5),aes(group=sp.group), alpha = 0.5) + 
  stat_summary(fun.y = mean, geom = "point", size = 5, position=position_dodge(0.5)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, position=position_dodge(0.5)) +
  theme_ac1() + 
  expand_limits(y = 0:350) +
  scale_color_manual(name = "Species group", values=c("red", "blue", "black")) +
  scale_x_discrete(limits = c("10", "50", "100"),
                   labels = c("10m", "50m", "100m")) +
  labs(x = "",
       y = "Latency to detection (hours)")

LTD_graph

# Save graph at 300dpi resolution
ggsave(
  "LTD_graph.tiff",
  dpi = 300
)

