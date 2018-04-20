#~~~~~~~~~~~~~~~~~~~~~~~ TINY TAGS ~~~~~~~~~~~~~~~~~~#
# Only 2017 data
# Extraction of recesses frequence, mean and ... did by Laurent Montagano (script : "LAREN_inc_geese.R" and data file: "LAREN_inc_geese.csv" )
# Field information of TinyTags are contained in "2017_TT_infos.csv"

rm(list = ls())
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

L <- read.csv("LAREN_inc_geese_v3.csv") # Need to use version 2, because computation error un the first file (registrement frequency: 1min vs 1min15)
C <- read.csv("2017_TT_infos.csv")

summary(L)
# socle = ID of TinyTag
# ttag_exp_start = the starting point of the profil for computation
# ttag_exp_end = the ending point of the profil for computation
# nbrecess = number of recesses
# lengthrecess = total duration of recesses
# inc_prop = proportion of time dedicated to incubation
# meanrecess = mean duration of recesses (= lengthrecess / nbrecess)
# meanfreq = mean frequency of recesses per 24h (nbrecess / nb total de minute * 1400 (soit 24h))

summary(C)
# socle = ID of TinyTag
# DEPLOY = date of deployment (JJ)
# MANIP_SONDE = 0 if there was no TinyTag manipulation, = 1 if there was at least one TinyTag manipulation
# RECUP_... = day (JJ) or hour of the TinyTag recuperation
# ISSUE = 0 for fail and 1 for success
# ISSUE_DATE = day where the nest issue was noted
# CLUTCH = minimal clutch size
# PREDPAR = 1 if there was at least one partial predation, = 0 if there was not


full <- merge(L, C, "socle")
summary(full)

# Obtaining the total duration of TT recording used to compute variables
require(lubridate)
#Setting dates as dates
full$ttag_exp_start <- as.POSIXct(full$ttag_exp_start, tz = "America/Toronto", format = "%Y-%m-%d %H:%M:%S")
full$ttag_exp_end <- as.POSIXct(full$ttag_exp_end, tz = "America/Toronto", format = "%Y-%m-%d %H:%M:%S")
full$ttag_duration <- as.numeric((full$ttag_exp_end - full$ttag_exp_start)*24*60) # in minutes
full$lengthinc <- full$ttag_duration - full$lengthrecess

# To check ... MISMATCH !!!!!
full$inc_propV2 <- full$lengthinc / full$ttag_duration
full[,c(1, 7, 26)]


# Keep variables of interest
TT <- full[, c(1, 4:8, 10, 11, 13, 16, 18:22)]
summary(TT)

# Visual exploration of data
pairs(TT[, -c(1, 2, 3, 7, 9, 15)])

#~~~~~~~~~~~~~~~~~~~~~~~~#
#### inc_prop models ####
#~~~~~~~~~~~~~~~~~~~~~~#

hist(full$inc.prop)

# 0. Null model
full$SUPPL <- relevel(full$SUPPL, "TEM")
inc0 <- glm(cbind(lengthinc, lengthrecess) ~ 1, family = binomial, data = full)
summary(inc0)

# 1. Supplementation effect
inc1 <- glm(cbind(lengthinc, lengthrecess) ~ SUPPL, family = binomial, data = full)
summary(inc1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### meanrecess models ####
#~~~~~~~~~~~~~~~~~~~~~~~~#
hist(full$meanrecess) # POISSON distribution ?

# 0. Null model
rec0 <- glm(meanrecess ~ 1, family = gaussian, data = full)
summary(rec0)

# 1. Supplementation effects
rec1 <- glm(meanrecess ~ SUPPL, family = gaussian, data = full)
summary(rec1)

# 2. Supplementation effects
rec2 <- glm(meanrecess ~ SUPPL, family = poisson, data = full)
summary(rec2)

#~~~~~~~~~~~~~~~~~~~~~~~~#
#### meanfreq models ####
#~~~~~~~~~~~~~~~~~~~~~~#

hist(full$meanfreq)

# 0. Null model
freq0 <- glm(meanfreq ~ 1, family = gaussian, data = full)
summary(freq0)

# 1. Supplementation effects
freq1 <- glm(meanfreq ~ SUPPL, family = gaussian, data = full)
summary(freq1)
