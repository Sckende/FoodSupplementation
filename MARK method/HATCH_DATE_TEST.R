getwd()
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
list.files()

h <- read.table("GOOSE-Supl_Nests_2015-2016-2017_LAST_version.txt", h = T, dec = ".", sep = "\t") # all years, with and without supplemented nests
str(h)
dim(h)
summary(h) 

#################### Data subset ####################
# Keep only true dates of initiation and hatching
hh <- subset(h, h$INI_STATUS == "TRUE")
hh <- subset(hh, hh$HATCH_STATUS == "TRUE" | hh$HATCH_STATUS == "MARIE_EST/TRUE")

# Keep only the successfull nests
hh <- subset(hh, hh$NIDIF == "S")
hh <- droplevels(hh)
summary(hh)

hh$INITIATION <- as.numeric(as.character(hh$INITIATION))
hh$HATCH <- as.numeric(as.character(hh$HATCH))

#### Supplementation effect on the hatching date ####

# Add rainfall year type (LOW/INTERMEDIATE/HIGH)
cum <- read.table("PREC_cum2.txt", dec = ".", h = T, sep = " ")
summary(cum)
hh$RAINFALL <- cum$RAINFALL[match(hh$YEAR, cum$YEAR)]

summary(hh)

# Add PER CAPITA the cumulative rainfall, cumulative rainfall per day and mean temperature from initiation date to hatching date 

rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", h = T, dec = ","); rain <- na.omit(rain)
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h = T, dec = ",")

# Date conversion for temp data 
temp$JJ <- strptime(paste(temp$DAY, temp$MONTH, temp$YEAR, sep = "-"), format = "%d-%m-%Y")
temp$JJ <- temp$JJ$yday + 1 #c

hh$ID <- 1:53
TAB <- NULL
for (i in hh$ID) {
  
  ID <- i
  INCUB <- hh$HATCH[i] - hh$INITIATION[i]
  cumPREC <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]])
  PREC_rate <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]]) / INCUB
  mean_TEMP <- mean(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  sd_TEMP <- sd(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  
  M <- c(ID, INCUB, cumPREC, PREC_rate, mean_TEMP, sd_TEMP)
  TAB <- as.data.frame(rbind(TAB, M))
  }
  View(TAB)
  names(TAB) <- c("ID", "INCUB", "cumPREC", "PREC_rate", "mean_TEMP", "sd_TEMP")
  
  hh <- merge(hh, TAB, by ="ID")

# MODELES

plot(hh$INCUB)
hist(hh$INCUB)

require(lme4)

# Full model
l0 <- lmer(INCUB ~ SUPPL + HAB + cumPREC + mean_TEMP + (1|YEAR), data = hh, REML = F) # REML = F allows comparison between likelihood estimator of different models 
l0
summary(l0)

# Null model
l.NULL <- lmer(INCUB ~ cumPREC + (1|YEAR), data = hh, REML = F)
l.NULL
summary(l.NULL)

# Model comparison
anova(l.NULL, l0)

