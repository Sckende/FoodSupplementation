getwd()
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())

gsg <- read.table("GOOSE_MARK_all_nests_all_years.txt", h = T, dec = ".", sep = "\t") # all years, with and without supplemented nests
str(gsg)
dim(gsg)
summary(gsg) 

#################### Cleaning of data ####################

gsg$HABITAT[gsg$HABITAT == "Mesic"] <- "MES"
gsg$HABITAT[gsg$HABITAT == "Wetland"] <- "WET"

gsg$SUPPL[gsg$SUPPL == "FOO"] <- "F"
gsg$SUPPL[gsg$SUPPL == "WAT"] <- "W"

gsg$Groupe[gsg$Groupe == "Colony"] <- "COLONY"

gsg <- droplevels(gsg)
# Formating the reference level
gsg$SUPPL <- relevel(gsg$SUPPL, "TEM")

# Delete WF level in SUPPL
gsg <- gsg[gsg$SUPPL != "WF",]
gsg <- gsg[gsg$SUPPL != "NONE",]

# Nest ISSUE 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
gsg$Fate[gsg$Groupe == "COLONY" & gsg$ISSUE == 1] <- 0
gsg$Fate[gsg$Groupe == "COLONY" & gsg$ISSUE == 2 | gsg$ISSUE == 3] <- 1


# Delete the ISSUE variable
gsg <- gsg[,-11]

gsg$Fate <- as.factor(gsg$Fate)
gsg <- droplevels(gsg)

# Remove NAs
gsg$SupplDate[is.na(gsg$SupplDate)] <- 99999
gsg <- na.omit(gsg)


#### Supplementation effect on the hatching date ####
# Using gsg database after cleaning code
# Keep only experimental years, i.e. 2015, 2016, 2017
try <- gsg[gsg$AN == 2015 | gsg$AN == 2016 | gsg$AN == 2017,]

# Keep only nest with success
try <- try[try$Fate == 0,]

# Check if all lastpresent date = lastchecked date
all(try$LastChecked %in% try$LastPresent)
# More right to use
identical(try$LastChecked, try$LastPresent)

# Add rainfall year type (LOW/INTERMEDIATE/HIGH)
cum <- read.table("PREC_cum2.txt", dec = ".", h = T, sep = " ")
summary(cum)
try$RAINFALL <- cum$RAINFALL[match(try$AN, cum$YEAR)]

summary(try)

# Add PER CAPITA the cumulative rainfall, cumulative rainfall per day and mean temperature from initiation date to hatching date 

# WARNING ! Here the LastChecked is considered as the hatching date. Could be more acurate ! 

rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", h = T, dec = ","); rain <- na.omit(rain)
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h = T, dec = ",")

try$ID <- 1:4248
try$AN <- as.numeric(try$AN)
for (i in unique(try$AN)) {
  try$
}
#### A FINIR ####

# MODELES

plot(try$LastChecked)
hist(try$LastChecked)

require(lme4)

# Full model
l0 <- lmer(LastChecked ~ SUPPL + (1|AN), data = try, REML = F) # REML = F allows comparison between likelihood estimator of different models 
l0
summary(l0)

# Null model
l.NULL <- lmer(LastChecked ~ 1  + (1|AN), data = try, REML = F)
l.NULL
summary(l.NULL)

# Model comparison
anova(l.NULL, l0)

