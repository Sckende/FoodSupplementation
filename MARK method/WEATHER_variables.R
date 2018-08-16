############ Climate data for supplementation experiment ########
rm(list = ls())
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### RAINFALL ####
rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", dec = ",", h = T)

summary(rain)
rain <- na.omit(rain)

#### cum - Trends between 1995 and 2017 -  Complete data ####
cum <- as.data.frame(tapply(rain$RAIN, rain$YEAR, sum))

cum <- cbind(1995:2017, cum)
names(cum) <- c("YEAR", "cumRAIN")
cum$meanCUMrain <- mean(cum$cumRAIN)

# Plot
my_vector <- cum$cumRAIN
names(my_vector) <- cum$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        main = "Trend for the complete field season",
        ylab = "Cumulative precipitation (mm)")
abline(h = mean(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum$cumRAIN) - sd(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN) + sd(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")

#### cum0 - Trends between 1995 and 2017 -  Complete summer (from 1 June to 20 August - Report Bylot 2017) ####
cum0 <- as.data.frame(tapply(rain$RAIN[rain$JJ >= 152 & rain$JJ <= 232], rain$YEAR[rain$JJ >= 152 & rain$JJ <= 232], sum))

cum0 <- cbind(1995:2017, cum0)
names(cum0) <- c("YEAR", "cumRAIN")
cum0$meanCUMrain <- mean(cum0$cumRAIN)

# Plot
my_vector <- cum0$cumRAIN
names(my_vector) <- cum0$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        main = "Trend for the complete summer (1 June to 20 August)",
        ylab = "Cumulative precipitation (mm)")
abline(h = mean(cum0$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum0$cumRAIN) - sd(cum0$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum0$cumRAIN) + sd(cum0$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")

#### cum 1 & cum11 - Trends between 1995 and 2017 -  Average goose nidification period (12 June - 25 July) ####
## Cf dates in Lecomte et al. 2009
## Without taking account of bissextile and non bissextile years
cum1 <- as.data.frame(tapply(rain$RAIN[rain$JJ >= 163 & rain$JJ <= 206], rain$YEAR[rain$JJ >= 163 & rain$JJ <= 206], sum))

cum1 <- cbind(1995:2017, cum1)
names(cum1) <- c("YEAR", "cumRAIN")
cum1$meanCUMrain <- mean(cum1$cumRAIN)
# Plot
my_vector <- cum1$cumRAIN
names(my_vector) <- cum1$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        main = "Trend for goose nidification period (12 June to 25 July)",
        ylab = "Cumulative precipitation (mm)")
abline(h = mean(cum1$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum1$cumRAIN) - sd(cum1$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum1$cumRAIN) + sd(cum1$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")

## By taking account of bissextile and non bissextile years
ann0 <- rain[rain$YEAR == 1995 | rain$YEAR == 1997 | rain$YEAR ==1998 | rain$YEAR == 1999 | rain$YEAR == 2001 | rain$YEAR == 2002 | rain$YEAR == 2003 | rain$YEAR == 2005 | rain$YEAR == 2006 | rain$YEAR == 2007 | rain$YEAR == 2009 | rain$YEAR == 2010 | rain$YEAR == 2011 | rain$YEAR == 2013 | rain$YEAR == 2014 | rain$YEAR == 2015 | rain$YEAR == 2017,] # non bissextile years
ann1 <- rain[rain$YEAR == 1996 | rain$YEAR == 2000 | rain$YEAR == 2004 | rain$YEAR == 2008 | rain$YEAR == 2012 | rain$YEAR == 2016,] #bissextile years

# non bissextile years
part0 <- as.data.frame(tapply(ann0$RAIN[ann0$JJ >= 163 & ann0$JJ <= 206], ann0$YEAR[ann0$JJ >= 163 & ann0$JJ <= 206], sum)); part0 <- cbind(c(1995, 1997:1999, 2001:2003, 2005:2007, 2009:2011, 2013:2015, 2017), part0); names(part0) <- c("YEAR","cumRAIN") 

# non bissextile years
part1 <- as.data.frame(tapply(ann1$RAIN[ann1$JJ >= 164 & ann1$JJ <= 207], ann1$YEAR[ann1$JJ >= 164 & ann1$JJ <= 207], sum)); part1 <- cbind(c(1996, 2000, 2004, 2008, 2012, 2016), part1); names(part1) <- c("YEAR","cumRAIN") 


cum11 <- rbind(part0, part1)

require(dplyr)
cum11 <- arrange(cum11, YEAR)

cum11$meanCUMrain <- mean(cum11$cumRAIN)

# Plot
my_vector <- cum11$cumRAIN
names(my_vector) <- cum11$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        main = "Trend for goose nidification period (12 June to 25 July) with bissextile and non bissextile years",
        ylab = "Cumulative precipitation (mm)")
abline(h = mean(cum11$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum11$cumRAIN) - sd(cum11$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum11$cumRAIN) + sd(cum11$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")

#### cum2 - Trends between 1995 and 2017 -  Specific dates for each goose nidification period *** ####
nidi <- read.table("GOOSE_jour_moy_ponte_eclos.txt", sep = "\t", dec = ".", h = T)
summary(nidi)

cum2 <- NULL
for (i in unique(rain$YEAR)) {
  YEAR <- i
  INI <- nidi$moy_ponte[nidi$year == i]
  ECLO <- nidi$moy_eclos[nidi$year == i]
  
  cumRAIN <- sum(rain$RAIN[rain$YEAR == i & rain$JJ >= INI & rain$JJ <= ECLO])
  
  TAB <- data.frame(YEAR, INI, ECLO, cumRAIN)
  
  cum2 <- rbind(cum2, TAB)
}
cum2$RAINFALL[cum2$cumRAIN <= mean(cum2$cumRAIN) - sd(cum2$cumRAIN)] <- "LOW"
cum2$RAINFALL[cum2$cumRAIN >= mean(cum2$cumRAIN) + sd(cum2$cumRAIN)] <- "HIGH"
cum2$RAINFALL[cum2$cumRAIN <= mean(cum2$cumRAIN) + sd(cum2$cumRAIN) & cum2$cumRAIN >= mean(cum2$cumRAIN) - sd(cum2$cumRAIN)] <- "INTER"
cum2$RAINFALL <- as.factor(cum2$RAINFALL)

# Check the duration of each nesting season
cum2$NEST_DURATION <- cum2$ECLO - cum2$INI

# cumRAIN/day to compensate the different length of nesing period
cum2$cumRAIN_DAY <- cum2$cumRAIN / cum2$NEST_DURATION

#write.table(cum2, "PREC_cum2.txt", dec = ".")


# Plot
#png("GOOSE_prec.tiff",
    #res=300,
    #width=20,
    #height=15,
    #pointsize=12,
    #unit="cm",
    #bg="transparent")

x11()
par(mar=c(5,5,1,1)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
my_vector <- cum2$cumRAIN
names(my_vector) <- cum2$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
       # main = "Trend for specific dates for each goose nesting period",
       # ylab = "Cumulative precipitation (mm)",
        yaxt = "n",
       ylim = c(0, 70))
axis(side = 2,
     lwd = 1,
     las = 2)
abline(h = mean(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum2$cumRAIN) - sd(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum2$cumRAIN) + sd(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 1.5,
       lty = "dotdash")

#dev.off()

#### cum3 - Annual cumulation prec between the earliest initiation and latest hatching ####

ni <- read.table("GOOSE_MARK_Complete_data.txt", h = T, sep = ",")
ni2<- as.data.frame(tapply(ni$INITIATION, ni$YEAR, min))
ni2 <- cbind(1995:2017, ni2)
ni2 <- cbind(ni2, as.data.frame(tapply(ni$LastChecked, ni$YEAR, max)))
names(ni2) <- c("YEAR", "early_INI", "late_HATCH")

cum3 <- NULL
for (i in unique(rain$YEAR)) {
  YEAR <- i
  INI <- ni2$early_INI[ni2$YEAR == i] 
  ECLO <- ni2$late_HATCH[ni2$YEAR == i]
  
  cumRAIN <- sum(rain$RAIN[rain$YEAR == i & rain$JJ >= INI & rain$JJ <= ECLO])
  
  TAB <- data.frame(YEAR, INI, ECLO, cumRAIN)
  
  cum3 <- rbind(cum3, TAB)
}


#### Correlation between cum11$cumRAIN (cumulative rain between same dates for each nesting season) and cum2$cumRAIN (cumulative rain specific to each nesting season)
plot(cum11$cumRAIN, cum2$cumRAIN)
cor.test(cum11$cumRAIN, cum2$cumRAIN)

##### DATA EXPLORATION #####
#### Trends between 2013 and 2017
my_vector2013 <- cum$cumRAIN[cum$YEAR >= 2013]
names(my_vector2013) <- cum$YEAR[cum$YEAR >= 2013]
barplot(my_vector2013,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2013]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "dotdash")

#### Trends with 2005, 2015, 2016 & 2017
my_vectorSUPPL <- cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]
names(my_vectorSUPPL) <- cum$YEAR[cum$YEAR == 2005 | cum$YEAR >= 2015]
barplot(my_vectorSUPPL,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "dotdash")

abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]) - sd(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 2,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]) + sd(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 2,
       lty = "dotdash")

#### Trends between 2005 and 2017
my_vector <- cum$cumRAIN[cum$YEAR >= 2005]
names(my_vector) <- cum$YEAR[cum$YEAR >= 2005]
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]) - sd(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]) + sd(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")


#### Partial water vapor pressure - p(H2O)  ####

# Buck equation
# p = 0.61121 exp((18.678 - T/234.5)(T/(257.14 + T)))
# T is the ambiant temperature in C
# P is in kPa
list.files()
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
summary(temp)

deg <- read.table("TEMP_PondInlet_2015-2017.txt", h = T, dec =".", sep = "\t")
summary(deg)


temp$pH2O <- 0.61121 * exp((18.678 - temp$TEMP / 234.5) * (temp$TEMP/(257.14 + temp$TEMP)))

# Goff-Gratch equation

Tst <- 373.15
#T0 <- temp$TEMP
T0 <- deg$Mean_Temp
Est <- 1013.25

deg$log_pH2O <- - 7.90298 * (Tst / (T0 - 1)) + 5.02808 * log(Tst / T0) - 1.3816 * 10^(-7) * (10^(11.344 * ((1 - T0)/Tst)) - 1) + 8.1328 * 10^(-3) * (10^(-3.49149 * (Tst / (T0-1))) - 1) + log(Est)

deg$pH2O <- 10^(deg$log_pH2O) 
#### WEIRDO RESULTS !!!!!!!!!!! #####

#### AIR TEMPERATURE ####
### BYLCAMP station data ####
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h = T, sep = "\t", dec = ",")
head(temp)
summary(temp)

  # JJ date
temp$JJ <- strptime(paste(temp$DAY, temp$MONTH, temp$YEAR, sep = "-"), format = "%d-%m-%Y")
temp$JJ <- temp$JJ$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires

  # Calcul
t <- NULL
for (i in 1996:2016) {
  YEAR <- i
  INI <- nidi$moy_ponte[nidi$year == i]
  ECLO <- nidi$moy_eclos[nidi$year == i]
  
  meanTEMP <- mean(temp$TEMP[temp$YEAR == i & temp$JJ >= INI & temp$JJ <= ECLO], na.rm = T)
  sdTEMP <- sd(temp$TEMP[temp$YEAR == i & temp$JJ >= INI & temp$JJ <= ECLO], na.rm = T)
  
  TAB <- data.frame(YEAR, INI, ECLO, meanTEMP, sdTEMP)
  
  t <- rbind(t, TAB)
}


### To cope with temperature missing data of BYLCAMP station ####


########### Correlation between both spring and summer temperature ???

cal <- split(temp, temp$YEAR)
summary(cal)
#str(cal)

# Years with different day number between spring and summer (so, impossible to apply a correlation test)
TABcor <- as.data.frame(matrix(data = NA, nrow = 29, ncol = 4))
names(TABcor) <- c("year", "cor_est", "p_val", "STARS")
TABcor$year <- 1989:2017

x11()
par(mfrow = c(5, 5))
for (i in 1:29) {
  if (dim(cal[[i]][cal[[i]]$SAISON == "E",])[1] != dim(cal[[i]][cal[[i]]$SAISON == "P",])[1]) {
    print(unique(cal[[i]]$YEAR))
    print(i)
    TABcor$cor_est[TABcor$year == unique(cal[[i]]$YEAR)] <- "-"
    TABcor$p_val[TABcor$year == unique(cal[[i]]$YEAR)] <- "-"
    TABcor$STARS[TABcor$year == unique(cal[[i]]$YEAR)] <- "-"
  } else {
    plot(cal[[i]]$TEMP[cal[[i]]$SAISON == "P"], cal[[i]]$TEMP[cal[[i]]$SAISON == "E"], main = unique(cal[[i]]$YEAR), xlab = "SPRING", ylab ="SUMMER")
    p <- cor.test(cal[[i]]$TEMP[cal[[i]]$SAISON == "P"], cal[[i]]$TEMP[cal[[i]]$SAISON == "E"], method = "spearman")
    TABcor$cor_est[TABcor$year == unique(cal[[i]]$YEAR)] <- as.numeric(as.character(p$estimate))
    TABcor$p_val[TABcor$year == unique(cal[[i]]$YEAR)] <- as.numeric(as.character(p$p.value))
    if (p$p.value <= 0.001) {TABcor$STARS[TABcor$year == unique(cal[[i]]$YEAR)] ="***"
    } else {
      if(p$p.value <= 0.01 ) {TABcor$STARS[TABcor$year == unique(cal[[i]]$YEAR)] ="**"
      } else {
        if(p$p.value <= 0.05 ) {TABcor$STARS[TABcor$year == unique(cal[[i]]$YEAR)] ="*"
        } else { TABcor$STARS[TABcor$year == unique(cal[[i]]$YEAR)] ="NS"}
  }}       
  }}

View(TABcor[TABcor$STARS != "NS" & TABcor$STARS != "-",])

########## Use the Pond Inlet temperature data (Governement of Canada) 
rm(list = ls())
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
deg <- read.table("TEMP_PondInlet_2015-2017.txt", h = T, dec =".", sep = "\t")
summary(deg)

deg$JJ <- strptime(as.character(deg$Date), format = "%Y-%m-%d")
deg$JJ <- deg$JJ$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires

# Supplemented nests data
suppl <- read.table("GOOSE_MARK_all_SUPPL_nests_all_years.txt", h = T, dec = ".", sep = "\t", stringsAsFactors = FALSE) # Supplemented nests data
# Conversion of data
# SUPPL_DATE
table(suppl$SUPPL_DATE, useNA = "always")
suppl$SUPPL_DATE[suppl$SUPPL_DATE == "NONE"] <- "999"

# INITIATION
table(suppl$INITIATION, useNA = "always")
suppl$INITIATION <- as.numeric(suppl$INITIATION)
summary(suppl$INITIATION)

# HATCH
table(suppl$HATCH, useNA = "always")
suppl$HATCH <- as.numeric(suppl$HATCH)
summary(suppl$HATCH)

# NIDIF
# S = success = 0 in MARK
# F = fail = 1 in MARK

suppl$Fate[which(suppl$NIDIF == "S")] <- 0
suppl$Fate[which(suppl$NIDIF == "F")] <- 1

table(suppl$Fate, useNA = "always")

# Need to correct the "LastPresent" and "LastVisit" date by the hatching date ONLY FOR SUCCESSFUL NESTS
suppl$LastPresent[which(suppl$NIDIF == "S")] <- suppl$HATCH[which(suppl$NIDIF == "S")]

suppl$LastVisit[which(suppl$NIDIF == "S")] <- suppl$HATCH[which(suppl$NIDIF == "S")]

# Check point
# For successful nests, LastVisit == LastPresent == HATCH
table(suppl$HATCH[which(suppl$Fate == 0)] == suppl$LastPresent[which(suppl$Fate == 0)], useNA = "always")
table(suppl$LastVisit[which(suppl$Fate == 0)] == suppl$LastPresent[which(suppl$Fate == 0)], useNA = "always")

# For unsuccessful nests, LastVisit != LastPresent
table(suppl$LastVisit[which(suppl$Fate == 1)] == suppl$LastPresent[which(suppl$Fate == 1)], useNA = "always") # here there are 3 TRUE. Need to treat these nests cause it can be integrate by MARK analyses
suppl[which(suppl$Fate == 1 & suppl$LastPresent == suppl$LastVisit),]

# Correction of LastVisit date for lihes # 69, 74 & 83 (cause LastVisit == LastPresent is impossible in MARK)
suppl$LastVisit[which(suppl$Fate == 1 & suppl$LastPresent == suppl$LastVisit)] <- suppl$LastVisit[which(suppl$Fate == 1 & suppl$LastPresent == suppl$LastVisit)] + 1

# Class of variables
suppl$Fate <- as.factor(suppl$Fate)
suppl$ID <- as.factor(suppl$ID)
suppl$SUPPL <- as.factor(suppl$SUPPL)
suppl$HAB <- as.factor(suppl$HAB)
suppl$ZONE <- as.factor(suppl$ZONE)
suppl$SUPPL_DATE <- as.numeric(suppl$SUPPL_DATE)
suppl$PRED1 <- as.factor(suppl$PRED1)
suppl$PRED2 <- as.factor(suppl$PRED2)
suppl$PRED_DATE <- as.numeric(suppl$PRED_DATE)


suppl$HATCH_STATUS <- as.factor(suppl$HATCH_STATUS)
suppl$INI_STATUS <- as.factor(suppl$INI_STATUS)


# Estimation of exposition day number for supplemented nests -- TO CONTINUE !!!!!!
for (i in 1:length(suppl$ID)) {
  if (suppl$LastPresent[i] == suppl$LastVisit[i]) 
    suppl$difference[i] <- 0
    suppl$EXPO[i] <- suppl$HATCH[i] - suppl$INITIATION[i] + 1 
} else {
  suppl$difference[i] <- suppl$LastVisit[i] - suppl$LastPresent[i]
  suppl$EXPO[i] <- (suppl$LastPresent + ((suppl$LastVisit - suppl$LastPresent) / 2)) - suppl$INITIATION + 1 
  
}
}



# Check the duration of each nesting season
cum2$NEST_DURATION <- cum2$ECLO - cum2$INI

# cumRAIN/day to compensate the different length of nesing period
cum2$cumRAIN_DAY <- cum2$cumRAIN / cum2$NEST_DURATION

#write.table(cum2, "PREC_cum2.txt", dec = ".")

