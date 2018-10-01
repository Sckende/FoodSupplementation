getwd()
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
list.files()

suppl <- read.table("GOOSE_MARK_all_SUPPL_nests_all_years.txt", h = T, dec = ".", sep = "\t", stringsAsFactors = FALSE) # Supplemented nests data
tem <- read.table("GOOSE_MARK_all_CONTROL_nests_all_years.txt", h = T, dec = ".", sep = "\t", stringsAsFactors = FALSE) # Control nests data (i.e. colony data)
str(suppl); str(tem)
dim(suppl); dim(tem)
summary(suppl); summary(tem) 

#################### Cleaning of data ####################
#### Supplemented nests data ####

names(suppl)
summary(suppl)

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

#### Control nests data ####
names(tem)
summary(tem)

# Standardization of variables names between both dataframes
names(suppl)
names(tem)
suppl$Groupe <- "EXPE"
suppl$Groupe <- as.factor(suppl$Groupe)
tem$Groupe <- "COLONY"
tem$Groupe <- as.factor(tem$Groupe)

names(tem)[c(1:3, 5, 6, 11)] <- c("YEAR", "ID", "HAB", "INITIATION", "SUPPL_DATE", "NIDIF")
names(suppl)[25] <- "LastChecked"

# Conversion of data
    # Fate --> 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
tem$Fate[tem$NIDIF == 1] <- 0
tem$Fate[is.na(tem$Fate)] <- 1

tem$ID <- as.factor(tem$ID)
tem$HAB <- as.factor(tem$HAB)
tem$SUPPL <- as.factor(tem$SUPPL)
tem$Fate <- as.factor(tem$Fate)

tem$SUPPL_DATE <- 9999


# Merge of both dataframes
    # Selection of identical variables between both dataframes
suppl2 <- suppl[, c(1, 2, 9, 5, 20, 12, 26, 11, 24, 25, 19, 27)]
    # Join both dataframes together
gsg <- rbind(tem, suppl2)
summary(gsg)

# Check point
    # For successful nests, LastVisit == LastPresent
table(gsg$LastChecked[which(gsg$Fate == "0")] == gsg$LastPresent[which(gsg$Fate == "0")], useNA = "always")

    # For unsuccessful nests, LastVisit != LastPresent
table(gsg$LastChecked[which(gsg$Fate == "1")] == gsg$LastPresent[which(gsg$Fate == "1")], useNA = "always")

# Cleaning of dataframe
gsg$YEAR <- as.factor(gsg$YEAR)

gsg$HAB[gsg$HAB == "Mesic"] <- "MES"
gsg$HAB[gsg$HAB == "Wetland"] <- "WET"

gsg$SUPPL[gsg$SUPPL == "FOO"] <- "F"
gsg$SUPPL[gsg$SUPPL == "WAT"] <- "W"
#gsg <- gsg[gsg$SUPPL != "WF",]
gsg <- gsg[gsg$SUPPL != "PRED_BEF_SUPPL",]

# Delete the NIDIF variable
gsg <- gsg[,-11]

# Formating the reference level
gsg$SUPPL <- relevel(gsg$SUPPL, "TEM")

# Remove NA's
gsg <- gsg[!is.na(gsg$INITIATION),]
gsg <- gsg[!is.na(gsg$Fate),]
gsg <- gsg[!is.na(gsg$LastPresent),]

gsg <- droplevels(gsg)

# Obtaining the EXPO variable
# For successful nest <- (LastPresent - FirstFound + 1)
# For unsuccessful nest <- (LastPresent - FirstFound + 1) + (LastChecked - LastPresent) / 2
gsg$X <- 1:dim(gsg)[1]
for(i in gsg$X) {
  if (gsg$LastPresent[i] == gsg$LastChecked[i]) {
    gsg$EXPO[i] <- gsg$LastPresent[i] - gsg$INITIATION[i] + 1
  } else {
    gsg$EXPO[i] <- (gsg$LastPresent[i] - gsg$INITIATION[i] + 1) + (gsg$LastChecked[i] - gsg$LastPresent[i]) / 2
  }
}

#write.csv(gsg, "GOOSE_MARK_Complete_data.txt")
# **** WARNING ! Here we deleted some failed nests ==> underestimation of failed nests number **** #

####Data exploration - Basic (AND FALSE) NS computation#####
#####--------------------------------------#####
SNgeeseTEM <- dim(gsg[gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$SUPPL == "TEM",])[1]

SNgeeseF <- dim(gsg[gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$SUPPL == "F",])[1]

SNgeeseW <- dim(gsg[gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$SUPPL == "W",])[1]

# Packages for data manipulation, plotting & presenting tables
library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')

library('ztable')     # format tables for reporting

#Creating new data frame - SN by year and by treatments
prop <- cbind(c(rep(2015, 3), rep(2016, 3), rep(2017, 3)), rep(c("TEM","W", "F"), 3))
colnames(prop) <- c("YEAR", "SUPPL")
prop <- as.data.frame(prop)
nn <- c(
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] ,
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1],
  
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1],
  
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1]
)
PP <- c(
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "F",])[1]
)

prop$n <- c(
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "TEM",])[1] ,
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2015" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2016" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$YEAR == "2017" & gsg$SUPPL == "F",])[1]
)
prop$PROP <- PP
prop$error_type <- sqrt(prop$PROP*(1-prop$PROP)/prop$n)

head(prop)
summary(prop)

# Graphic (for experiment nests)
#x11()
color <- c("olivedrab3", "aquamarine3", "darkgoldenrod2")

barCenters <- barplot(prop$PROP,
                      width = 0.5,
                      col = color,
                      xlab = "Year",
                      ylab = "Nesting success",
                      ylim = c(0, 1),
                      names.arg = c("",2015, "","",2016, "","",2017, ""),
                      main = "Goose nesting success  depending on year and treatments",
                      legend.text = TRUE,
                      space = c(0.2,0,0,0.2,0,0,0.2,0,0))

legend("topleft",
       inset = c(0, -0,05),
       legend = c("TEMOIN", "WATER", "FOOD"), 
       fill = color,
       bty = "n")
segments(barCenters, prop$PROP - prop$error_type, barCenters, prop$PROP + prop$error_type, lwd = 1.5)
text(barCenters,0.2, labels = paste("(", as.character(prop$n), ")", sep = ""))


#Creating new data frame - SN by year, by habitat and by treatments
prop2 <- NULL
for (i in 2015:2017){
  for (j in c("TEM", "W", "F")) {
    for (k in c("MES", "WET")){
      YEAR <- i
      TREAT <- j
      HAB <- k
      N <- dim(gsg[gsg$YEAR == i & gsg$SUPPL == j & gsg$HAB == k,])[1]
      PROP <- dim(gsg[gsg$YEAR == i & gsg$SUPPL == j & gsg$HAB == k & gsg$Fate == "0",])[1] / dim(gsg[gsg$YEAR == i & gsg$SUPPL == j & gsg$HAB == k,])[1]
      error_type <- sqrt(PROP*(1-PROP)/N)
      
      r <- data.frame(YEAR, HAB, TREAT, N, PROP, error_type)
      
      prop2 <- rbind(prop2, r)
    }    
  }
}
#View(prop2)
# Graphic (for experiment nests)
#png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/RESULTS figures/RAW_goose_nesting_success.tiff", # C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/RESULTS figures
    #res=300,
    #width=30,
   # height=15,
    #pointsize=12,
    #unit="cm",
    #bg="transparent")

#x11()

#par(oma=c(0,0,0,3)) # outer margin
par(mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

color <- c("olivedrab3", "olivedrab4", "aquamarine3", "aquamarine4", "darkgoldenrod2", "darkgoldenrod3")

barCenters <- barplot(prop2$PROP, 
                      col = color,
                      xlab = "", 
                      ylab = "Nesting success", 
                      ylim = c(0, 1.2), 
                      names.arg = prop2$HAB, 
                      main = "", 
                      legend.text = TRUE, 
                      space = c(0.2, rep(c(0,0.1,0,0.1,0,0.4), 2) , c(0,0.1,0,0.1,0)), 
                      las = 2)

legend("topleft", 
       #inset = c(0, -0,05),
       legend = c("CONTROL", "WATER", "FOOD"), 
       fill = c("olivedrab3", "aquamarine3", "darkgoldenrod2"), bty = "n", cex = 1)
segments(barCenters, prop2$PROP - prop2$error_type, barCenters, prop2$PROP + prop2$error_type, lwd = 1.5)
text(barCenters,0.2, labels = paste("(", as.character(prop2$N), ")", sep = ""), cex = 1)
text(3.3, 1.1, labels = 2015, cex = 2)
text(9.9, 1.1, labels = 2016, cex = 2)
text(16.5, 1.1, labels = 2017, cex = 2)

#dev.off()


#### Formating data for MARK analysis ####
# Here choose one specific year or not
geese <- gsg
geese <- droplevels(geese)
#geese <- gsg[gsg$YEAR == "2015" | gsg$YEAR == "2016" | gsg$YEAR == "2017",]
summary(geese)
dim(geese)

div <- split(geese, geese$YEAR)

for(i in 1:nlevels(geese$YEAR)){

#Creation of AgeFound variable#
#--------------------------------#
#WARNING ! It has to be done before the modification of the FirstFound variable
div[[i]]$AgeFound <- (div[[i]]$FirstFound - div[[i]]$INITIATION)
div[[i]]$FindNest <- div[[i]]$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
#Attention ici valeur change selon les annees
FF <- min(div[[i]]$FirstFound) #date minimum = 160
div[[i]]$FirstFound <- div[[i]]$FirstFound - (FF - 1) #ici 159 = 160 - 1 (pour Day 1)

#Idem pour les variables LastPresent, LastChecked#
#-------------------------------------------------#
div[[i]]$LastPresent <- div[[i]]$LastPresent - (FF - 1)
div[[i]]$LastChecked <- div[[i]]$LastChecked - (FF - 1)

#Obtention de la variable AgeDay1#
#--------------------------------#
#correspond à l'âge du nid lors du premier jour du suivi de nids
div[[i]]$AgeDay1 <- (div[[i]]$AgeFound - div[[i]]$FirstFound) + 1

# Need to round EXPO variable to the superior integer
div[[i]]$EXPO2 <- ceiling(div[[i]]$EXPO)
}

geese <- do.call("rbind", div)

#Check point #1
table(geese$LastChecked[which(geese$Fate == "1")] == geese$LastPresent[which(geese$Fate == "1")], useNA = "always")
#geese[geese$Fate == "1" & geese$LastPresent==geese$LastChecked,]
geese$LastChecked[geese$Fate == "1" & geese$LastPresent==geese$LastChecked] <- geese$LastChecked[geese$Fate == "1" & geese$LastPresent==geese$LastChecked] + 1 # correction of LastChecked == LAstPresent for failed nests

#Check point #2
table(geese$LastChecked[which(geese$LastPresent == geese$LastChecked & geese$LastPresent == geese$FirstFound)], useNA = "always")
geese[which(geese$LastPresent == geese$LastChecked & geese$LastPresent == geese$FirstFound),]
geese <- geese[!(geese$X == 3144 | geese$X == 4330),] # Delete nest with FirstFound = LastPresent = LastVisit
geese <- droplevels(geese)

# DELETE EXTREM DATA for exposition > 35 days - n = 1 for 2000, n = 1 for 2008, n = 2 for 2010, n = 1 for 2011, n = 2 for 2015 & n = 2 for 2017
geese[geese$EXPO2 > 35,]
geese <- geese[!(geese$X == 222 | geese$X == 244 | geese$X == 671 | geese$X == 673 | geese$X == 2012 | geese$X == 4110 | geese$X == 4668 | geese$X == 4686 | geese$X == 4869),]
geese <- droplevels(geese)

# Minimal values of exposition nests for successful ones

#### CF COURRIEL OF NICOLAS !!! ####
#### retrait des nids à succès avec EXPO < 20 ####

x11()
boxplot(geese$EXPO2[geese$Fate == "0"] ~ geese$YEAR[geese$Fate == "0"], ylim = c(8, 36))


# Treshold value for minimal of day exposure
tr <- 20; col1 <- "olivedrab3"; y <- 8
tr <- 24; col1 <- "aquamarine3"; y <- 10


minima <- geese[geese$Fate == "0" & geese$EXPO2 < tr,]
m <- as.data.frame(table(minima$YEAR), 1995:2017)
minima$N <- m$Freq[match(minima$YEAR, m$Var1)]
abline( h = tr, col = col1, lty = "longdash", lwd = 2)
for (i in 1:nlevels(geese$YEAR)){
  cols <- ifelse(m$Freq[i] > 10, "red", col1)
    text(x = i, 
         y = y, 
         labels = m$Freq[i], 
         cex = 1,
         col = cols
         )
}

geese <- geese[!(geese$Fate == "0" & geese$EXPO < 20),]
geese <- droplevels(geese)

#### Type of rainfall year ####
cum2 <- read.table("PREC_cum2.txt",  dec = ".", h = T)
geese$RAINFALL <- cum2$RAINFALL[match(geese$YEAR, cum2$YEAR)]

#### Cumulative precipitation for each nests ####
rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", dec = ",", h = T)
rain <- rain[!is.na(rain$RAIN),]

# Data exploration
  # Cumulative rain far all the season and for each year
div <- split(rain, rain$YEAR)
for (i in 1:23){
  div[[i]]$cumRAIN <- div[[i]]$RAIN
  for (j in 2:dim(div[[i]])[1]) {
    div[[i]]$cumRAIN[j] <- div[[i]]$cumRAIN[j - 1] + div[[i]]$RAIN[j]
  }
  
}
  # Plots of cumulative rainfall for each year
#dev.off()
#x11()
par(mfrow = c(5, 5), mar = c(4, 5, 1, 1))
for(i in 1:23){
  plot(div[[i]]$JJ, div[[i]]$cumRAIN, xlab = unique(div[[i]]$YEAR), ylab = "cum. Prec. (mm)", type = "h", bty = "n")
  legend

}

# Compute the cumulative rainfall per nest & cumulative rainfal per nest / per day
geese$X <- 1:dim(geese)[1] #new number for the X variable
for (i in geese$X) {
  geese$cumPREC[i] <- 
    sum(rain$RAIN[which(rain$YEAR == geese$YEAR[i] & rain$JJ >= geese$INITIATION[i] & rain$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
  geese$PREC_day[i] <- 
    sum(rain$RAIN[which(rain$YEAR == geese$YEAR[i] & rain$JJ >= geese$INITIATION[i] & rain$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)]/geese$EXPO2[i])

}

# Exploration of cumPREC variable
dev.off()
x11()
boxplot(geese$cumPREC ~ geese$YEAR)
boxplot(geese$PREC_day ~ geese$YEAR)
plot(geese$PREC_day[geese$Fate == 0])


# Data eploration
div <- split(geese, geese$YEAR)
x11()
par(mfrow = c(5, 5))
#dev.off()
for (i in 1:nlevels(geese$YEAR)) {
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO2, div[[i]]$cumPREC, main = unique(div[[i]]$YEAR), ylab = "cumulative prec", xlab = "Number of exposition days", bty = "n", ylim = c(min(geese$cumPREC), max(geese$cumPREC)), pch = pchs, col = cols, cex = 2)
}

  # Visualisation des cumPREC_rate pour les nids successfull
succ_nest <- geese[geese$Fate == 0,]
succ_list <- split(succ_nest, succ_nest$YEAR)

x11()
#dev.off()
par(mfrow = c(5, 5))
for (i in 1:nlevels(geese$YEAR)) {
  plot(succ_list[[i]]$EXPO2, succ_list[[i]]$PREC_day, main = unique(succ_list[[i]]$YEAR), bty = "n", xlab = "Exposition time", ylab = "Cum. Prec. per day (mm/day)", xlim = c(min(succ_nest$EXPO2), max(succ_nest$EXPO2)))
}

### Cumulative precipitation per year ####
# Compute the cumulative rainfall per year between the both mean date of initiation and hatchling
geese$PREC_Y <- cum2$cumRAIN[match(geese$YEAR, cum2$YEAR)]

#### Temperature variables for each nests - TO UPDATE WHEN THE DOWNLOAD OF LAST BYLCAMP DATA WILL BE DONE #####
#deg <- read.table("TEMP_PondInlet_2015-2017.txt", h = T, dec =".", sep = "\t")
deg <- read.csv("TEMP_PondInlet_1995-2017.csv", h = T, sep = ";")
summary(deg)

deg$JJ <- strptime(as.character(deg$Date), format = "%Y-%m-%d")
deg$JJ <- deg$JJ$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires

deg <- deg[!is.na(deg$Mean_Temp),]

# Data exploration
# Cumulative Mean temperature during the nesting season (JJ 163 - 12 June to JJ 206 - 25 July) and for each year
div <- split(deg, deg$Year)
for (i in 1:23){
  div[[i]]$cumTEMP <- div[[i]]$Mean_Temp
  for (j in 2:dim(div[[i]])[1]) {
    div[[i]]$cumTEMP[j] <- div[[i]]$cumTEMP[j - 1] + div[[i]]$Mean_Temp[j]
  }
  
}

x11()
par(mfrow = c(5, 5), mar = c(4, 5, 1, 1))
for(i in 1:23){
  plot(div[[i]]$JJ[div[[i]]$JJ <= 206 & div[[i]]$JJ >= 163], div[[i]]$cumTEMP[div[[i]]$JJ <= 206 & div[[i]]$JJ >= 163], main = unique(div[[i]]$Year), xlab = "JJ", ylab = "cum. Temp.", type = "b", bty = "n")
  
}
dev.off()

# Computation of new variables - mean temperature per nest
for(i in geese$X){
  geese$meanTEMP[i] <- mean(deg$Mean_Temp[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
  geese$sdTEMP[i] <- sd(deg$Mean_Temp[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
  geese$cumTEMP[i] <- sum(deg$Mean_Temp[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
}

# Data eploration
div <- split(geese, geese$YEAR)

x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$cumTEMP, bty = "n", xlab = unique(div[[i]]$YEAR),col = cols, pch = pchs, ylab = "Cum. Temp", ylim = c(min(geese$cumTEMP), max(geese$cumTEMP)), cex = 2)}


x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$meanTEMP, bty = "n",col = cols, pch = pchs, xlab = unique(div[[i]]$YEAR), ylab = "Mean Temp",  ylim = c(min(geese$meanTEMP), max(geese$meanTEMP)), cex = 2)
  
}

#### Mean temperature per year ####
# Computation of new variables - mean temperature per year
deg <- read.csv("TEMP_PondInlet_1995-2017.csv", h = T, sep = ";")
deg$JJ <- strptime(as.character(deg$Date), format = "%Y-%m-%d")
deg$JJ <- deg$JJ$yday + 1 #comme dates continues pas besoin de traiter 
 
# Compute the mean temperature per year between the both mean date of initiation and hatchling

deg2 <- NULL
for (i in unique(deg$Year)) {
  YEAR <- i
  INI <- cum2$INI[cum2$YEAR == i]
  ECLO <- cum2$ECLO[cum2$YEAR == i]
  
  temp1 <- deg$Mean_Temp[deg$Year == i & deg$JJ == INI]
  temp2 <- deg$Mean_Temp[deg$Year == i & deg$JJ == ECLO]
  
  meanTEMP_year <- mean(deg$Mean_Temp[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  sdTEMP_year <- sd(deg$Mean_Temp[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  
  TAB <- data.frame(YEAR, INI, ECLO, temp1, temp2, meanTEMP_year, sdTEMP_year)
  
  deg2 <- rbind(deg2, TAB)
}

geese$TEMP_Y <- deg2$meanTEMP_year[match(geese$YEAR, deg2$YEAR)]
geese$sdTEMP_Y <- deg2$sdTEMP_year[match(geese$YEAR, deg2$YEAR)]

#### Type of year concerning temperature ####
ann_temp <- mean(deg2$meanTEMP_year)
ann_temp_sd <- sd(deg2$meanTEMP_year)


deg2$TEMP_TYP[deg2$meanTEMP_year < ann_temp - ann_temp_sd] <- "COLD"
deg2$TEMP_TYP[deg2$meanTEMP_year > ann_temp + ann_temp_sd] <- "WARM"
deg2$TEMP_TYP[deg2$meanTEMP_year >= ann_temp - ann_temp_sd & deg2$meanTEMP_year <= ann_temp + ann_temp_sd] <- "INTER"

deg2$TEMP_TYP <- as.factor(deg2$TEMP_TYP)

geese$TYP_TEMP <- deg2$TEMP_TYP[match(geese$YEAR, deg2$YEAR)]
#### Partial pressure of water per nest ####
#######----Buck equation----#######
# the simpliest one

deg$pH2O_Buck <- 0.61121 * exp((18.678 - deg$Mean_Temp / 234.5) * (deg$Mean_Temp/(257.14 + deg$Mean_Temp)))


for(i in geese$X){
  geese$mPh2o_B[i] <- mean(deg$pH2O_Buck[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)], na.rm = T)
  geese$sdPh2o_B[i] <- sd(deg$pH2O_Buck[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)], na.rm = T)
}

# Data exploration
div <- split(geese, geese$YEAR)

x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$meanPh2o_Buck, bty = "n", xlab = unique(div[[i]]$YEAR),col = cols, pch = pchs, ylab = "Buck Mean p(H2O)", ylim = c(min(geese$meanPh2o_Buck), max(geese$meanPh2o_Buck)), cex = 2)}

#######----Goff-Gratch equation ***----####### 

#e is the saturation water vapor pressure in hPa
t <- deg$Mean_Temp + 273.15 #the absolute air temperature in kelvins
tst <- 373.15 #the steam-point (i.e. boiling point at 1 atm) temperature = 373.15 K 
est <- 1013.25 #the steam-point pressure (1 atm = 1013.25 hPa)

log_e <- -7.90298 * (tst/(t - 1)) + 5.02808 * log(tst / t) - 1.3816 * 10^-7 * (10^(11.344 * ((1 - t)/tst)) - 1) + 8.1328 * 10^-3 * (10^(-3.49149 * (tst/(t-1)))- 1) + log(est)

deg$pH2O_GG <- exp(log_e)

# Mean of pH2O computation per nest
for(i in geese$X){
  geese$mPh2o_GG[i] <- mean(deg$pH2O_GG[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)], na.rm = T)
  geese$sdPh2o_GG[i] <- sd(deg$pH2O_GG[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)], na.rm = T)
}

# Data exploration
div <- split(geese, geese$YEAR)

x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$mPh2o_GG, bty = "n", xlab = unique(div[[i]]$YEAR),col = cols, pch = pchs, ylab = "GG Mean p(H2O)", ylim = c(min(geese$mPh2o_GG), max(geese$mPh2o_GG)), cex = 2)}

#### Partial pressure of water per year --- Buck equation ####
phh <- NULL
for (i in unique(deg$Year)) {
  YEAR <- i
  INI <- cum2$INI[cum2$YEAR == i]
  ECLO <- cum2$ECLO[cum2$YEAR == i]
  
  pH2O1 <- deg$pH2O_Buck[deg$Year == i & deg$JJ == INI]
  pH2O2 <- deg$pH2O_Buck[deg$Year == i & deg$JJ == ECLO]
  
  meanPH2O_year <- mean(deg$pH2O_Buck[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  sdPH2O_year <- sd(deg$pH2O_Buck[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  
  TAB <- data.frame(YEAR, INI, ECLO, pH2O1, pH2O2, meanPH2O_year, sdPH2O_year)
  
  phh <- rbind(phh, TAB)
}

geese$PH2O_B_Y <- phh$meanPH2O_year[match(geese$YEAR, phh$YEAR)]

#### Partial pressure of water per year --- GG equation ####
ph <- NULL
for (i in unique(deg$Year)) {
  YEAR <- i
  INI <- cum2$INI[cum2$YEAR == i]
  ECLO <- cum2$ECLO[cum2$YEAR == i]
  
  pH2O1 <- deg$pH2O_GG[deg$Year == i & deg$JJ == INI]
  pH2O2 <- deg$pH2O_GG[deg$Year == i & deg$JJ == ECLO]
  
  meanPH2O_year <- mean(deg$pH2O_GG[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  sdPH2O_year <- sd(deg$pH2O_GG[deg$Year == i & deg$JJ >= INI & deg$JJ <= ECLO], na.rm = TRUE)
  
  TAB <- data.frame(YEAR, INI, ECLO, pH2O1, pH2O2, meanPH2O_year, sdPH2O_year)
  
  ph <- rbind(ph, TAB)
}

geese$PH2O_GG_Y <- ph$meanPH2O_year[match(geese$YEAR, ph$YEAR)]

#### Check point for the climate variables per year ####
check <- as.data.frame(1995:2017)
names(check) <- "YEAR"
check$GG_Ph2o <- ph$meanPH2O_year[match(check$YEAR, ph$YEAR)]
check$GG_Ph2o_sd <- ph$sdPH2O_year[match(check$YEAR, ph$YEAR)]
check$Buck_Ph2o <- phh$meanPH2O_year[match(check$YEAR, phh$YEAR)]
check$Buck_Ph2o_sd <- phh$sdPH2O_year[match(check$YEAR, phh$YEAR)]
check$cumPREC <- geese$PREC_Y[match(check$YEAR, geese$YEAR)]
check$meanTEMP <- geese$TEMP_Y[match(check$YEAR, geese$YEAR)]
check$sdTEMP <- geese$sdTEMP_Y[match(check$YEAR, geese$YEAR)]
head(check)

#write.table(check, "GOOSE_PAPER_Annual_weather.txt") # For paper graphes

x11()
par(mfrow = c(3, 1), mar = c(4, 4, 0, 4))
plot(check$YEAR, check$meanTEMP, col = "red", type = "l", xlab = "Year", ylab = "Seasonal mean temperature", bty = "n", lwd = 2)
plot(check$YEAR, check$cumPREC, col = "blue", type = "l", xlab = "Year", ylab = "Seasonal cumulative precipitation", bty = "n", lwd = 2)

plot(check$YEAR, check$GG_Ph2o, col = "green", type = "l", xlab = "Year", ylab = "Seas. mean pH2O (GG equation)", bty = "n", lwd = 2, col.lab = "green")
par(new = T)
plot(check$YEAR, check$Buck_Ph2o, col = "darkgreen", type = "l", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",lwd = 2)
axis(side = 4,
     lwd = 1,
     #las = 2,
     cex.axis = 1)
mtext(side = 4,
      #cex.lab = 0.5,
      line = 3,
      "Seas. mean pH2O - Buck equation",
      col.lab = "darkgreen")

#### Data Analyses ####
  # Setting factors
geese$TYP_TEMP <- relevel(geese$TYP_TEMP, "INTER")
geese$YEAR <- as.factor(geese$YEAR)
geese$RAINFALL <- relevel(geese$RAINFALL, "INTER")
geese <- droplevels(geese)

#write.csv(geese, "GOOSE_geese.txt") # For Rmarkdown document & analysis on VAIO
#write.csv(geese, "GOOSE_geese_with_WF.txt") # Same than before PLUS WF supplemented nests - For analysis in GOOSE_MARK_suppl_nests.R

#### True nesting success per habitat / treatment / year ####
geese_SUPPL <- geese[geese$YEAR == "2015" | geese$YEAR == "2016" | geese$YEAR == "2017",]
geese_SUPPL <- droplevels(geese_SUPPL)

require(RMark)
# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(geese_SUPPL$LastChecked)

nest_succ <- mark(geese_SUPPL, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)

nest_succ$results$beta # view estimated beta for model in R
nest_succ$results$real

exi <- as.data.frame(nest_succ$results$real)
exi <- exi[, -c(5:6)]
exi$SUPPL <- rep(c("TEM", "F", "W"), 6)
exi$SUPPL <- ordered(exi$SUPPL, levels = c("TEM", "W", "F"))
exi$HAB <- rep(c("MES", "MES", "MES", "WET", "WET", "WET"), 3)
exi$YEAR <- c(rep(2015, 6), rep(2016, 6), rep(2017,6))
exi$SN <- exi$estimate^27
exi$SN_se <- exi$se^27


div <- split(exi, exi$YEAR)
for(i in 1:3){
  div[[i]]$SUPPL <- ordered(div[[i]]$SUPPL, levels = c("TEM", "W", "F"))
  div[[i]] <- div[[i]][order(div[[i]]$SUPPL),]
  div[[i]]$N <- c(dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "TEM" & geese$HAB == "MES",])[1], dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "TEM" & geese$HAB == "WET",])[1], dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "W" & geese$HAB == "MES",])[1], dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "W" & geese$HAB == "WET",])[1], dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "F" & geese$HAB == "MES",])[1], dim(geese[geese$YEAR == unique(div[[i]]$YEAR) & geese$SUPPL == "F" & geese$HAB == "WET",])[1])
}

exi <- do.call("rbind", div)
#write.csv(exi, "GOOSE_exi.csv") #For Rmarkdown document

# Plot

x11()
par(mfrow = c(1, 2), mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

color <- c("olivedrab3", "olivedrab4", "aquamarine3", "aquamarine4", "darkgoldenrod2", "darkgoldenrod3")

barCenters <- barplot(exi$estimate,
                      #exi$SN,
                      col = color,
                      xlab = "", 
                      ylab = "Nesting success", 
                      ylim = c(0.96, 1.015),
                      #ylim = c(0.4, 1.015),
                      #beside = TRUE,
                      xpd = FALSE,
                      names.arg = exi$HAB, 
                      main = "", 
                      legend.text = TRUE, 
                      space = c(0.2, rep(c(0,0.1,0,0.1,0,0.4), 2) , c(0,0.1,0,0.1,0)), 
                      las = 2)

legend("topright", 
       #inset = c(0, -0,05),
       legend = c("CONTROL", "WATER", "FOOD"), 
       fill = c("olivedrab3", "aquamarine3", "darkgoldenrod2"), bty = "n", cex = 1)
segments(barCenters,
         exi$estimate - exi$se, 
         #exi$SN - exi$SN_se,
         barCenters, 
         exi$estimate + exi$se, 
         #exi$SN + exi$SN_se,
         lwd = 1.5)
text(barCenters,0.965, labels = paste("(", as.character(exi$N), ")", sep = ""), cex = 1)
text(3.3, 1, labels = 2015, cex = 2)
text(9.9, 1, labels = 2016, cex = 2)
text(16.5, 1, labels = 2017, cex = 2)

#### Competitive models ####

#### Models per YEAR - 2015/2016/2017 ####

geeseYEAR <- geese[geese$YEAR == "2015",]
geeseYEAR <- geese[geese$YEAR == "2016",]
geeseYEAR <- geese[geese$YEAR == "2017",]

# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(geeseYEAR$LastChecked)

# PART 1
run.geese.YEAR.1 = function()
{
mYEAR_0 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
# BEST MODEL 2 
mYEAR_1 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)  

mYEAR_2 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T) 

mYEAR_3 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)

mYEAR_4 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
mYEAR_5 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
mYEAR_6 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
   
mYEAR_7 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)

mYEAR_8 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + NestAge)), groups = "SUPPL", delete = T)

# BEST MODEL 1 
mYEAR_9 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge)), groups = "HAB", delete = T)

return(collect.models() )
}

geese.YEAR.1.results <- run.geese.YEAR.1()
geese.YEAR.1.results

# Save models list and est model
#save(geese.YEAR.1.results, file = "geese2017.rda")
#save(mYEAR_1, file = "geese2017_1.rda")
#save(mYEAR_8, file = "geese2017_2.rda")

# PART 2
run.geese.YEAR.2 = function()
{
  mYEAR_0 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  mYEAR_10 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + cumPREC + meanTEMP)), groups = c("SUPPL", "HAB"), delete = T)
  
  mYEAR_11 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + cumPREC*meanTEMP)), groups = c("SUPPL", "HAB"), delete = T)
  
  mYEAR_12 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + cumPREC)), groups = c("SUPPL", "HAB"), delete = T)
  
  mYEAR_13 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + meanTEMP)), groups = c("SUPPL", "HAB"), delete = T)
  
  mYEAR_14 <- mark(geeseYEAR, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)
  
  return(collect.models() )
}

geese.YEAR.2.results <- run.geese.YEAR.2()
geese.YEAR.2.results


#Save models list and est model
#save(geese.YEAR.2.results, file = "geese2017bis.rda")
#save(mYEAR_11, file = "geese2017_1bis.rda")


#### FULL MODELS ####
# Models explaining maximal of variation of explained response with the time effect (YEAR, NestAge) and the habitat effect. Models including ALL MONITORED GOOSE NESTS IN COLONY since 1995

# Models were run on VAIO computer --> see the VAIO script
# Results are saved under "geeseFULL.rda" for the comparaiso of all models and under "geeseFULL_1.rda" for the best model

load("geeseFULL.rda")
geese.FULL.results$model.table

#### WEATHER models - ANNUAL VALUES####

# *** Here, we use annual values for climate variables ***

# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(geese$LastChecked)

run.geese.WEATHER=function()
{
  # Null model
  m1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # HAB + INITIATION + YEAR
  m2 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR)), groups = c("HAB", "YEAR"), delete = T)

# Temperature  
  m3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + TEMP_Y)), groups = c("HAB", "YEAR"), delete = T)

  
  # Precipitation
  m4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + PREC_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # p(H2O)
  m5 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + PH2O_GG_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # Temperature + precipitation
  m6 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + TEMP_Y + PREC_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # Temperature + precipitation
  m7 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + TEMP_Y + PH2O_GG_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # Precipitation + p(H2O)
  m8 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + PREC_Y + PH2O_GG_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # Precipitation + p(H2O) + temperature
  m9 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + PREC_Y + PH2O_GG_Y + TEMP_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  # Precipitation x temperature
  m10 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + INITIATION + YEAR + PREC_Y*TEMP_Y)), groups = c("HAB", "YEAR"), delete = T)
  
  
  return(collect.models())
}


geese.WEATHER.results <- run.geese.WEATHER()
geese.WEATHER.results

#Save models list and est model
#save(geese.WEATHER.results, file = "geeseWEATHER.rda")
#save(XXX, file = "geeseWEATHER_1.rda")

#### WEATHER models - FACTORIAL VALUES - 3 levels ####

nocc <- max(geese$LastChecked)

run.geese.EXTREM=function()
{
  # Null model
  m1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)

  m2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge + TYP_TEMP)), groups = c("HAB", "TYP_TEMP"), delete = T)
  
  m3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge + RAINFALL)), groups = c("HAB", "RAINFALL"), delete = T)
  
  m4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge + RAINFALL*TYP_TEMP)), groups = c("HAB", "RAINFALL", "TYP_TEMP"), delete = T)
  
  m5 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge + RAINFALL + TYP_TEMP)), groups = c("HAB", "RAINFALL", "TYP_TEMP"), delete = T)
  
  return(collect.models())
}

geese.EXTREM.results <- run.geese.EXTREM()
geese.EXTREM.results

#Save models list and est model
write.table(m4$results$beta, "test.txt")
save(geese.EXTREM.results, file = "geeseEXTREM.rda")
save(m4, file = "geeseEXTREM_1.rda")

#################### Best model for full database ####################
############## only considering models without interaction ##########

x11()

#Test on mYEAR_5 model
plot(mYEAR_5$results$real$estimate, pch = 16, xlim = c(0, 3), ylim = c(0.95, 1))
arrows(1, mYEAR_5$results$real$estimate[1], x1=1, y1=mYEAR_5$results$real$lcl[1], length=0.1,angle=90, lwd=1)
arrows(1, mYEAR_5$results$real$estimate[1], x1=1, y1=mYEAR_5$results$real$lcl[1], length=0.1,angle=90, lwd=1)
arrows(1, mYEAR_5$results$real$estimate[1], x1=1, y1=mYEAR_5$results$real$ucl[1], length=0.1,angle=90, lwd=1)
arrows(2, mYEAR_5$results$real$estimate[2], x1=2, y1=mYEAR_5$results$real$lcl[2], length=0.1,angle=90, lwd=1)
arrows(2, mYEAR_5$results$real$estimate[2], x1=2, y1=mYEAR_5$results$real$ucl[2], length=0.1,angle=90, lwd=1)
arrows(3, mYEAR_5$results$real$estimate[3], x1=3, y1=mYEAR_5$results$real$lcl[3], length=0.1,angle=90, lwd=1)
arrows(3, mYEAR_5$results$real$estimate[3], x1=3, y1=mYEAR_5$results$real$ucl[3], length=0.1,angle=90, lwd=1)

dev.off()

#### Plot visualization - to end the CODE #### 
# To obtain a plot
M04 <- geese.results$M04
fc <- find.covariates(M04,geese)
fc$value[1:41]=1:41 # assign 1:35 to 1st 35 nest ages


fc$value[fc$var == "M04"] <- 0.1 # assign new value to PpnGrass
design <- fill.covariates(M04,fc) # fill design matrix with values


# extract 1st 41 rows of output
M04.survival <- compute.real(M04, design = design)[1:41,]


# insert covariate columns
M04.survival <- cbind(design[1:41, c(2:3)], M04.survival)
colnames(M04.survival) <- c("Age", "M04","DSR","seDSR","lclDSR","uclDSR")
M04.survival # view estimates of DSR for each age and PpnGrass combo
