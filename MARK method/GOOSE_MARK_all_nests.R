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
gsg <- gsg[gsg$SUPPL != "WF",]
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
div[[i]]$AgeFound <- (div[[i]]$FirstFound - div[[i]]$INITIATION) + 1 #...+1 cause age 0 is impossible
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

#### Add type of rainfall year ####
cum2 <- read.table("PREC_cum2.txt",  dec = ".", h = T)
geese$RAINFALL <- cum2$RAINFALL[match(geese$YEAR, cum2$YEAR)]

#### Add cumulative precipitation for each nests ####
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


#### Add temperature variables for each nests - TO UPDATE WHEN THE DOWNLOAD OF LAST BYLCAMP DATA WILL BE DONE #####
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

# Computation of new variables
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

#### Partial pressure of water ####
#Buck equation - the simpliest one

deg$pH2O_Buck <- 0.61121 * exp((18.678 - deg$Mean_Temp / 234.5) * (deg$Mean_Temp/(257.14 + deg$Mean_Temp)))


for(i in geese$X){
  geese$meanPh2o_Buck[i] <- mean(deg$pH2O_Buck[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
  geese$sdPh2o_Buck[i] <- sd(deg$pH2O_Buck[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
}

# Data exploration
div <- split(geese, geese$YEAR)

x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$meanPh2o_Buck, bty = "n", xlab = unique(div[[i]]$YEAR),col = cols, pch = pchs, ylab = "Buck Mean p(H2O)", ylim = c(min(geese$meanPh2o_Buck), max(geese$meanPh2o_Buck)), cex = 2)}

# Goff-Gratch equation 

#e is the saturation water vapor pressure in hPa
t <- deg$Mean_Temp + 273.15 #the absolute air temperature in kelvins
tst <- 373.15 #the steam-point (i.e. boiling point at 1 atm) temperature = 373.15 K 
est <- 1013.25 #the steam-point pressure (1 atm = 1013.25 hPa)

log_e <- -7.90298 * (tst/(t - 1)) + 5.02808 * log(tst / t) - 1.3816 * 10^-7 * (10^(11.344 * ((1 - t)/tst)) - 1) + 8.1328 * 10^-3 * (10^(-3.49149 * (tst/(t-1)))- 1) + log(est)

deg$pH2O_GG <- exp(log_e)

# Mean of pH2O computation per nest
for(i in geese$X){
  geese$meanPh2o_GG[i] <- mean(deg$pH2O_GG[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
  geese$sdPh2o_GG[i] <- sd(deg$pH2O_GG[which(deg$Year == geese$YEAR[i] & deg$JJ >= geese$INITIATION[i] & deg$JJ <= geese$INITIATION[i] + geese$EXPO2[i] - 1)])
}

# Data exploration
div <- split(geese, geese$YEAR)

x11()
par(mfrow = c(5, 5), mar = c(5, 5, 1, 1))
for (i in 1:23){
  cols <- ifelse(div[[i]]$Fate == "0", "olivedrab3", "darkgoldenrod2")
  pchs <- ifelse(div[[i]]$Fate == "0", 20, 8)
  plot(div[[i]]$EXPO, div[[i]]$meanPh2o_GG, bty = "n", xlab = unique(div[[i]]$YEAR),col = cols, pch = pchs, ylab = "GG Mean p(H2O)", ylim = c(min(geese$meanPh2o_GG), max(geese$meanPh2o_GG)), cex = 2)}


#### Data Analyses ####
  # Setting factors
geese$YEAR <- as.factor(geese$YEAR)
geese$RAINFALL <- relevel(geese$RAINFALL, "LOW")
geese <- droplevels(geese)

#write.csv(geese, "GOOSE_geese.txt") # For Rmarkdown document

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

full_goo <- geese[geese$SUPPL == "TEM",]

# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(full_goo$LastChecked)

run.geese.FULL = function()
{

full0 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
full1 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge)), groups = "YEAR", delete = T)

full3 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR )), groups = "YEAR", delete = T)

full4 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)

full5 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge + HAB)), groups = c("YEAR", "HAB"), delete = T)

full6 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB)), groups = c("YEAR", "HAB"), delete = T)

full7 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB)), groups = "HAB", delete = T)

full8 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)

# INTERACTION
full2 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR * NestAge)), groups = "YEAR", delete = T)

full9 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*HAB)), groups = c("YEAR", "HAB"), delete = T)

full10 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*HAB + NestAge)), groups = c("YEAR", "HAB"), delete = T)

full11 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*NestAge + HAB)), groups = c("YEAR", "HAB"), delete = T)

full12 <- mark(full_goo, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*NestAge + YEAR*HAB)), groups = c("YEAR", "HAB"), delete = T)

return(collect.models() )
}

# run defined models
geese.FULL.results <- run.geese.FULL()
geese.FULL.results


#Save models list and est model
#save(geese.FULL.TIME.results, file = "geeseFULLTIME.rda")
#save(time1, file = "geeseFULLTIME_1.rda")


#### WEATHER/SUPPLEMENTATION models ####

# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(geese$LastChecked)

run.geese.WEATHER.SUPPL=function()
{
  m0 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  m1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  m2 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)

# TEMPERATURE  
  temp1 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP)), delete = T)
  
  temp2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + HAB)), groups = "HAB", delete = T)
  
  temp3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL)), groups = "SUPPL", delete = T)
  
  temp4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
# CUMULATIVE PRECIPITATION  
  prec1 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC)), delete = T)
  
  prec2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC + HAB)), groups = "HAB", delete = T)
  
  prec3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC + SUPPL)), groups = "SUPPL", delete = T)
  
  prec4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
# RAINFALL  
  rain1 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ RAINFALL)), groups = "RAINFALL", delete = T)
  
  rain2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ RAINFALL + HAB)), groups = c("RAINFALL", "HAB"), delete = T)
  
  rain3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ RAINFALL + SUPPL)), groups = c("RAINFALL", "SUPPL"), delete = T)
  
  rain4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ RAINFALL + SUPPL + HAB)), groups = c("RAINFALL", "SUPPL", "HAB"), delete = T)
  
# p(H2O)  
  vap1 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanPh2o)), delete = T)
  
  vap2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanPh2o + HAB)), groups = "HAB", delete = T)
  
  vap3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanPh2o + SUPPL)), groups = "SUPPL", delete = T)
  
  vap4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanPh2o + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
# WEATHER MIXTE VARIABLES
  mix1 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + meanPh2o + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  mix2 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  mix3 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + meanPh2o + SUPPL)), groups = "SUPPL", delete = T)
  
  mix4 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + meanPh2o + HAB)), groups = "HAB", delete = T)
  
  mix5 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + meanPh2o)), delete = T)
  
  mix6 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + SUPPL)), groups = "SUPPL", delete = T)
  
  mix7 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + HAB)), groups = "HAB", delete = T)
  
  mix8 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC)), delete = T)
  
  
  mix9 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP*cumPREC + SUPPL + HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  return(collect.models())
}


geese.WEATHER.SUPPL.results <- run.geese.WEATHER.SUPPL()
geese.WEATHER.SUPPL.results

#Save models list and est model
#save(geese.WEATHER.SUPPL.results, file = "geeseWEASUPPL.rda")
#save(mix2, file = "geeseWEASUPPL_1.rda")



run.geese=function()
{
  
# Complete model with prec-temp interaction
#MtotalINTER <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP * cumPREC + SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)

# Complete model with prec-temp interaction
#Mtotal <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)

#Mt <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC + SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)

#Mtt <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC + SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)

# Temperature model
Mtemp0 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP)), delete = T, groups = "YEAR")

# Temperature model
#Mtemp1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL + HAB + YEAR + NestAge)), delete = T, groups = c( "YEAR", "HAB", "SUPPL"))

# Temperature model
#Mtemp2 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL*HAB + YEAR + NestAge)), delete = T, groups = c( "YEAR", "HAB", "SUPPL"))

# Temperature model
#Mtemp3 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL + HAB + YEAR )), delete = T, groups = c( "YEAR", "HAB", "SUPPL"))

# Temperature model
Mtemp4 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL + HAB)), delete = T, groups = c("HAB", "SUPPL"))

# Temperature model
#Mtemp5 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + SUPPL + HAB + NestAge)), delete = T, groups = c("HAB", "SUPPL"))

#MpH2O <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanPh2o + NestAge)), delete = T, groups = "YEAR")

#Precipitation model
Mrain <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ RAINFALL)), groups = "RAINFALL", delete = T)

#Precipitation model
Mprec1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumPREC)), delete = T)

#Mprec2 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ PREC_day)), delete = T)

Mtemp_prec1 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC )), delete = T)

#Mtemp_prec2 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + PREC_day)), delete = T)
  
Mprec_temp_suppl <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + SUPPL)), groups = "SUPPL", delete = T)

#Mprec_temp_suppl_NestAge <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ meanTEMP + cumPREC + SUPPL + NestAge)), groups = "SUPPL", delete = T)

# 0. A model of constant daily survival rate (DSR)
M0 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~1)), delete = T) # delete = TRUE erases the output files

# 00. year effect
#M00 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)

# 000. habitat effect
M000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = TRUE)

# 0000. supplementation effect
M0000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = TRUE)

# 00000. NestAge effect
#M00000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = TRUE)

# 000000. habitat*NestAge
#M000000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB*NestAge)), groups = "HAB", delete = TRUE)

# 1. AN + SUPPL
#M01 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL)), groups = c("YEAR", "SUPPL"), delete = TRUE)


# 2. AN + SUPPL + HABITAT
#M02 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + HAB)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 3. AN + SUPPL + HABITAT + HABITAT*SUPPL
#M03 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 4. AN + SUPPL + HABITAT + NestAge
#M04 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR  + SUPPL + HAB + NestAge)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 5. AN + NestAge + HABITAT*SUPPL
#M05 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 8. AN + HABITAT
#M08 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB)), groups = c("YEAR", "HAB"), delete = TRUE)

# 9. AN + HABITAT + NestAge
#M09 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB + NestAge)), groups = c("YEAR", "HAB"), delete = TRUE)

# 11. AN + NestAge
#M11 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge)), groups = "YEAR", delete = TRUE)

# 14. AN + SUPPL + NestAge
#M14 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + NestAge)), groups = c("YEAR", "SUPPL"), delete = TRUE)

# 15. AN * SUPPL
#M15 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ -1 + YEAR:SUPPL)), groups = c("YEAR", "SUPPL"), delete = TRUE)

# 16. AN*SUPPL + HAB*SUPPL
#M16 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR * SUPPL + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 17. AN*SUPPL + HAB*SUPPL + NestAGe
#M17 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*SUPPL + HAB*SUPPL + NestAge)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)

# 18. AN*SUPPL + HAB + NestAge
#M18 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*SUPPL + HAB + NestAge)), groups = c("YEAR", "SUPPL", "HAB"), delete = TRUE)
  
# 19. AN*SUPPL + HAB
#M19 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR*SUPPL)), groups = c("YEAR", "SUPPL"), delete = TRUE)

# 20. SUPPL*HAB
M20 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB)), groups = c("SUPPL", "HAB"), delete = TRUE)


return(collect.models() )
}

# run defined models
geese.results <- run.geese()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
geese.results # print model-selection table to screen

#save(geese.results, file = "MARK_models_V2.rda") # without weather variables
#save(geese.results, file = "MARK_models_V3.rda") # with temperature and precipitation variables
#save(geese.results, file = "MARK_models_V4.rda") # only with precipitation variable
#save(geese.results, file = "MARK_models_V5.rda") # only with temperature variable

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
