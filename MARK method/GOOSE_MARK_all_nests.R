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
 ############## ATTENTION' VOIR ICI POUR LES ÉGALITÉS ENTRE LES DATES !!!!! ##################

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
x11()
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
X11()
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

legend("topright", 
       #inset = c(0, -0,05),
       legend = c("CONTROL", "WATER", "FOOD"), 
       fill = c("olivedrab3", "aquamarine3", "darkgoldenrod2"), bty = "n", cex = 1)
segments(barCenters, prop2$PROP - prop2$error_type, barCenters, prop2$PROP + prop2$error_type, lwd = 1.5)
text(barCenters,0.2, labels = paste("(", as.character(prop2$N), ")", sep = ""), cex = 1)
text(3.3, 1.1, labels = 2015, cex = 2)
text(9.9, 1.1, labels = 2016, cex = 2)
text(16.5, 1.1, labels = 2017, cex = 2)

dev.off()

#### Add type of rainfall year ####
cum2 <- read.table("PREC_cum2.txt",  dec = ".", h = T)
gsg$RAINFALL <- cum2$RAINFALL[match(gsg$YEAR, cum2$YEAR)]

#### Add cumulative precipitation for each nests ####
rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", dec = ",", h = T)
rain <- rain[!is.na(rain$RAIN),]
gsg$YEAR <- as.numeric(as.character(gsg$YEAR))

gsg$X <- 1:dim(gsg)[1]
for (i in gsg$X) {
gsg$prec[i] <- 
  sum(rain$RAIN[which(rain$YEAR == gsg$YEAR[i] & rain$JJ >= gsg$INITIATION[i] & rain$JJ<= gsg$LastPresent[i])])
}

#### Data Analyses ####
# Here choose one specific year or not
geese <- gsg[gsg$YEAR == "2015" | gsg$YEAR == "2016" | gsg$YEAR == "2017",]
geese <- droplevels(geese)
summary(geese)
dim(geese)

#Creation of AgeFound variable#
#--------------------------------#
#WARNING ! It has to be done before the modification of the FirstFound variable
geese$AgeFound <- (geese$FirstFound - geese$INITIATION) + 1 #...+1 cause age 0 is impossible
geese$FindNest <- geese$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
#Attention ici valeur change selon les annees
FF <- min(geese$FirstFound) #date minimum = 160
geese$FirstFound <- geese$FirstFound - (FF - 1) #ici 159 = 160 - 1 (pour Day 1)

#Idem pour les variables LastPresent, LastChecked#
#-------------------------------------------------#
geese$LastPresent <- geese$LastPresent - (FF - 1)
geese$LastChecked <- geese$LastChecked - (FF - 1)

#Obtention de la variable AgeDay1#
#--------------------------------#
#correspond à l'âge du nid lors du premier jour du suivi de nids
geese$AgeDay1 <- (geese$AgeFound - geese$FirstFound) + 1


# valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(geese$LastChecked)

require(RMark)
 #Check point
table(geese$LastChecked[which(geese$Fate == "1")] == geese$LastPresent[which(geese$Fate == "1")], useNA = "always")
geese[geese$Fate == "1" & geese$LastPresent==geese$LastChecked,]
geese$LastChecked[geese$Fate == "1" & geese$LastPresent==geese$LastChecked] <- geese$LastChecked[geese$Fate == "1" & geese$LastPresent==geese$LastChecked] + 1 # correction of LastChecked == LAstPresent for failed nests






run.geese=function()
{
  
# 0. A model of constant daily survival rate (DSR)
M0 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~1)))

# 00. year effect
M00 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR")

# 000. habitat effect
M000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB")

# 0000. supplementation effect
M0000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL")

# 00000. NestAge effect
M00000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)))

# 000000. habitat*NestAge
M000000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB*NestAge)), groups = "HAB")

# 1. AN + SUPPL
M01 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL)), groups = c("YEAR", "SUPPL"))


# 2. AN + SUPPL + HABITAT
M02 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + HAB)), groups = c("YEAR", "SUPPL", "HAB"))

# 3. AN + SUPPL + HABITAT + HABITAT*SUPPL
M03 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"))

# 4. AN + SUPPL + HABITAT + NestAge
M04 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + HAB + NestAge)), groups = c("YEAR", "SUPPL", "HAB"))

# 5. AN + NestAge + HABITAT*SUPPL
M05 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"))

# 8. AN + HABITAT
M08 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB)), groups = c("YEAR", "HAB"))

# 9. AN + HABITAT + NestAge
M09 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB + NestAge)), groups = c("YEAR", "HAB"))

# 11. AN + NestAge
M11 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + NestAge)), groups = "YEAR")

# 14. AN + SUPPL + NestAge
M14 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + NestAge)), groups = c("YEAR", "SUPPL"))

# 15. AN * SUPPL
M15 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR * SUPPL)), groups = c("YEAR", "SUPPL"))

return(collect.models() )
}

# run defined models
geese.results <- run.geese()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
geese.results # print model-selection table to screen

#################### Best model for full database ####################
############## only considering models without interaction ##########
# Two best model
# 4. YEAR + SUPPL + HABITAT + NestAge
M04 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + SUPPL + HAB + NestAge)), groups = c("YEAR", "SUPPL", "HAB"))
# 3. AN + SUPPL + HABITAT + HABITAT*SUPPL
M03 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB*SUPPL)), groups = c("YEAR", "SUPPL", "HAB"))

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
