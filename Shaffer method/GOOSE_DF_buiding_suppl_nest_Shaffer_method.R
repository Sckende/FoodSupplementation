#### Database building with one row for each per nest ########
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())
list.files()

# File with supplemented nests
sup <- read.csv("GOOSE_SHAFFER_all_SUPPL_nests_all_years.txt", sep = "\t", h = T)[,-c(3, 4, 6:8, 15:18)]
names(sup)
summary(sup)

# File with detailled nest monitoring in colony - 2005, 2015 - 2017
y <- c(2005, 2015:2017)

goo <- data.frame()
for(i in y){
  q <- read.csv(paste("GOOSE_GSGO_", i, ".csv", sep = ""), h = T, sep = ";", stringsAsFactors = FALSE) 
  goo <- rbind(goo, q)
}
summary(goo)

# Replace na by NA in all the dataframe
goo[goo == "na"] <- NA

# Correction of the first variable name
names(goo)[1] <- "AN"


#### Data cleaning of SUPPLEMENTED nests monitoring ####
# Check if all ID are unique in the dataframe 
length(unique(sup$ID)) == dim(sup)[1]
sup$ID

# Deletion of unused row - SUPPL == WF | PRED_BEF_SUPPL
sup <- droplevels(sup[sup$SUPPL != "WF" & sup$SUPPL != "PRED_BEF_SUPPL",])

# Nest checking with is.na(NIDIF) - GM2W (2016) & TH15 (2016) -
sup[is.na(sup$NIDIF),]
sup$NIDIF[sup$ID == "TH15"] <- "S"
sup <- sup[!sup$ID == "GM2W",]

# Nest checking with is.na(HATCH_STATUS)
sup[is.na(sup$HATCH_STATUS),]
sup <- sup[!is.na(sup$HATCH_STATUS),]

# Nest checking with is.na(LastPresent/LastVisit)
sup[is.na(sup$LastPresent),] # corresponds to a non-hatched nest ==> delete
sup <- sup[!is.na(sup$LastPresent),]

# Nest checking with weirdo initiation date
table(sup$INITIATION) # see "IMP_169" and "INC"
ini.pb <- sup[which(sup$INITIATION == "IMP_169" | sup$INITIATION == "INC"),] 
# *** Here problem with the estimation of the initiation date with early predated nests ! ***
# Possibility to delete control nests in this subset, nests with "DEAD_EGG", and the only nest of 2015 (step 1)
#but for the remaining supplemented nests
# have to keep these rows and use a modelisation of the relationship between the FirstFound date and and the initiation date based on both control and suppl. nests databases
# step 1
sup <- sup[-which(sup$ID == "SH71B"),]
sup <- sup[-which(sup$INITIATION == "INC" & sup$SUPPL == "TEM"),]
sup <- sup[-which(sup$INITIATION == "INC" & sup$HATCH == "DEAD_EGG"),]
sup <- droplevels(sup)
# 8 remaining nests in 2017 with unknown initiation date
ini.pb <- sup[which(sup$INITIATION == "INC"),]

#### Relationship between the FirstFound dates and Initiation date in 2017 with linear model ####
#### **** No conclusing **** ####
goo.2017 <- goo[goo$AN == 2017,]
summary(goo.2017)

# Keep only the nest with initiation date
goo.2017 <- goo.2017[!is.na(goo.2017$Init),]
goo.2017 <- droplevels(goo.2017)
summary(goo.2017)
length(unique(goo.2017$No_ter))

j <- unique(as.character(goo.2017$No_ter))
ini.mod <- NULL
for(i in j){
  ini <- unique(goo.2017$Init[goo.2017$No_ter == i])
  FF <- goo.2017$Date[goo.2017$No_ter == i][1]
  
  
  ini.mod <- rbind(ini.mod, c(i, ini, FF))
}
ini.mod <- as.data.frame(ini.mod)
names(ini.mod) <- c("ID", "Ini", "FF") 
ini.mod$Ini <- as.numeric(as.character(ini.mod$Ini))
ini.mod$FF <- as.numeric(as.character(ini.mod$FF))
summary(ini.mod)

x11()
plot(ini.mod$FF, ini.mod$Ini)
hist(ini.mod$Ini)

scatter.smooth(x = ini.mod$FF,
               y = ini.mod$Ini,
               main = "Ini ~ FF")  # scatterplot
par(mfrow = c(1, 2))
boxplot(ini.mod$Ini)
boxplot(ini.mod$FF)

cor(ini.mod$Ini, ini.mod$FF) # No correlation between these two variables....



#### Use of the mean date Initiation for one specific FF date based on the control nests data ####
# for 2017 --> FF = 173 / 170 / 162 / 171 / 167
#### **** Not conclusing **** ####

d.FF <- c(173 , 170 , 162 , 171 , 167)
Inidate.comp <- NULL
for(i in d.FF){
  m <- mean(ini.mod$Ini[ini.mod$FF == i])
  dim <- length(ini.mod$Ini[ini.mod$FF == i])
  
  Inidate.comp <- rbind(Inidate.comp, c(i, m, dim))
}
Inidate.comp <- as.data.frame(Inidate.comp)
names(Inidate.comp) <- c("FF", "Ini.mean", "n")
Inidate.comp <- Inidate.comp[order(Inidate.comp$FF),]
Inidate.comp


#### Mean date of the initiation date for all control nests ####
#### **** Used value for the 8 nests without initiation date **** ####
mean(as.numeric(goo.2017$Init))
sup$INITIATION[which(sup$INITIATION == "INC")] <- floor(mean(as.numeric(goo.2017$Init)))
sup <- droplevels(sup)
table(sup$INITIATION)

#### Adding of rows for each visit in SUPPLEMENTED nests data ####
summary(sup)
length(unique(sup$ID)) == dim(sup)[1]
sup.2 <- sup
#subbb <- droplevels(sup[sup$VISIT_NUMBER >= 2, ])

for(i in sup$ID){
  if(sup$VISIT_NUMBER[sup$ID == i] >= 2){
    matr <- matrix(rep(c(t(sup[sup$ID == i,])), as.numeric(sup$VISIT_NUMBER[sup$ID == i])), byrow = T, ncol = dim(sup)[2])
    matr <- as.data.frame(matr)
    names(matr) <- names(sup)
    
    sup.2 <- rbind(sup.2, as.data.frame(matr))
  }else{
    matr <- sup[sup$ID == i,]
    
    sup.2 <- rbind(sup.2, matr)
  }
  
}

sup.2$LastPresent <- as.numeric(as.character(sup.2$LastPresent))
sup.2$LastVisit <- as.numeric(as.character(sup.2$LastVisit))
sup.2$FirstFound <- as.numeric(as.character(sup.2$FirstFound))
sup.2$INITIATION <- as.numeric(as.character(sup.2$INITIATION))
sup.2$VISIT_NUMBER <- as.numeric(as.character(sup.2$VISIT_NUMBER))
sup.2$CLUTCH <- as.numeric(as.character(sup.2$CLUTCH))
sup.2$YEAR <- as.numeric(sup.2$YEAR)
summary(sup.2)

# Ordering data per ID
sup.2 <- sup.2[order(sup.2$ID),]
# Subset to find information about visit date
#dd <- sup.2[,c(1, 2, 3, 4, 5, 8, 16)]
#write.table(dd, "GOOSE_suppl_nests_TO_COMPLETE.csv")

# Delete Control nest in wetland Peksek cause repetition with colony nests
sup.2 <- sup.2[!(sup.2$SUPPL == "TEM" & sup.2$HAB == "WET"),]

# VISIT_DATE matching with new completed file
date <- read.csv("GOOSE_suppl_nests_visit_date_COMPLETED.csv", h = T, sep = ";")
head(date)

sup.2$VISIT_DATE <- date$VISIT_DATE[match(sup.2$ID, date$ID)]

#### **** Warning - Special care for nests with only one visit ! **** ####
#### **** Division of the variable sup.2$HATCH **** ####
### **** Warning for the computation of the last exposition interval for each nest **** ####

