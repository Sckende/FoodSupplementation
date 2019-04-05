#### Database building with one row for each per nest ########
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())
list.files()

# File with supplemented nests
sup <- read.csv("GOOSE_SHAFFER_all_SUPPL_nests_all_years.txt", sep = "\t", h = T)[,-c(6:8, 15:18)]
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
table(sup$INITIATION, useNA = "always") # see "IMP_169", "INC", and NA
ini.pb <- sup[which(sup$INITIATION == "IMP_169" | sup$INITIATION == "INC" | is.na(sup$INITIATION)),] 
# *** Here problem with the estimation of the initiation date with early predated nests ! ***
# Possibility to delete control nests in this subset, nests with "DEAD_EGG", and the only nest of 2015 (step 1)
#but for the remaining supplemented nests
# have to keep these rows and use a modelisation of the relationship between the FirstFound date and and the initiation date based on both control and suppl. nests databases
# step 1
sup <- sup[-which(sup$ID == "SH71B"),]
sup <- sup[-which(sup$INITIATION == "INC" & sup$SUPPL == "TEM"),]
sup <- sup[-which(is.na(sup$INITIATION) & sup$SUPPL == "TEM"),]
sup <- sup[-which(sup$INITIATION == "INC" & sup$HATCH == "DEAD_EGG"),]
sup <- droplevels(sup)
table(sup$INITIATION, useNA = "always")
ini.pb <- sup[which(sup$INITIATION == "INC" | is.na(sup$INITIATION)),] 
# 5 & 8 remaining nests in 2016 & 2017, respectively, with unknown initiation date

#### Relationship between the FirstFound dates and Initiation date in 2017 with linear model ####
#### **** No conclusing **** ####
goo.2017 <- goo[goo$AN == 2017,]
summary(goo.2017)

goo.2016 <- goo[goo$AN == 2016,]
summary(goo.2016)
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

#x11()
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
# For 2017 nests
mean(as.numeric(goo.2017$Init), na.rm = T)
sup$INITIATION[which(sup$INITIATION == "INC" & sup$YEAR == 2017)] <- floor(mean(as.numeric(goo.2017$Init)))
sup <- droplevels(sup)
table(sup$INITIATION)

# For 2016 nests
sup$INITIATION[which(is.na(sup$INITIATION))] <- floor(mean(as.numeric(goo.2016$Init), na.rm = T))

sup$INITIATION <- droplevels(sup$INITIATION)

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

#### Ordering data per ID ####
sup.2 <- sup.2[order(sup.2$ID),]
# Subset to find information about visit date
#dd <- sup.2[,c(1, 2, 3, 4, 5, 8, 16)]
#write.table(dd, "GOOSE_suppl_nests_TO_COMPLETE.csv")

##### Delete Control nest in wetland Peksek cause repetition with colony nests ####
sup.2 <- sup.2[!(sup.2$SUPPL == "TEM" & sup.2$HAB == "WET"),] 

#### VISIT_DATE matching with new completed file ####
date <- read.csv("GOOSE_suppl_nests_visit_date_COMPLETED.csv", h = T, sep = ";")

head(date)
date <- date[order(date$ID),]
names(date)[3] <- "coco"
#sup.2$VISIT_DATE <- date$VISIT_DATE[match(sup.2$ID, date$ID)]
sup.2 <- cbind(sup.2, coco = date$coco, VISIT_DATE = date$VISIT_DATE)

table(as.character(sup.2$ID) == as.character(sup.2$coco)) # Check point to see whether all ID are the same between "ID" and "coco"
# Need to have 1476 TRUE!
sup.2 <- sup.2[,-19]
summary(sup.2)

#### Cleaning of the HATCH variable ####
  # For successful nests with impossible hatching dates (HATCH ==   "IMP_191" or "IMP_193"), use mean date between LastPresent and LastVisit

sup.2$HATCH <- as.character(sup.2$HATCH)

 for(i in 1:length(sup.2$ID)){
   if(sup.2$NIDIF[i] == "S"){
     if(sup.2$HATCH[i] == "IMP_191"){
       sup.2$HATCH[i] <- round(mean(c(sup.2$LastPresent[i], sup.2$LastVisit[i])))
       }else{
     if(sup.2$HATCH[i] == "IMP_193"){
       sup.2$HATCH[i] <- round(mean(c(sup.2$LastPresent[i], sup.2$LastVisit[i])))
     }
   }
    
   }
   
 }

  # Failed nest has a NA for the date of hatching
sup.2$HATCH[sup.2$NIDIF == "F"] <- NA
sup.2$HATCH <- as.numeric(sup.2$HATCH)
 
#### Computation of exposition intervals ####
sup.3 <- NULL
for(i in unique(sup.2$ID)){
  if(unique(sup.2$NIDIF[sup.2$ID == i]) == "S"){
    EXPO <- diff(sup.2$VISIT_DATE[sup.2$ID == i])
    dat <- sup.2[sup.2$ID == i,][-1,] #Delete the first visit of the nest
    dat <- cbind(dat, EXPO = EXPO)
    
    if(unique(dat$HATCH) != unique(dat$LastVisit)){
      # if date of LastVisit != date of HATCH, modification of the last EXPO value using the HATCH date
      dat$EXPO[length(dat$EXPO)] <- unique(dat$HATCH) - tail(dat$VISIT_DATE, n = 2)[1] 
    }
    
    sup.3 <- rbind(sup.3, dat)
    
  }else{
    EXPO <- diff(sup.2$VISIT_DATE[sup.2$ID == i])
    dat <- sup.2[sup.2$ID == i,][-1,] #Delete the first visit of the nest, so the nests with only one re-vist display only row (= only one exposition interval) in this dataset
    dat <- cbind(dat, EXPO = EXPO)
    dat$EXPO[length(dat$EXPO)] <- ceiling(tail(EXPO, n = 1)/2) # mean value corresponding to the incertainty of when the fail occurs 
    
    sup.3 <- rbind(sup.3, dat)
  }
}
summary(sup.3)
unique(sup.2$ID) == unique(sup.3$ID)
#utils::View(sup.3)

#### CREATION OF THE COMPLETE DATASET ####
rm(list = ls()[-c(7, 21)])
goo.2 <- goo
names(goo.2)
names(sup.3)

#### Cleaning variables of goo dataset ####
# Deletion of Nest_found variable
goo.2 <- goo.2[,-13]

# Nesting success variable


# Date of HATCH variable --- **** Do that after regularisation of the NIDIF variable !!! ***
table(goo.2$Eclo_est, useNA = "always")
table(goo.2$Eclo_reel, useNA = "always")
for(i in 1:dim(goo.2)[1]){
  if(goo.2$NIDIF[i] == "S"){
    if(!is.na(goo.2$Eclo_reel[i])){
      goo.2$HATCH[i] <- goo.2$Eclo_reel[i] 
    }else{
      goo.2$HATCH[i] <- goo.2$Eclo_est[i]
    }
  }else{
    goo.2$HATCH <- NA
  }
}


#### Add "Nest_type" variable to fit with the goo db ####
sup.3$Nest_type <- "Supplementation"
