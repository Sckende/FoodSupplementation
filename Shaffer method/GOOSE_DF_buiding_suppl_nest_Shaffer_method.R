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

#### DATA CLEANING OF CONTROL NESTS MONITORING ####
# Correction of the first variable name
names(goo)[1] <- "AN"

# Ordering dataframe by YEAR, ID, and VISIT_DATE

goo <- goo[order(goo$AN, goo$No_ter, goo$Date),]

# Check if there are ducplication in date for each nest
pp <- split(goo, paste(goo$AN, goo$No_ter), drop = T)
dup <- lapply(pp, function(x) {
  if(any(duplicated(x$Date))){
    x
  }
})
dup <- dup[!sapply(dup, is.null)] # Delete null object in the list and check when appear the dupicated VISIT_DATE
dup
# All of them are successful nests, so no danger to loose important information by deleting duplicated date of visit
# Approbation for deletion of duplicated VISIT_DATE
pp <- lapply(pp, function(x){
  x <- x[!duplicated(x$Date),] # Erase row with duplicated VISIT_DATE
})

goo <- do.call("rbind", pp)


# Change the names of variable if possible
goo.2 <- goo[,-16]
summary(goo.2)

names(goo.2)[-c(5, 6, 9, 12:14)] <- c("YEAR", "ID", "INITIATION", "INI_STATUS", "CLUTCH", "VISIT_DATE", "HAB", "ZONE", "NIDIF")

# Habitat
goo.2$HAB[goo.2$HAB == "Mesic"] <- "MES"
goo.2$HAB[goo.2$HAB == "Wetland"] <- "WET"
goo.2$HAB[goo.2$HAB == "Wetland/mesic"] <- NA

goo.2$HAB <- as.factor(goo.2$HAB)

# Zone
table(goo.2$ZONE)
goo.2$ZONE[goo.2$ZONE == "C2-riviere camp 2" | goo.2$ZONE == "C2-riviere du camp 2"] <- "C2-riviere camp 2"

goo.2 <- goo.2[!(goo.2$ZONE == "C1-Lac aux Goelands" | goo.2$ZONE == "C1-rive sud"),] # Deletion of C1 nests
goo.2$ZONE <- as.factor(goo.2$ZONE)

# Nest_type -> deletion of "collared female" status
table(goo.2$Nest_type)
goo.2 <- goo.2[!(goo.2$Nest_type == "Collared female" | goo.2$Nest_type == "N/A" | goo.2$Nest_type == "Marked female"),]

# ID / INITIATION / CLUTCH / INI_STATUS / Nest_type / NIDIF / Eclo_est / Eclo_reel
goo.2$ID <- as.factor(goo.2$ID)
goo.2$INITIATION <- as.numeric(goo.2$INITIATION)
goo.2$CLUTCH <- as.numeric(goo.2$CLUTCH)
goo.2$INI_STATUS <- as.factor(goo.2$INI_STATUS)
goo.2$Nest_type <- as.factor(goo.2$Nest_type)
goo.2$Eclo_reel <- as.numeric(goo.2$Eclo_reel)
goo.2$Eclo_est <- as.numeric(goo.2$Eclo_est)

goo.2$NIDIF[goo.2$NIDIF == "Yes"] <- 1
goo.2$NIDIF[goo.2$NIDIF == "No"] <- 0
goo.2$NIDIF[goo.2$NIDIF == "Unk"] <- NA

goo.2$NIDIF <- as.numeric(goo.2$NIDIF)

goo.2 <- goo.2[!is.na(goo.2$NIDIF),]


# HACTCH
table(goo.2$Eclo_est, useNA = "always")
table(goo.2$Eclo_reel, useNA = "always")

for(i in 1:dim(goo.2)[1]){
  if(goo.2$NIDIF[i] == 1){
    if(is.na(goo.2$Eclo_reel[i]) == F){
      goo.2$HATCH[i] <- goo.2$Eclo_reel[i] 
    }else{
      goo.2$HATCH[i] <- goo.2$Eclo_est[i]
    }
  }else{
    goo.2$HATCH[i] <- NA
  }
}
# Deletion of 19 rows with no hatch date and no clutch size
goo.2 <- goo.2[!(is.na(goo.2$HATCH) & goo.2$NIDIF == 1),]

# Deletion of nests for which is.na(INITIATION) & is.na(CLUTCH)
summary(goo.2[is.na(goo.2$INITIATION) & is.na(goo.2$CLUTCH),])
dim(goo.2[is.na(goo.2$INITIATION) & is.na(goo.2$CLUTCH),])
goo.2 <- goo.2[!(is.na(goo.2$INITIATION) & is.na(goo.2$CLUTCH)),]


#### Check if more than one visit after the hatch date (with HATCH == one of VISIT_DATE) ####
# If yes, deletion of following visits 
pp <- split(goo.2, paste(goo.2$YEAR, goo.2$ID))

  # TEST
# db <- lapply(pp, function(x){
#   if(x$HATCH[1] %in% x$VISIT_DATE){
#     #x
#     x <- x[!(x$VISIT_DATE > x$HATCH[1]),]
#   }
# })
# db <- db[!sapply(db, is.null)]# Give all the nest with a visit the same day of the hatch AND at least one visit after the hatch date

  # Test ok -> Deletion of the rows with unnecessary visits
pp <- lapply(pp, function(x){
  if(x$HATCH[1] %in% x$VISIT_DATE){
    x <- x[!(x$VISIT_DATE > x$HATCH[1]),]
    x
  }
  x # If the condition if() != TRUE, keep original x
})

#### Check if more than one visit after the hatch date (with HATCH between two visits) ####
# In this case, deletion of rows with visit date > HATCH and creation of one new row with the last VISIT_DATE == HATCH

 # Test
# db <- lapply(pp, function(x){
#   if(x$NIDIF[1] == 1){
#     if(any(x$HATCH[1] < x$VISIT_DATE)){
#       #x
#       x <- x[!(x$VISIT_DATE > x$HATCH[1]),]
#       x <- rbind(x, x[1,])
#       x$VISIT_DATE[nrow(x)] <- x$HATCH[1]
#       x
#     }
#   }
# }) 
# db <- db[!(sapply(db, is.null))]
# db[1:10]
  
  # Test ok -> Deletion of the rows with unnecessary visits
pp <- lapply(pp, function(x){
  if(x$NIDIF[1] == 1){
    if(any(x$HATCH[1] < x$VISIT_DATE)){
      x <- x[!(x$VISIT_DATE > x$HATCH[1]),]
      x <- rbind(x, x[1,])
      x$VISIT_DATE[nrow(x)] <- x$HATCH[1]
    }
  }
  x # If the condition if() != TRUE, keep original x
}) 

#### Creation of VISIT_NUMBER ####
# The first visit, when the nest was found, is deleted 
goo.2 <- goo.2[order(goo.2$YEAR, goo.2$ID, goo.2$VISIT_DATE),] # To be sure that the visit dates are ordered

pp <- lapply(pp, function(x){
  x$VISIT_NUMBER <- nrow(x) - 1
  x
})

goo.2 <- do.call("rbind", pp)
table(goo.2$VISIT_NUMBER)
goo.2 <- goo.2[!(goo.2$VISIT_NUMBER == 0),] # deletion of nest with no re-visit

#### Creation of EXPO ####
# For successful nests == Substraction between the VISIT_DATE 
# For fail nests == idem except for the last value. HAs to be equal to the mean time between the two last visits

  # TEST
# fail <- lapply(pp, function(x){
#   if(x$NIDIF[1] == 0){
#     browser()
#     y <- x$VISIT_DATE
#     x <- x[-1,]
#     x$EXPO <- diff(y)
#     x$EXPO[nrow(x)] <- ceiling(x$EXPO[nrow(x)]/2)
#     x
#   }
# })
# fail <- fail[!(sapply(fail, is.null))]

  # Application on the data frame
pp <- split(goo.2, paste(goo.2$YEAR, goo.2$ID))
pp <- lapply(pp, function(x){
  if(x$NIDIF[1] == 1){
    y <- x$VISIT_DATE
    x <- x[-1,]#Delete the first visit of the nest, so the nests with only one re-vist display only row (= only one exposition interval) in this dataset
    x$EXPO <- diff(y)
    x
  }else{
    y <- x$VISIT_DATE
    x <- x[-1,]#Delete the first visit of the nest, so the nests with only one re-vist display only row (= only one exposition interval) in this dataset
    x$EXPO <- diff(y)
    x$EXPO[nrow(x)] <- ceiling(x$EXPO[nrow(x)]/2)
    x
  }
})

goo.2 <- do.call("rbind", pp)
summary(goo.2)
table(goo.2$YEAR)
table(goo.2$YEAR, goo.2$HAB)
#### TO DO LIST ####
# POUR LES NIDS EN ÉCHEC
    # Repérer le premier state qui stipule l'échec
# Computation of age nest 
# Check initiation date

#### DATA CLEANING OF SUPPLEMENTED NEST MONITORING ####
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
sup.2 <- sup.2[,-19] # Erase "coco" variable
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

#### *** Computation of exposition intervals *** ####
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

unique(sup.2$ID) == unique(sup.3$ID) # Check point

# Issue of nidification
sup.3$NIDIF <- as.character(sup.3$NIDIF)
sup.3$NIDIF[sup.3$NIDIF == "F"] <- 0
sup.3$NIDIF[sup.3$NIDIF == "S"] <- 1

sup.3$NIDIF <- as.numeric(sup.3$NIDIF)
#utils::View(sup.3)

#### CREATION OF THE COMPLETE DATASET ####
rm(list = ls()[-c(7, 21)])
goo.2 <- goo
names(goo.2)
names(sup.3)


#### Add "Nest_type" variable to fit with the goo db ####
sup.3$Nest_type <- "Supplementation"
