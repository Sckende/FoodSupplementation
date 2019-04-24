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

#### Check if there are ducplication in date visit for each nest ####
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


#### Change the names of variable if possible ####
goo.2 <- goo[,-16]
summary(goo.2)

names(goo.2)[-c(5, 6, 9, 12:14)] <- c("YEAR", "ID", "INITIATION", "INI_STATUS", "CLUTCH", "VISIT_DATE", "HAB", "ZONE", "NIDIF")

#### Habitat ####
goo.2$HAB[goo.2$HAB == "Mesic"] <- "MES"
goo.2$HAB[goo.2$HAB == "Wetland"] <- "WET"
goo.2$HAB[goo.2$HAB == "Wetland/mesic"] <- NA

goo.2$HAB <- as.factor(goo.2$HAB)

#### Zone ####
table(goo.2$ZONE)
goo.2$ZONE[goo.2$ZONE == "C2-riviere camp 2" | goo.2$ZONE == "C2-riviere du camp 2"] <- "C2-riviere camp 2"

goo.2 <- goo.2[!(goo.2$ZONE == "C1-Lac aux Goelands" | goo.2$ZONE == "C1-rive sud"),] # Deletion of C1 nests
goo.2$ZONE <- as.factor(goo.2$ZONE)

#### Nest_type -> deletion of "collared female" status ####
table(goo.2$Nest_type)
goo.2 <- goo.2[!(goo.2$Nest_type == "Collared female" | goo.2$Nest_type == "N/A" | goo.2$Nest_type == "Marked female"),]

#### ID / INITIATION / CLUTCH / INI_STATUS / Nest_type / NIDIF / Eclo_est / Eclo_reel ####
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


#### HACTCH and HATCH_STATUS ####
table(goo.2$Eclo_est, useNA = "always")
table(goo.2$Eclo_reel, useNA = "always")

for(i in 1:nrow(goo.2)){
  if(goo.2$NIDIF[i] == 1){
    if(is.na(goo.2$Eclo_reel[i]) == F){
      goo.2$HATCH[i] <- goo.2$Eclo_reel[i]
      goo.2$HATCH_STATUS[i] <- "TRUE"
    }else{
      goo.2$HATCH[i] <- goo.2$Eclo_est[i]
      goo.2$HATCH_STATUS[i] <- "EST"
    }
  }else{
    goo.2$HATCH[i] <- NA
    goo.2$HATCH_STATUS[i] <- NA 
  }
}
goo.2$HATCH_STATUS <- as.factor(goo.2$HATCH_STATUS)
# Deletion of 19 rows with no hatch date and no clutch size
goo.2 <- goo.2[!(is.na(goo.2$HATCH) & goo.2$NIDIF == 1),]

#### Deletion of nests for which is.na(INITIATION) & is.na(CLUTCH) ####
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

#### Creation of VISIT_NUMBER & FirstFound ####
goo.2 <- goo.2[order(goo.2$YEAR, goo.2$ID, goo.2$VISIT_DATE),] # To be sure that the visit dates are ordered

pp <- lapply(pp, function(x){
  x$VISIT_NUMBER <- nrow(x) - 1 # The first visit, when the nest was found, is not included in the number of visit 
  x$FirstFound <- x$VISIT_DATE[1]
  x
})

goo.2 <- do.call("rbind", pp)
table(goo.2$VISIT_NUMBER)
goo.2 <- goo.2[!(goo.2$VISIT_NUMBER == 0),] # deletion of nest with no re-visit

#### ***Creation of EXPO*** ####
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

#### Check for failed nests with repeated failed status (= 8)
pp <- lapply(pp, function(x){
  if(x$NIDIF[1] == 0){
    if(any(duplicated(x$Stade, incomparables = x$Stade[x$Stade < 8]))){
      x <- x[-nrow(x),]
      x
    }else{
      x 
    }
  }
})

# Here error in stade code for failed nests
# lapply(rr, function(x){
#   if(x$Stade[nrow(x)] != 8){
#     x
#   }
# })

#### Creation of missing variable to fit with the supplementation nests database ####
goo.2$SUPPL <- "TEM"
goo.2$SUPPL_DATE <- NA

goo.3 <- goo.2[,-c(5, 6, 9)]

#### DATA CLEANING OF SUPPLEMENTED NEST MONITORING ####
#### Check if all ID are unique in the dataframe ####
length(unique(sup$ID)) == nrow(sup)
sup$ID

#### Deletion of unused row - SUPPL == WF | PRED_BEF_SUPPL ####
sup <- droplevels(sup[sup$SUPPL != "WF" & sup$SUPPL != "PRED_BEF_SUPPL",])

#### Nest checking with is.na(NIDIF) - GM2W (2016) & TH15 (2016) - ####
sup[is.na(sup$NIDIF),]
sup$NIDIF[sup$ID == "TH15"] <- "S" # Success in raw data
sup <- sup[!sup$ID == "GM2W",] # Unknown in raw data -> deletion

#### Nest checking with is.na(HATCH_STATUS) ####
sup[is.na(sup$HATCH_STATUS),]
sup <- sup[!is.na(sup$HATCH_STATUS),] # 2 nests, one of them is a non-hatched nest ==> deletion

#### Nest checking with is.na(LastPresent/LastVisit) ####
sup[is.na(sup$LastPresent),] # corresponds to a non-hatched nest ==> delete
sup <- sup[!is.na(sup$LastPresent),]

#### Nest checking with weirdo initiation date ####
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
goo.2017 <- goo.3[goo.3$YEAR == 2017,]
summary(goo.2017)

goo.2016 <- goo.3[goo.3$YEAR == 2016,]
summary(goo.2016)
# Keep only the nest with initiation date
goo.2017 <- goo.2017[!is.na(goo.2017$INITIATION),]
goo.2017 <- droplevels(goo.2017)
summary(goo.2017)
length(unique(goo.2017$ID))



tt <- split(goo.3, paste(goo.3$YEAR, goo.3$ID))
ini.mod <- lapply(tt, function(x){
  INI <- x$INITIATION[1]
  FF <- x$FirstFound[1]
  NEST <- as.character(x$ID[1])
  YEAR <- x$YEAR[1]
  x <- c(YEAR, NEST, INI, FF)
  })

ini.mod <- as.data.frame(do.call("rbind", ini.mod))
names(ini.mod) <- c("YEAR", "NEST", "INI", "FF")
ini.mod$YEAR <- as.numeric(as.character(ini.mod$YEAR))
ini.mod$INI <- as.numeric(as.character(ini.mod$INI))
ini.mod$FF <- as.numeric(as.character(ini.mod$FF))
summary(ini.mod)

x11()
par(mfrow = c(2,2))

s <- split(ini.mod, ini.mod$YEAR)

lapply(s, function(x){
  x <- na.omit(x)
  plot(x$FF, x$INI, main = x$YEAR[1], bty = "n", xlab = "FirstFound date", ylab = "Initiation date")
  summary(lm(x$INI ~ x$FF))
})

x11()
par(mfrow = c(2,2))

lapply(s, function(x){
  x <- na.omit(x)
  h1 <- hist(x$INI, plot = F)
  h2 <- hist(x$FF, plot =F)
  #browser()
  y <- max(h1$counts, h2$counts)
  
  hist(x$INI,
       xlim = c(min(x$INI), max(x$FF)),
       ylim = c(0, ceiling(y)),
       col = rgb(1, 0, 0, 1/4),
       breaks = length(min(x$INI):max(x$INI)),
       main = x$YEAR[1],
       xlab = "Julian date")
  hist(x$FF,
       xlim = c(min(x$INI),max(x$FF)),
       ylim = c(0, ceiling(y)),
       col = rgb(0, 0, 1, 1/4),
       breaks = length(min(x$FF):max(x$FF)),
       add = T,
       main = "",
       xlab = "")
  legend(min(x$INI),
         y,
         c("Initiation", "FirstFound"),
         fill = c(rgb(1, 0, 0, 1/4), rgb(0, 0, 1, 1/4)),
         bty = "n")
  
})

scatter.smooth(x = ini.mod$FF,
               y = ini.mod$INI,
               main = "Ini ~ FF")  # scatterplot
par(mfrow = c(1, 2))
boxplot(ini.mod$INI)
boxplot(ini.mod$FF)

ini.mod <- na.omit(ini.mod)
cor(ini.mod$INI, ini.mod$FF) # No correlation between these two variables....



#### Use of the mean date Initiation for one specific FF date based on the control nests data ####
# for 2017 --> FF = 173 / 170 / 162 / 171 / 167
#### **** Not conclusing **** ####

d.FF <- c(173 , 170 , 162 , 171 , 167)
Inidate.comp <- NULL
for(i in d.FF){
  m <- mean(ini.mod$INI[ini.mod$FF == i])
  dim <- length(ini.mod$INI[ini.mod$FF == i])
  
  Inidate.comp <- rbind(Inidate.comp, c(i, m, dim))
}
Inidate.comp <- as.data.frame(Inidate.comp)
names(Inidate.comp) <- c("FF", "Ini.mean", "n")
Inidate.comp <- Inidate.comp[order(Inidate.comp$FF),]
Inidate.comp


#### Mean date of the initiation date for all control nests ####
#### **** Used value for nests without initiation date **** ####
# For 2017 nests
mean(goo.2017$INITIATION, na.rm = T)
sup$INITIATION[which(sup$INITIATION == "INC" & sup$YEAR == 2017)] <- floor(mean(as.numeric(goo.2017$INITIATION)))
sup <- droplevels(sup)
table(sup$INITIATION)

# For 2016 nests
sup$INITIATION[which(is.na(sup$INITIATION))] <- floor(mean(goo.2016$INITIATION, na.rm = T))

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
exp <- split(sup.2, paste(sup.2$YEAR, sup.2$ID))
tt <- lapply(exp, function(x){
  if(x$NIDIF[1] == "S"){
    x$VISIT_DATE[nrow(x)] <- x$HATCH[1]
    x
  }else{
    x$VISIT_DATE[nrow(x)] <- ceiling(mean(x$VISIT_DATE[nrow(x)], x$VISIT_DATE[nrow(x) - 1]))
    x
  }
  expos <- diff(x$VISIT_DATE)
  x <- x[-1,]
  x$EXPO <- expos
  x
})

sup.3 <- do.call("rbind", tt)
summary(sup.3)

table(unique(sup.2$ID) == unique(sup.3$ID)) # Check point

# Issue of nidification
sup.3$NIDIF <- as.character(sup.3$NIDIF)
sup.3$NIDIF[sup.3$NIDIF == "F"] <- 0
sup.3$NIDIF[sup.3$NIDIF == "S"] <- 1

sup.3$NIDIF <- as.numeric(sup.3$NIDIF)
#utils::View(sup.3)

#### CREATION OF THE COMPLETE DATASET ####
rm(list = ls()[-c(10, 22)])

names(goo.3)
names(sup.3)
sup.3 <- sup.3[, -18]

data <- merge(goo.3, sup.3, all = TRUE)
data$SUPPL_DATE <- as.numeric(data$SUPPL_DATE)
data$UTM_E <- as.numeric(data$UTM_E)
data$UTM_N <- as.numeric(data$UTM_N)
summary(data)
#### Computation of NestAge variable ####
# Check of INITIATION date 

# ini.pb <- data[is.na(data$INITIATION),]
# summary(ini.pb)
# table(ini.pb$YEAR)
# table(ini.pb$NIDIF)
# 
# 
# ini <- split(ini.pb, paste(ini.pb$YEAR, ini.pb$ID))
# 
# tt <- lapply(ini, function(x){
#   if(x$NIDIF[1] == 1){
#     if(x$CLUTCH[1] <= 3){
#       x$INITIATION <- x$HATCH[1] - (23 + x$CLUTCH[1] - 1)
#       x$INI_STATUS <- "HATCH_EST"
#       x
#     }
#     if(x$CLUTCH[1] > 3){
#       x$INITIATION <- x$HATCH[1] - (23 + x$CLUTCH[1])
#       x$INI_STATUS <- "HATCH_EST"
#       x
#     }
#    }
#   x$INITIATION <- floor(mean(goo.3$INITIATION[goo.3$YEAR == x$YEAR[1]], na.rm = T))
#   x$INI_STATUS <- "MEAN"
#   x
# })
# 
# tt <- do.call("rbind", tt)
# summary(tt)
# 
# table(tt$NIDIF)
# tt[tt$NIDIF == 0,]

# Use hatch date and clutch size to estimate initiation date

# Test --> ok
tt <- split(data, paste(data$YEAR, data$ID))

tt <- lapply(tt, function(x){
  if(is.na(x$INITIATION[1])){
    if(x$NIDIF[1] == 1){
      if(x$CLUTCH[1] <= 3){
        x$INITIATION <- x$HATCH[1] - (23 + x$CLUTCH[1] - 1)
        x$INI_STATUS <- "HATCH_EST"
        x
      }
      if(x$CLUTCH[1] > 3){
        x$INITIATION <- x$HATCH[1] - (23 + x$CLUTCH[1])
        x$INI_STATUS <- "HATCH_EST"
        x
      }
    }
    x$INITIATION <- floor(mean(goo.3$INITIATION[goo.3$YEAR == x$YEAR[1]], na.rm = T))
    x$INI_STATUS <- "MEAN"
    x
  }
  x
})
tt <- do.call("rbind", tt)

data <- tt
summary(data)

# Check whether INITIATION DATE > FirstFound DATE AND deletion of them
tt <- split(data, paste(data$YEAR, data$ID))
 tttt <- lapply(tt, function(x){
   if(x$INITIATION[1] <= x$FirstFound[1]){
     x
   }
 })

tttt <- tttt[!sapply(tttt, is.null)] #Nest with FirstFound DATE posterior or equal to the INITIATION DATE

tttt <- do.call("rbind", tttt)
data <- tttt

# Calcul of NestAge
tttt <- split(data, paste(data$YEAR, data$ID))

tttt <- lapply(tttt, function(x){
  x$NestAge <- x$VISIT_DATE - x$INITIATION[1]
  x
})

data <- lapply(tttt, function(x){
  if(all(x$NestAge <= 35)){
    x
  }
    })
data <- do.call("rbind", data)
summary(data)

head(data)

#### Fill NA for habitat with GPS points ####
library(raster)
library(sp)
library(rgdal)
library(mapview)
library(sf)

pol<-readOGR("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/QGIS/GIS BYLOT/wetland_C2_region.shp")

# Projection system
proj4string(pol)

d<-data
coordinates(d)<-~UTM_E+UTM_N # Transforme un dataframe par un dataframe spacial et specifie les coordonnées dans l'ordre ~ X + Y
# Homogeneization of projection system
proj4string(d)<-proj4string(pol)

mapview(list(st_as_sf(pol), st_as_sf(d)))
# option "show in a new window" allows to see online
# sf = simple feature
# st = 
o <- over(d, pol) # Check the superposition between d (GPS points) and pol (wetland polygons) - Be carefull, Tricky command!
# Good idea to check if o is the same length of GPS points vector

h <- ifelse(is.na(o$WETLANDS), "MES", "WET") # If gps point is not superposed with a wetland polygion, NA is returned

d$HAB2<-ifelse(!d$HAB %in% c("MES", "WET"), h, as.character(d$HAB)) # Replacement of NA in HAB
table(d$HAB == d$HAB2, useNA="always")

data$HAB2 <- as.factor(d$HAB2)
summary(data)
data$YEAR <- as.factor(data$YEAR)
data$SUPPL <- as.factor(data$SUPPL)
data$SUPPL <- relevel(data$SUPPL, "TEM")

#### Modification of NIDIF for the first visits to failed nests ####
tt <- split(data, paste(data$YEAR, data$ID))

tt <- lapply(tt, function(x){
  if(x$NIDIF[1] == 0){
    if(nrow(x) > 1){
      x$NIDIF[1:(nrow(x)-1)] <- 1
    }
  }
  x
})

data <- do.call("rbind", tt) # Doesn't work if there are null in the list

#### Addition of local climate variables ####

# RAINFALL
rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", dec = ",", h = T)
summary(rain)
rain <- na.omit(rain)

# TEMPERATURE
deg <- read.table("TEMP_PondInlet_1995-2017.csv", h = T, dec =".", sep = ";")
#deg <- na.omit(deg)
deg$JJ <- strptime(as.character(deg$Date), format = "%Y-%m-%d")
deg$JJ <- deg$JJ$yday + 1 
summary(deg)

deg$Mean_Temp[deg$Year == 2016 & deg$JJ >= 180 & deg$JJ <= 195 & is.na(deg$Mean_Temp)] <- c(7.6, 4.8, 4.5, 3.5)
deg$Mean_Temp[deg$Year == 2015 & deg$JJ >= 164 & deg$JJ <= 199 & is.na(deg$Mean_Temp)] <- 5.8 # Data from http://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2004-09-18%7C2019-04-14&dlyRange=2005-01-26%7C2019-04-14&mlyRange=2005-04-01%7C2007-11-01&StationID=43223&Prov=NU&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=5&searchMethod=contains&Month=7&Day=1&txtStationName=Pond+Inlet&timeframe=2&Year=2016
# Local climate variables
local <- split(data, paste(data$YEAR, data$ID))
local <- lapply(local, function(x){
  #browser()
  # Temperature
  x$TEMP_NIDIF <- mean(deg$Mean_Temp[deg$Year == x$YEAR[1] & deg$JJ >= x$FirstFound[1] & deg$JJ <= x$VISIT_DATE[nrow(x)]])
  x$TEMP_NIDIF_sd <- sd(deg$Mean_Temp[deg$Year == x$YEAR[1] & deg$JJ >= x$FirstFound[1] & deg$JJ <= x$VISIT_DATE[nrow(x)]])
  # Precipitation
  x$PREC_NIDIF <- sum(rain$RAIN[rain$YEAR == x$YEAR[1] & rain$JJ > x$FirstFound[1] & rain$JJ <= x$VISIT_DATE[nrow(x)]])
  expo_date <- c(x$FirstFound[1], x$VISIT_DATE)
  temp.expo <- NULL
  prec.expo <- NULL
  for(i in 2:length(expo_date)){
    #browser()
    q <- mean(deg$Mean_Temp[deg$Year == x$YEAR[1] & deg$JJ >= expo_date[i - 1] & deg$JJ <= expo_date[i]])
    temp.expo[i - 1] <- q
    
    r <- sum(rain$RAIN[rain$YEAR == x$YEAR[1] & rain$JJ > expo_date[i - 1] & rain$JJ <= expo_date[i]])
    prec.expo[i - 1] <- r
  }
  x$TEMP_EXPO <- temp.expo
  x$PREC_EXPO <- prec.expo
  x$ID.2 <- paste(x$YEAR[1], x$ID[1], sep = ".")
  x
})

local[1:5]
data <- do.call("rbind", local)
row.names(data)<-1:nrow(data)
data$ID.2 <- as.factor(data$ID.2)
head(data)

summary(data)

#write.table(data, "GOOSE_SHAFFER_database_all_nests_2005_2015-2017.csv")
