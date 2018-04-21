getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())


gsg <- read.table("GOOSE_MARK_all_nests.csv", h = T, dec = ".", sep = ",")
str(gsg)
dim(gsg)
summary(gsg) 

#################### Cleaning of data ####################

gsg$HABITAT[gsg$HABITAT == "Mesic"] <- "MES"
gsg$HABITAT[gsg$HABITAT == "Wetland"] <- "WET"

gsg$SUPPL[gsg$SUPPL == "FOO"] <- "F"
gsg$SUPPL[gsg$SUPPL == "WAT"] <- "W"
gsg$SUPPL[gsg$SUPPL == "NONE"] <- "TEM"

# Formating the reference level
gsg$SUPPL <- relevel(gsg$SUPPL, "TEM")

# Delete WF level in SUPPL
gsg <- gsg[gsg$SUPPL != "WF",]
gsg <- droplevels(gsg)

# Formating variables
gsg$IniDate <- as.numeric(gsg$IniDate)
gsg$SupplDate <- as.numeric(gsg$SupplDate)
gsg$AN <- as.factor(gsg$AN)

# Nest ISSUE 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
gsg$Fate[gsg$ISSUE == 1] <- 0
gsg$Fate[gsg$ISSUE == 3] <- 1
gsg$Fate[gsg$ISSUE == 0|gsg$ISSUE == 2|gsg$ISSUE == 5] <- NA

gsg <- gsg[,-11]
gsg <- droplevels(gsg)
gsg$Fate <- as.factor(gsg$Fate)

# Subset depending on the year
gsg2015 <- subset(gsg, AN == 2015)
gsg2016 <- subset(gsg, AN == 2016)
gsg2017 <- subset(gsg, AN == 2017)

#################### Which year(s) tested ####################

# Here choose one specific year or not
geese <- gsg2017
summary(geese)

# Remove NAs
geese$SupplDate[is.na(geese$SupplDate)] <- 99999
geese <- na.omit(geese)

#Data exploration - Basic NS computation#
#--------------------------------------#
SNgeeseTEM <- dim(geese[geese$SUPPL == "TEM" & geese$Fate == "0",])[1] / dim(geese[geese$SUPPL == "TEM",])[1]

SNgeeseF <- dim(geese[geese$SUPPL == "F" & geese$Fate == "0",])[1] / dim(geese[geese$SUPPL == "F",])[1]

SNgeeseW <- dim(geese[geese$SUPPL == "W" & geese$Fate == "0",])[1] / dim(geese[geese$SUPPL == "W",])[1]

# Packages for data manipulation, plotting & presenting tables
library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')

library('ztable')     # format tables for reporting

#Creating new data frame
prop <- cbind(c(rep(2015, 3), rep(2016, 3), rep(2017, 3)), rep(c("TEM","W", "F"), 3), NA, NA)
colnames(prop) <- c("YEAR", "SUPPL", "n", "PROP")

SNgeeseTEM <- dim(geese[geese$SUPPL == "TEM" & geese$Fate == "0",])[1] / dim(geese[geese$SUPPL == "TEM",])[1]


#Creation of AgeFound variable#
#--------------------------------#
#WARNING ! It has to be done before the modification of the FirstFound variable
geese$AgeFound <- (geese$FirstFound - geese$IniDate) + 1 #...+1 cause age 0 is impossible
geese$FindNest <- geese$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
#Attention ici valeur change selon les annees
FF <- min(geese$FirstFound) #date minimum = 164
geese$FirstFound <- geese$FirstFound - (FF - 1) #ici 163 = 164 - 1 (pour Day 1)

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
# Write a function for evaluating a set of competing models
# Set of modeles to test on MARK_modeles.odt 

#run.geese=function()
#{
# 1. A model of constant daily survival rate (DSR)

run.geese=function()
{
  
# 0. A model of constant daily survival rate (DSR)
M0 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~1)))

# 00. year effect
M00 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN)), groups = "AN")

# 000. habitat effect
M000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT)), groups = "HABITAT")

# 0000. supplementation effect
M0000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL")

# 1. AN + SUPPL
M01 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL)), groups = c("AN", "SUPPL"))

# 2. AN + SUPPL + HABITAT
M02 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT)), groups = c("AN", "SUPPL", "HABITAT"))

# 3. AN + SUPPL + HABITAT + HABITAT*SUPPL
M03 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT + HABITAT*SUPPL)), groups = c("AN", "SUPPL", "HABITAT"))

# 4. AN + SUPPL + HABITAT + NestAge
M04 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT + NestAge)), groups = c("AN", "SUPPL", "HABITAT"))

# 5. AN + SUPPL + HABITAT + NestAge + HABITAT*SUPPL
M05 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT + NestAge + HABITAT*SUPPL)), groups = c("AN", "SUPPL", "HABITAT"))

# 8. AN + HABITAT
M08 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + HABITAT)), groups = c("AN", "HABITAT"))

# 9. AN + HABITAT + NestAge
M09 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + HABITAT + NestAge)), groups = c("AN", "HABITAT"))

# 11. AN + NestAge
M11 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + NestAge)), groups = "AN")

# 14. AN + SUPPL + NestAge
M14 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + NestAge)), groups = c("AN", "SUPPL"))

return(collect.models() )
}

# run defined models
geese.results <- run.geese()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
geese.results # print model-selection table to screen

#################### Best model for full database ####################

# First best model
# 5. AN + SUPPL + HABITAT + NestAge + HABITAT*SUPPL
M05 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT + NestAge + HABITAT*SUPPL)), groups = c("AN", "SUPPL", "HABITAT"))

# Second best model
# 4. AN + SUPPL + HABITAT + NestAge
M04 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN + SUPPL + HABITAT + NestAge)), groups = c("AN", "SUPPL", "HABITAT"))
