getwd()
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())


gsg <- read.table("GOOSE_MARK_all_nests.txt", h = T, dec = ".", sep = "\t")
str(gsg)
dim(gsg)
summary(gsg) 

#################### Cleaning of data ####################

gsg$HABITAT[gsg$HABITAT == "Mesic"] <- "MES"
gsg$HABITAT[gsg$HABITAT == "Wetland"] <- "WET"

gsg$SUPPL[gsg$SUPPL == "FOO"] <- "F"
gsg$SUPPL[gsg$SUPPL == "WAT"] <- "W"

# Formating the reference level
gsg$SUPPL <- relevel(gsg$SUPPL, "TEM")

# Delete WF level in SUPPL
gsg <- gsg[gsg$SUPPL != "WF",]
gsg <- gsg[gsg$SUPPL != "NONE",]

# Formating variables
gsg$AN <- as.factor(gsg$AN)

# Nest ISSUE 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
gsg$Fate[gsg$Groupe == "COLONY" & gsg$ISSUE == 1] <- 0
gsg$Fate[gsg$Groupe == "COLONY" & gsg$ISSUE == 3] <- 1

# Delete the ISSUE variable
gsg <- gsg[,-11]

gsg$Fate <- as.factor(gsg$Fate)
gsg <- droplevels(gsg)

# Remove NAs
gsg$SupplDate[is.na(gsg$SupplDate)] <- 99999
gsg <- na.omit(gsg)
# WARNING ! Here we deleted some failed nests ==> underestimation of failed nests number

####Data exploration - Basic NS computation#####
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
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] ,
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1],
  
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1],
  
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1],
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1]
)
PP <- c(
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2015" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2016" & gsg$SUPPL == "F",])[1],
  
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "TEM" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "TEM",])[1],
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "W" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "W",])[1],
  dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "F" & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == "2017" & gsg$SUPPL == "F",])[1]
)

prop$n <- nn
prop$PROP <- PP
prop$error_type <- sqrt(prop$PROP*(1-prop$PROP)/prop$n)

head(prop)
summary(prop)

# Graphic
color <- c("olivedrab3", "aquamarine3", "darkgoldenrod2")

barCenters <- barplot(prop$PROP, width = 0.5, col = color, xlab = "Year", ylab = "Nesting success", ylim = c(0, 1), names.arg = c("",2015, "","",2016, "","",2017, ""), main = "Goose nesting success  depending on year and treatments", legend.text = TRUE, space = c(0.2,0,0,0.2,0,0,0.2,0,0))

legend("topleft", inset = c(0, -0,05),
       legend = c("TEMOIN", "WATER", "FOOD"), 
       fill = color, bty = "n")
segments(barCenters, prop$PROP - prop$error_type, barCenters, prop$PROP + prop$error_type, lwd = 1.5)
text(barCenters,0.2, labels = paste("(", as.character(prop$n), ")", sep = ""))


#Creating new data frame - SN by year, by habitat and by treatments
prop2 <- NULL
for (i in unique(gsg$AN)){
  for (j in c("TEM", "W", "F")) {
    for (k in c("MES", "WET")){
      YEAR <- i
      TREAT <- j
      HAB <- k
      N <- dim(gsg[gsg$AN == i & gsg$SUPPL == j & gsg$HABITAT == k & gsg$Fate == "0",])[1]
      PROP <- dim(gsg[gsg$AN == i & gsg$SUPPL == j & gsg$HABITAT == k & gsg$Fate == "0",])[1] / dim(gsg[gsg$AN == i & gsg$SUPPL == j & gsg$HABITAT == k,])[1]
      error_type <- sqrt(PROP*(1-PROP)/N)
      
      r <- data.frame(YEAR, HAB, TREAT, N, PROP, error_type)
      
      prop2 <- rbind(prop2, r)
    }    
  }
}
#View(prop2)
# Graphic
color <- c("olivedrab3", "olivedrab4", "aquamarine3", "aquamarine4", "darkgoldenrod2", "darkgoldenrod3")

barCenters <- barplot(prop2$PROP, col = color, xlab = "", ylab = "Nesting success", ylim = c(0, 1.2), names.arg = paste(prop2$HAB, " (" , prop2$N, ")", sep = ""), main = "Goose nesting success  depending on year, habitat and treatment", legend.text = TRUE, space = c(0.2, rep(c(0,0.1,0,0.1,0,0.4), 2) , c(0,0.1,0,0.1,0)), las = 2)

legend("topright", 
       #inset = c(0, -0,05),
       legend = c("TEMOIN", "WATER", "FOOD"), 
       fill = c("olivedrab3", "aquamarine3", "darkgoldenrod2"), bty = "n")
segments(barCenters, prop2$PROP - prop2$error_type, barCenters, prop2$PROP + prop2$error_type, lwd = 1.5)


#### Data Analyses ####

# Subset depending on the year
gsg2015 <- subset(gsg, AN == 2015)
gsg2016 <- subset(gsg, AN == 2016)
gsg2017 <- subset(gsg, AN == 2017)

# Here choose one specific year or not
geese <- gsg
summary(geese)
dim(geese)


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

# 00000. NestAge effect
M00000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)))

# 000000. habitat and NestAge interaction effect
M000000 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT*NestAge)), groups = "HABITAT")

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

# 15. AN * SUPPL
M15 <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ AN * SUPPL)), groups = c("AN", "SUPPL"))

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
