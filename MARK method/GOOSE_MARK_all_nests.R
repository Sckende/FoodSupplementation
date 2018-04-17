getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
gsg <- read.table("GOOSE_MARK_all_nests.csv", h = T, dec = ".", sep = ",")
str(gsg)
dim(gsg)
summary(gsg) 

gsg$HABITAT[gsg$HABITAT == "Mesic"] <- "MES"
gsg$HABITAT[gsg$HABITAT == "Wetland"] <- "WET"

gsg$SUPPL[gsg$SUPPL == "FOO"] <- "F"
gsg$SUPPL[gsg$SUPPL == "WAT"] <- "W"
gsg$SUPPL[gsg$SUPPL == "NONE"] <- "TEM"

gsg$IniDate <- as.numeric(gsg$IniDate)
gsg$SupplDate <- as.numeric(gsg$SupplDate)

# Nest ISSUE 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
gsg$Fate[gsg$ISSUE == 1] <- 0
gsg$Fate[gsg$ISSUE == 3] <- 1
gsg$Fate[gsg$ISSUE == 0|gsg$ISSUE == 2|gsg$ISSUE == 5] <- NA

gsg <- gsg[,-11]
gsg <- droplevels(gsg)
gsg$Fate <- as.factor(gsg$Fate)

#################### choix de l'annee a tester####################
gsg2015 <- subset(gsg, AN == 2015)
gsg2016 <- subset(gsg, AN == 2016)
gsg2017 <- subset(gsg, AN == 2017)

# Ici utiliser l'annee souhaitee
geese <- gsg2016
summary(geese)

#enlever les données manquantes
geese$SupplDate[is.na(geese$SupplDate)] <- 99999
geese <- na.omit(geese)

#Création de la variable AgeFound#
#--------------------------------#
#attention, ceci doit être fait avant la modification de la variable FirstFound
geese$AgeFound <- (geese$FirstFound - geese$IniDate) + 1 #...+1 car l'âge 0 est impossible
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
#run.geese=function()
#{# 1. A model of constant daily survival rate (DSR)
Dot <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~1)))

# 2. DSR varies by habitat type - treats habitats as factors and the output provides S-hats for each habitat type
Hbt <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT)), groups = "HABITAT") 

# 3. DSR varies with NestAge
AgeGeese <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)))

#4.DSR varie with NestAge and Habitat
AgeHbt <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HABITAT)), groups = "HABITAT")

# 5. DSR follows a trend through time
TimeTrend <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ Time))) 

# 6. DSR varies with treatments
Expe <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL")

# 7. DSR varies with treatments and habitats
ExpeHab <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HABITAT)), groups = c("SUPPL", "HABITAT"))

# 8. DSR varies with treatments and NestAge
AgeExpe <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + SUPPL)), groups = "SUPPL")

# 9. DSR varies with treatments, NestAge and habitats
AgeExpeHab <- mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + SUPPL + HABITAT)), groups = c("SUPPL", "HABITAT"))
