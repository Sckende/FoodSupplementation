####################DONNÉES 2015####################
##### Snow Geese - EXPERIMENTATION - MARK_SUPPL_2016 #####
#fichier d'analyse de Rmark pour le calcul et l'étude du taux de survie journalier des nids supplémentés#
#attention pour SM2J prédation observée le jour même=>FirstFound=LastPresent et LastChecked=LastPresent+1#
#pour que la ligne soit valide#

require(RMark)

getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

sup2016<-read.table("MARK_suppl_2016.txt", h=T, dec=".", sep=",")

str(sup2016)
dim(sup2016)
summary(sup2016)

## CORRECTIONS A APPORTER CAR AgeDay1 = -2

#Attention ici la valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc <- max(sup2016$LastChecked)

#summary(supp)
#run.expe=function ()
#{
# 1. A model of constant daily survival rate (DSR)
Dot <- mark(sup2016, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~1)))

# 2. DSR varies by experimentation type - treats experimentation as factors and the output provides S-expe for each experimentation type
EXPE <- mark(supp, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~SUPPL)),groups = "SUPPL")

# 3. DSR varies with habitat type
Habitat <- mark(sup2016, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT)), groups = "HABITAT")

#
EXPEHAB <- mark(sup2016, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HABITAT)), groups = c("SUPPL","HABITAT"))

#
Age <- mark(sup2016, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~NestAge)))

#           }

#expe.results=run.expe()
#expe.results