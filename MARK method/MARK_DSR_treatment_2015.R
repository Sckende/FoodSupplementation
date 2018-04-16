####################DONNÉES 2015####################
#####Snow Geese - EXPERIMENTATION - MARK2#####
#fichier d'analyse de Rmark pour le calcul et l'étude du taux de survie journalier des nids supplémentés#
#attention pour SM2J prédation observée le jour même=>FirstFound=LastPresent et LastChecked=LastPresent+1#
#pour que la ligne soit valide#
#require(RMark)

getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
supp<-read.table("MARK_suppl_2015.txt", h = T, dec = ".", sep = ",")
str(supp)
dim(supp)
summary(supp)

# Treat dummy variables for experimentation types as factors
#supp$FOOD=factor(supp$FOOD)
#supp$WATER=factor(supp$WATER)
#supp$FOWA=factor(supp$FOWA)
#supp$WET=factor(supp$WET)
#supp$MES=factor(supp$MES)

#summary(subset(supp,FOOD=="1"& MES=="1"))#pour obtenir les effectifs dans les differents traitements et milieux


#summary(supp)
#run.expe=function ()
#{# 1. A model of constant daily survival rate (DSR)
Dot <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~1)))

# 2. DSR varies by experimentation type - treats experimentation as factors and the output provides S-expe for each experimentation type
EXPE <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula= ~SUPPL)), groups = "SUPPL")

# 3. DSR varies with habitat type
Habitat <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT)), groups = "HABITAT")

# 4.
EXPEHAB <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HABITAT)), groups = c("SUPPL", "HABITAT"))

# 5.
Age <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)))
# 6.
AgeHab <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ HABITAT + NestAge)), groups = "HABITAT")

# 7.
AgeSuppl <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + NestAge)), groups = "SUPPL")

# 8.
AgeSupplHab <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HABITAT + NestAge)), groups = c("SUPPL", "HABITAT"))

# 9.
ExperimentDate <- mark(supp, nocc = 27, model = "Nest", model.parameters = list(S = list(formula = ~ExpDate)))

#           }

#expe.results=run.expe()
#expe.results


#####Snow Geese - EXPERIMENTATION - MARK3#####
#idem Fichier Rmark 2 mais sans les nids avec date d'initiation aberrante pour utiliser l'analyse "AgeDay1" (SH36J, SM3J, SM11J, SM13J, SH16R)#
require(RMark)

getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
supp3<-read.table("MARK3-2015.txt", h=T, dec=",", sep="\t")
str(supp3)
dim(supp3)
summary(supp3)
table(supp3$Exp)
# Treat dummy variables for experimentation types as factors
supp3$FOOD=factor(supp3$FOOD)
supp3$WATER=factor(supp3$WATER)
supp3$FOWA=factor(supp3$FOWA)
supp3$WET=factor(supp3$WET)
supp3$MES=factor(supp3$MES)
supp3$Fate <- as.factor(supp3$Fate)
#supp3$ExpDate=factor(supp3$ExpDate)
supp3$AgeFound<-(supp3$FirstFound-supp3$IniDate)+1
supp3$AgeDay1<-(supp3$AgeFound-supp3$FirstFound)+1
#summary(subset(supp,FOOD=="1"& MES=="1"))#pour obtenir les effectifs dans les differents traitements et milieux


summary(supp3)
#run.expe=function ()
#  {# 1. A model of constant daily survival rate (DSR)
Dot=mark(supp3,nocc=27,model="Nest",model.parameters=list(S=list(formula=~1)), delete = T)
# 2. DSR varies by experimentation type - treats experimentation as factors
# and the output provides S-expe for each experimentation type
EXPE=mark(supp3,nocc=27,model="Nest", model.parameters=list(S=list(formula=~FOOD+WATER+FOWA)),groups=c("FOOD","WATER","FOWA"), delete = T)
# 3. DSR varies with habitat type
Habitat=mark(supp3,nocc=27,model="Nest",model.parameters=list(S=list(formula=~MES+WET)),groups=c("MES","WET"), delete = T)
EXPEHAB=mark(supp3,nocc=27, model="Nest",model.parameters = list(S=list(formula=~FOOD+WATER+FOWA+MES+WET)),groups=c("FOOD","WATER","FOWA","MES","WET"), delete = T)
Age=mark(supp3,nocc=27,model="Nest",model.parameters=list(S=list(formula=~NestAge)), delete = T)
Predation=mark(supp3,nocc=27,model="Nest",model.parameters=list(S=list(formula=~ParPred)), delete = T)
ExperimentDate=mark(supp3,nocc=27,model = "Nest", model.parameters = list(S=list(formula=~ExpDate)), delete = T)
#}

#expe.results=run.expe()
#expe.results

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Habitat$design.matrix # view the design matrix that was used
Habitat$results$beta # view estimated beta for model in R
Habitat$results$beta.vcv # view variance-covariance matrix for beta's
Habitat$results$real


#####Plot of DSR vs age nest#####
# To obtain estimates of DSR for various values of 'NestAge'
# some work additional work is needed.

Age#the used model to plot
# Build design matrix with ages of interest
fc=find.covariates(Age,supp3);fc
fc$value[1:26]=1:26 # assign 1:26 to 1st 26nest ages
design=fill.covariates(Age,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(Age,design=design)[1:26,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:26,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.88,1),main = "Daily survival nest with age nest - Supplemented nests"))
grid()
require(plotrix)
axis.break(axis=2,breakpos=0.877,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))



#####Test for DSR vs experimentation date - MARK4#####
#use the NestAge name variable with experimentation date as values to see if effect of experimentation date
#on the DSR

require(RMark)

getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
supp4<-read.table("Mark4-2015.txt", h=T, dec=",", sep="\t")
str(supp4)
dim(supp4)
summary(supp4)

ExperimentDate=mark(supp4,nocc=27,model = "Nest", model.parameters = list(S=list(formula=~NestAge)))

ExperimentDate#the used model to plot
# Build design matrix with ages of interest
fc4=find.covariates(ExperimentDate,supp4);fc4
fc4$value[1:26]=1:26 # assign 1:26 to 1st 26nest ages
design4=fill.covariates(ExperimentDate,fc4);design4 # fill design matrix with values

# extract 1st 26 rows of output
Age.survival4=compute.real(ExperimentDate,design=design4)[1:26,];Age.survival4

# insert covariate columns
Age.survival4=cbind(design4[1:26,2],Age.survival4);Age.survival4
colnames(Age.survival4)=c("ExperimentDate","DSR","seDSR","lclDSR","uclDSR")
Age.survival4 # view estimates of DSR for each age
# Plot results
with(data=Age.survival4,plot(ExperimentDate,DSR,'l',ylim=c(0.88,1),main = "Daily survival nest with experimentation date - Supplemented nests"))
grid()
require(plotrix)
axis.break(axis=2,breakpos=0.877,style='slash')
with(data=Age.survival4,points(ExperimentDate,lclDSR,'l',lty=3))
with(data=Age.survival4,points(ExperimentDate,uclDSR,'l',lty=3))







