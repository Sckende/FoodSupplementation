####################DONNÉES 2015####################

require(RMark)
getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
geese<-read.table("MARK1-2015.txt", h=T, sep="\t")

summary(geese)
str(geese)
#class(geese$id)

# Treat dummy variables for habitat types as factors
geese$WET=factor(geese$WET)
geese$MES=factor(geese$MES)

# Examine a summary of the dataset
summary(geese)

#----------------------------------#
#####Snow geese - control 2015#####
#--------------------------------#
# Write a function for evaluating a set of competing models
#run.geese=function()
#{# 1. A model of constant daily survival rate (DSR)
Dot=mark(geese,nocc=38,model="Nest",model.parameters=list(S=list(formula=~1)))
# 2. DSR varies by habitat type - treats habitats as factors
# and the output provides S-hats for each habitat type
Hbt=mark(geese,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))
# 3. DSR varies with NestAge
AgeGeese=mark(geese,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
#4.DSR varie with NestAge and Habitat
AgeHbt=mark(geese,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET+MES)), groups=c("WET","MES"))
# Return model table and list of models
#
#  return(collect.models(table = T) )}

# The next line runs the 3 models above
geese.results=run.geese(model.)#création de 4 fichiers par modéles avec extensions .inp, .out, .vcv, .res

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#export.MARK(geese.results$Age$data,"GeeseDSR",geese.results,replace=TRUE,ind.covariates="all")
#Creates a .Rinp, .inp and optionally renamed output files that can be imported into MARK to
#create a new MARK project with all the data and output files.
geese.results # print model-selection table to screen - Bilan AICc
#exportation des r?sultats en format .txt
options(width=100) # set page width to 100 characters
sink("resultsgeese.table.txt") # capture screen output to file
print(geese.results) # send output to file
sink() # return output to screen
# remove "#" on next line to see output in notepad
#system("notepad resultsgeese.table.txt",invisible=FALSE,wait=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
#geese.results$Dot # print MARK output to designated text editor
geese.results$Dot$results$beta # view estimated beta for model in R
geese.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# geese.results$Hab # print MARK output to designated text editor
geese.results$Hbt$design.matrix # view the design matrix that was used
geese.results$Hbt$results$beta # view estimated beta for model in R
geese.results$Hbt$results$beta.vcv # view variance-covariance matrix for beta's
geese.results$Hbt$results$real # view the estimates of Daily Survival Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model plot #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# To obtain estimates of DSR for various values of 'NestAge'
# some work additional work is needed.
require(plotrix)
AgeGeese=mark(geese,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
AgeGeese#the used model to plot
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese,geese);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))
#----------AgeHbt plots---------#
AgeHbt
# Build design matrix with ages of interest
fc=find.covariates(AgeHbt,geese);fc
fc$value[1:74]=rep(1:37,2) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeHbt,fc);design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival=compute.real(AgeHbt,design=design)[1:74,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:74,2],Age.survival);Age.survival
HAB<-c(rep("WET",37),rep("MES",37))
Age.survival<-cbind(HAB,Age.survival)



colnames(Age.survival)=c("HAB","Age","DSR","seDSR","lclDSR","uclDSR")

Age.survival # view estimates of DSR for each age

#to calculate the NS for each habitat
#WETLAND
DSRwet<-Age.survival[Age.survival$HAB=="WET",]
NSwet<-prod(DSRwet$DSR[c(1:27)]);NSwet

#MESIC
DSRmes<-Age.survival[Age.survival$HAB=="MES",]
NSmes<-prod(DSRmes$DSR[c(1:27)]);NSmes
# Plot results
require(plotrix)
par(mfrow=c(2,1))
with(data=Age.survival,plot(Age[HAB=="MES"],DSR[HAB=="MES"],'l',ylim=c(0.91,1),
                            main = "Daily survival nest with age nest & mesic habitat- CONTROL",
                            xlab = "Age of nests",ylab = "DSR"))
grid()
axis.break(axis=2,breakpos=0.913,style='slash')
with(data=Age.survival,points(Age[HAB=="MES"],lclDSR[HAB=="MES"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="MES"],uclDSR[HAB=="MES"],'l',lty=3))

with(data=Age.survival,plot(Age[HAB=="WET"],DSR[HAB=="WET"],'l',ylim=c(0.975,1),
                            main = "Daily survival nest with age nest & wetland habitat- CONTROL",
                            xlab = "Age of nests", ylab="DSR"))
grid()
axis.break(axis=2,breakpos=0.9753,style='slash')
with(data=Age.survival,points(Age[HAB=="WET"],lclDSR[HAB=="WET"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="WET"],uclDSR[HAB=="WET"],'l',lty=3))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#to test the NestAge modele with the same spread of
#initiation date than the supplemented nests (155 to 166)

geese1<-subset(geese,geese$IniDate<=166)
summary(geese1)
#DSR varies with NestAge
AgeGeese1=mark(geese1,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))

#To plot results
AgeGeese1#the used model to plot
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese1,geese1);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese1,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese1,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
require(plotrix)
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#to test the NestAge modele with initiation dates which were estimated
#with accurateness (observed laying or observed hatch)

###here I obtain good graphics for the nesting success by NestAge and habitat
#but the value for the computation of the nesting success with DSR is not possible for the mesic tundra

require(RMark)
getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
geese<-read.table("MARK1-2015.txt", h=T, sep="\t")

summary(geese)
str(geese)
class(geese$id)

geese2<-subset(geese,!(geese$IniCode==2))
summary(geese2)

# Treat dummy variables for habitat types as factors
geese2$WET=factor(geese2$WET)
geese2$MES=factor(geese2$MES)

#DSR varies with NestAge
AgeGeese2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
#DSR varies with habitat type
Hbt2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))
#DSR varies with nest age & habitat type
AgeHab2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET+MES)),groups=c("WET","MES"))

#Results plot
AgeHab2
# Build design matrix with ages of interest
fc=find.covariates(AgeHab2,geese2);fc
fc$value[1:74]=rep(1:37,2) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeHab2,fc);design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival=compute.real(AgeHab2,design=design)[1:74,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:74,2],Age.survival);Age.survival
HAB<-c(rep("WET",37),rep("MES",37))
Age.survival<-cbind(HAB,Age.survival)



colnames(Age.survival)=c("HAB","Age","DSR","seDSR","lclDSR","uclDSR")

Age.survival # view estimates of DSR for each age

# Plot results
require(plotrix)
par(mfrow=c(2,1))
with(data=Age.survival,plot(Age[HAB=="MES"],DSR[HAB=="MES"],'l',ylim=c(0.78,1),
                            main = "Daily survival nest with age nest & mesic habitat- CONTROL",
                            xlab = "Age of nests",ylab = "DSR"))
grid()
axis.break(axis=2,breakpos=0.785,style='slash')
with(data=Age.survival,points(Age[HAB=="MES"],lclDSR[HAB=="MES"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="MES"],uclDSR[HAB=="MES"],'l',lty=3))

with(data=Age.survival,plot(Age[HAB=="WET"],DSR[HAB=="WET"],'l',ylim=c(0.98,1),
                            main = "Daily survival nest with age nest & wetland habitat- CONTROL",
                            xlab = "Age of nests", ylab="DSR"))
grid()
axis.break(axis=2,breakpos=0.9796,style='slash')
with(data=Age.survival,points(Age[HAB=="WET"],lclDSR[HAB=="WET"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="WET"],uclDSR[HAB=="WET"],'l',lty=3))

###Other graphic
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese2,geese2);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese2,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese2,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))
#temporal variation of the DSR is good with this subset of datas
#test of the other modeles with GEESE 2 datas#
# Treat dummy variables for habitat types as factors
geese2$WET=factor(geese2$WET)
geese2$MES=factor(geese2$MES)
#A model of constant daily survival rate (DSR)
Dot2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~1)))
# 2. DSR varies by habitat type - treats habitats as factors
# and the output provides S-hats for each habitat type
Hbt2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))

Dot2$results$beta # view estimated beta for model in R
Dot2$results$real # view estimated DSR estimate in R

Hbt2$results$beta # view estimated beta for model in R
Hbt2$results$real # view estimated DSR estimate in R
#recuperation des valeurs de DSR
DSRHbt2<-compute.real(model = Hbt2, data=geese2)
DSRHbt2$NS<-DSRHbt2$estimate^27;DSRHbt2#NS for nesting success=DSR^27


####################DONNÉES 2016####################
getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
#Pour faire tourber ce script, le tableau de base doit contenir 9 variables - id, FirstFound (en JJ), LastPresent (en JJ), LastChecked (en JJ), Fate (= issue des nids), Freq (=1), hab (Wetland, Mesic ou NA), IniDate (en JJ), Inicode (= 1 à 5)


go<-read.table("MARK1-2016.txt",sep="\t" ,  h=T)
summary(go)

#Idéalement retrait des dates d'initiation obtenues avec incertitude, soit garder INICODE == 1 ou 3
#Mais si j'enlève INICODE == 2, beaucoup d'effectif en moins et plus que 4 nids en milieux mésique
go<-go[(go$IniCode==1 | go$IniCode==2| go$IniCode==3),]

#Traitement de la variable Fate (0 = succès, tout le reste = échec soit 1) ATTENTION ! Avec Rmark 0 = succès et 1 = échec
go$Fate[go$Fate!=1]<-0#obtention d'une variable binomiale
#mais les 0 et 1 doivent être inversés car 0 = succès pour Rmark et 1 = échec
FATEbis<-NULL
x <- go$Fate
for(i in x){
  if (i == 1) {
    r<-0
  } else
    r<-1
  FATEbis<-rbind(FATEbis,r)	}
go$Fate<-FATEbis
#go<-cbind(go, FATEbis)


#Traitement des données et création des variables nécessaires pour les analyses
go$IniDate<-as.numeric(as.character(go$IniDate))

#Création des variables "milieu" - WET/MES#
#-----------------------------------------#
#boucle pour la variable WET
WET<-NULL
x <- go$hab
for(i in x){
  if (i == "Wetland") {
    r<-1
  } else if (i == "Mesic") {
    r<-0
  } else
    r<-NA
  WET<-rbind(WET,r)	}

go<-cbind(go, WET)

#-----------------------------------------#
#boucle pour la variable MES
MES<-NULL
x <- go$hab
for(i in x){
  if (i == "Wetland") {
    r<-0
  } else if (i == "Mesic") {
    r<-1
  } else
    r<-NA
  MES<-rbind(MES,r)	}

go<-cbind(go, MES)
#-----------------------------------------#
go$WET<-factor(go$WET)
go$MES<-factor(go$MES)

#enlever les données manquantes
#g<-na.omit(g)

#Création de la variable AgeFound#
#--------------------------------#
#attention, ceci doit être fait avant la modification de la variable FirstFound
go$AgeFound<-(go$FirstFound-go$IniDate)+1 #...+1 car l'âge 0 est impossible
go$FindNest<-go$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
min(go$FirstFound) #date minimum = 164
go$FirstFound<-go$FirstFound-160 #ici 160 = 161 - 1 (pour Day 1)

#Idem pour les variables LastPresent, LastChecked#
#-------------------------------------------------#
go$LastPresent<-go$LastPresent-160
go$LastChecked<-go$LastChecked-160

#Obtention de la variable AgeDay1#
#--------------------------------#
go$AgeDay1<-(go$AgeFound-go$FirstFound)+1
#Retrait de valeur extrême de LastChecked = 54
go[go$LastChecked>39,]
go$LastChecked>39
#go<-go[-(go$LastChecked>39),]# retire une ligne mais pas la bonne ---> INCOMPRÉHENSIBLE !!!!
go<-go[-(235),]
summary(go)
#----------------------------------#
#####Snow geese - control 2016#####
#--------------------------------#

# Write a function for evaluating a set of competing models
#run.go=function()
#{# 1. A model of constant daily survival rate (DSR)
Dot=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~1)))
# 2. DSR varies by habitat type - treats habitats as factors
# and the output provides S-hats for each habitat type
Hbt=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))
# 3. DSR varies with NestAge
AgeGeese=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
#4.DSR varie with NestAge and Habitat
AgeHbt=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET+MES)), groups=c("WET","MES"))
#nocc corrspond au nombre de jours où le suivi a duré soit max(LastChecked)
# Return model table and list of models
#
#return(collect.models(table = T) )}

# The next line runs the 3 models above
#go.results=run.go()#création de 4 fichiers par modéles avec extensions .inp, .out, .vcv, .res

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#export.MARK(geese.results$Age$data,"GeeseDSR",geese.results,replace=TRUE,ind.covariates="all")
#Creates a .Rinp, .inp and optionally renamed output files that can be imported into MARK to
#create a new MARK project with all the data and output files.
go.results # print model-selection table to screen - Bilan AICc
#exportation des r?sultats en format .txt
options(width=100) # set page width to 100 characters
sink("resultsgo.table.txt") # capture screen output to file
print(go.results) # send output to file
sink() # return output to screen
# remove "#" on next line to see output in notepad
#system("notepad resultsgeese.table.txt",invisible=FALSE,wait=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
#geese.results$Dot # print MARK output to designated text editor
go.results$Dot$results$beta # view estimated beta for model in R
go.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# geese.results$Hab # print MARK output to designated text editor
go.results$Hbt$design.matrix # view the design matrix that was used
go.results$Hbt$results$beta # view estimated beta for model in R
go.results$Hbt$results$beta.vcv # view variance-covariance matrix for beta's
go.results$Hbt$results$real # view the estimates of Daily Survival Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model plot #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# To obtain estimates of DSR for various values of 'NestAge'
# some work additional work is needed.
require(RMark)
require(plotrix)

AgeGeese=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
AgeGeese#the used model to plot
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese,go);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.92,1),main = "Daily survival nest with age nest - CONTROL 2016"))
grid()
axis.break(axis=2,breakpos=0.9195,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))

Hbt=mark(go,nocc=39,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))

##### BROUILLON #####
