
####################DONNÉES 2016####################
getwd()
setwd("/home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
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
