setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
#Pour faire tourber ce script, le tableau de base doit contenir les données originelles issues de la rentrée de données suite aux expérimentations de supplémentations en eau et en nourriture des nids de la grande oie des neiges - Cf protocole d'extraction de données : "GOOSE-extracBDD.pdf"

#s<-read.table("2015_suppl_MARK.txt",sep = "\t", dec = ",", h = T)
s<-read.table("2015_suppl_MARK.txt", sep = "\t", dec = ",", h = T)
s<-read.table("2016_suppl_MARK.txt", sep = "\t", dec = ",", h = T)
s<-read.table("2017_suppl_MARK.txt", sep = "\t", dec = ",", h = T)

s$Fate<-as.factor(s$Fate)
summary(s)

########-------------Variables qualitatives creation-----------------#############
#boucle pour la variable WET
#WET<-NULL
#x <- s$HABITAT
#for(i in x){
#  if (i == "WET") {
#    r<-1
#  } else
#    r<-0
#  WET<-rbind(WET,r)	}

#s<-cbind(s, WET)

#-----------------------------------------#
#boucle pour la variable MES
#MES<-NULL
#x <- s$HABITAT
#for(i in x){
#  if (i == "WET") {
#    r<-0
#  } else
#    r<-1
#  MES<-rbind(MES,r)	}

#s<-cbind(s, MES)
#-----------------------------------------#
#s$WET=as.factor(s$WET)
#s$MES=as.factor(s$MES)

#boucle pour les variables de supplémentation : WATER
#W<-NULL
#x <- s$SUPPL
#for(i in x){
#  if (i == "W") {
#    r<-1
#  }  else
#    r<-0
#  W<-rbind(W,r)	}

#s<-cbind(s, W)

#boucle pour les variables de supplémentation : FOOD
#Fo<-NULL
#x <- s$SUPPL
#for(i in x){
#  if (i == "F") {
#    r<-1
#  }  else
#    r<-0
#  Fo<-rbind(Fo,r)	}

#s<-cbind(s, Fo)

#boucle pour les variables de supplémentation : WATER+FOOD
#To use only for the 2015 data
#WF<-NULL
#x <- s$SUPPL
#for(i in x){
#  if (i == "WF") {
#    r<-1
#  }  else
#    r<-0
#  WF<-rbind(WF,r)	}

#s<-cbind(s, WF)

#s$W=as.factor(s$W)
#s$Fo=as.factor(s$Fo)
#s$WF=as.factor(s$WF)

#### Retrait des nids à echec avant supplémentation pour 2017 ####
s <- s[s$SUPPL != "NONE",]
s <- droplevels(s)
# Retrait des NAs dans supplémentation 2017
s <- na.omit(s)

#### Création des variables pour analyses ####
#Création de la variable AgeFound#
#--------------------------------#
#attention, ceci doit être fait avant la modification de la variable FirstFound
s$AgeFound<-(s$FirstFound-s$IniDate)+1 #...+1 car l'âge 0 est impossible
s$FindNest<-s$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
#Attention ici valeur à changer selon les cas
FF<-min(s$FirstFound) #date minimum = 164
s$FirstFound<-s$FirstFound-(FF-1) #ici 163 = 164 - 1 (pour Day 1)

#Idem pour les variables LastPresent, LastChecked#
#-------------------------------------------------#
s$LastPresent<-s$LastPresent-(FF-1)
s$LastChecked<-s$LastChecked-(FF-1)

#Obtention de la variable AgeDay1#
#--------------------------------#
#correspond à l'âge du nid lors du premier jour du suivi de nids
s$AgeDay1<-(s$AgeFound-s$FirstFound)+1

#### Modeles ####
require(RMark)

#Attention ici la valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc<-max(s$LastChecked)

#### Null modele ####
Dot=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~1)), delete = T)

#### Age-dependent modele ####
AgeGeese=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge)), delete = T)

#### Habitat-dependent modele ####
Hbt=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~HABITAT)), groups="HABITAT", delete = T)

#### Treatment-dependent modele ####
Suppl=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~SUPPL)), groups="SUPPL", delete = T)

#### Age/Habitat dependent modele ####
AgeHbt=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge+HABITAT)), groups="HABITAT", delete = T)

#### Age/Treatment-dependent modele ####
AgeSuppl=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge+SUPPL)), groups="SUPPL", output = T, delete = T)

#### Age/Habitat/Treatment-dependent modele ####
AgeHbtSuppl=mark(s,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge+HABITAT+SUPPL)), groups = c("HABITAT", "SUPPL"), output = T, delete = T)
#####------------------Plot of results--------------------#####

#### AgeNest ####
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese,s);fc
fc$value[1:nrow(fc)]=1:nrow(fc) # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese,design=design)[1:nrow(fc),];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:nrow(fc),2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(min(Age.survival$lclDSR),1),main = paste(c("Daily survival of supplemented nests with age nest - ",unique(s$AN)))))
grid()
require(plotrix)
axis.break(axis=2,breakpos=min(Age.survival$lclDSR)+0.005,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))

#### AgeHbt ####
# Build design matrix with ages of interest
fc=find.covariates(AgeHbt,s);fc
fc$value[1:nrow(fc)]=rep(1:(nrow(fc)/2),2) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeHbt,fc);design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival=compute.real(AgeHbt,design=design)[1:nrow(fc),];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:nrow(fc),2],Age.survival);Age.survival
HAB<-c(rep("WET",(nrow(fc)/2)),rep("MES",(nrow(fc)/2)))
Age.survival<-cbind(HAB,Age.survival)

colnames(Age.survival)=c("HAB","Age","DSR","seDSR","lclDSR","uclDSR")

Age.survival<-Age.survival[,-7];Age.survival # view estimates of DSR for each age
require(plotrix) #for the command axis.break()

with(data=Age.survival,plot(Age[HAB=="MES"],DSR[HAB=="MES"],'l',ylim=c(min(Age.survival$lclDSR[Age.survival$HAB=="MES"]),1),
  main =paste(c("Daily survival of supplemented nests with age nest & mesic habitat - ", unique(s$AN))),
  xlab = "Age of nests",ylab=paste(c("DSR",unique(s$AN)))))
grid()
axis.break(axis=2,breakpos=(min(Age.survival$lclDSR[Age.survival$HAB=="MES"])+0.005),style='slash')
with(data=Age.survival,points(Age[HAB=="MES"],lclDSR[HAB=="MES"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="MES"],uclDSR[HAB=="MES"],'l',lty=3))

with(data=Age.survival,plot(Age[HAB=="WET"],DSR[HAB=="WET"],'l',ylim=c(min(Age.survival$lclDSR[Age.survival$HAB=="MES"]),1),
  main = paste(c("Daily survival of supplementeds nests with age nest & wetland habitat - ", unique(s$AN))),
  xlab = "Age of nests", ylab=paste(c("DSR",unique(s$AN)))))
grid()
axis.break(axis=2,breakpos=(min(Age.survival$lclDSR[Age.survival$HAB=="MES"])+0.005),style='slash')
with(data=Age.survival,points(Age[HAB=="WET"],lclDSR[HAB=="WET"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="WET"],uclDSR[HAB=="WET"],'l',lty=3))

#to calculate the NS for each habitat
#WETLAND
DSRwet<-Age.survival[Age.survival$HAB=="WET",]
NSwet<-prod(DSRwet$DSR);NSwet
#MESIC
DSRmes<-Age.survival[Age.survival$HAB=="MES",]
NSmes<-prod(DSRmes$DSR);NSmes

#### AgeSuppl ####
# Build design matrix with ages of interest
fc=find.covariates(AgeSuppl,s);fc
fc$value[1:nrow(fc)]=rep(1:(nrow(fc)/length(levels(s$SUPPL))),length(levels(s$SUPPL))) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeSuppl,fc);design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival=compute.real(AgeSuppl,design=design)[1:nrow(fc),];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:nrow(fc),length(levels(s$SUPPL))],Age.survival);Age.survival
SUPPL<-c(rep("F",(nrow(fc)/2)),rep("W",(nrow(fc)/2)))
####### !!!!!!!!!!!!!!!!!!!!!! ############
SUPPL<-c(rep("F",(nrow(fc)/length(levels(s$SUPPL)))),rep("W",(nrow(fc)/length(levels(s$SUPPL))),rep("WF",(nrow(fc)/length(levels(s$SUPPL))))))

Age.survival<-cbind(SUPPL,Age.survival)
colnames(Age.survival)=c("SUPPL","Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival<-Age.survival[,-7];Age.survival # view estimates of DSR for each age

# Food treatment
with(data=Age.survival,plot(Age[SUPPL=="F"],DSR[SUPPL=="F"],'l',ylim=c(min(Age.survival$lclDSR[Age.survival$SUPPL=="F"]),1),
  main =paste(c("Daily survival of supplemented nests with age nest & food treatment - ", unique(s$AN))),
  xlab = "Age of nests",ylab=paste(c("DSR",unique(s$AN)))))
grid()
require(plotrix) #for the command axis.break()
axis.break(axis=2,breakpos=(min(Age.survival$lclDSR[Age.survival$SUPPL=="F"])+0.005),style='slash')
with(data=Age.survival,points(Age[SUPPL=="F"],lclDSR[SUPPL=="F"],'l',lty=3))
with(data=Age.survival,points(Age[SUPPL=="F"],uclDSR[SUPPL=="F"],'l',lty=3))

# Water treatment
with(data=Age.survival,plot(Age[SUPPL=="W"],DSR[SUPPL=="W"],'l',ylim=c(min(Age.survival$lclDSR[Age.survival$SUPPL=="F"]),1),
  main = paste(c("Daily survival of supplementeds nests with age nest & water treatment - ", unique(s$AN))),
  xlab = "Age of nests", ylab=paste(c("DSR",unique(s$AN)))))
grid()
axis.break(axis=2,breakpos=(min(Age.survival$lclDSR[Age.survival$SUPPL=="F"])+0.005),style='slash')
with(data=Age.survival,points(Age[SUPPL=="W"],lclDSR[SUPPL=="W"],'l',lty=3))
with(data=Age.survival,points(Age[SUPPL=="W"],uclDSR[SUPPL=="W"],'l',lty=3))
