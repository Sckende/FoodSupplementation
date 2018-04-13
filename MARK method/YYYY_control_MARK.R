setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
#Pour faire tourner ce script, le tableau de base doit contenir les données originelles issues de la base de données sur le suivi de nidification de la grande oie des neiges - Cf protocole d'extraction de données : "GOOSE-extrac_BDD.pdf"


#g<-read.table("MARK1-2015-bis.txt",sep="\t" ,  h=T)
g<-read.table("2015_control_MARK.txt",sep="\t" ,  h=T)
g<-read.table("2016_control_MARK.txt",sep="\t" ,  h=T)
g<-read.table("2017_control_MARK.txt",sep="\t" ,  h=T)
summary(g)
colnames(g)<-c("AN","id","hab","IniDate","ECLO","EXPO","Fate","PRED","IniCode","FirstFound","LastPresent","LastChecked")

#Idéalement retrait des dates d'initiation obtenues avec incertitude, soit garder INICODE == 1 ou 3
#Mais si j'enlève INICODE == 2, beaucoup d'effectif en moins et plus que 4 nids en milieux mésique
#g<-g[(g$IniCode==1 | g$IniCode==2| g$IniCode==3),]

#Traitement de la variable Fate : 0= exclus, 1= succès, 2= abandon, 3= détruit, 5= inconnu  ATTENTION ! Avec Rmark 0 = succès et 1 = échec
table(g$Fate)


#obtention d'une variable binomiale
#mais les 0 et 1 doivent être inversés car 0 = succès pour Rmark et 1 = échec

for (i in 1:nrow(g)) {
  if (g$Fate[i] == 1) {
    g$Fate[i] <- 0
  } else {
    g$Fate[i] <- 1
  }
}


#Traitement des données et création des variables nécessaires pour les analyses

#Remplacement des "N/A" par NA pour la variable ECLO
for (i in 1:nrow(g)) {
  if (g$ECLO[i] == "N/A") {
    g$ECLO[i] <- NA
  }
}
g$ECLO<-as.numeric(as.character(g$ECLO))

#Création des variables "milieu" - WET/MES#
#-----------------------------------------#
#boucle pour la variable WET

#for ( i in 1:nrow(g)) {
#  if (g$hab[i] == "Mesic") {
#    g$WET[i] <- 0 
#    g$MES[i] <- 1
#  } else {
#    g$WET[i] <- 1 
#    g$MES[i] <- 0    
#  }
#}

#-----------------------------------------#
#g$WET <- factor(g$WET)
#g$MES <- factor(g$MES)
g$Fate <- factor(g$Fate)
summary(g)
#enlever les données manquantes
#g<-na.omit(g)

#Création de la variable AgeFound#
#--------------------------------#
#attention, ceci doit être fait avant la modification de la variable FirstFound
g$AgeFound <- (g$FirstFound-g$IniDate)+1 #...+1 car l'âge 0 est impossible
g$FindNest <- g$FirstFound

#Modification des dates de la variables FirstFound (la date minimale = Jour 1)#
#-----------------------------------------------------------------------------#
#Attention ici valeur change selon les annees
FF<-min(g$FirstFound) #date minimum = 164
g$FirstFound<-g$FirstFound-(FF-1) #ici 163 = 164 - 1 (pour Day 1)

#Idem pour les variables LastPresent, LastChecked#
#-------------------------------------------------#
g$LastPresent<-g$LastPresent-(FF-1)
g$LastChecked<-g$LastChecked-(FF-1)

#Obtention de la variable AgeDay1#
#--------------------------------#
#correspond à l'âge du nid lors du premier jour du suivi de nids
g$AgeDay1<-(g$AgeFound-g$FirstFound)+1

#Modèles
require(RMark)

#Attention ici la valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc<-max(g$LastChecked)

Dot=mark(g,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~1)), delete = T)

Hbt=mark(g,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"), delete = T) 

Hbt1=mark(g,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~hab)), groups = "hab", delete = T) 

AgeGeese=mark(g,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge)), delete = T)

AgeHbt=mark(g,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET)), groups="WET", delete = T)

AgeHbt1 = mark(g, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + hab)), groups = "hab", delete = T)


#####Plot of results#####
#AgeHbt
# Build design matrix with ages of interest
fc=find.covariates(AgeHbt,g);fc
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
with(data=Age.survival,plot(Age[HAB=="MES"],DSR[HAB=="MES"],'l',ylim=c(min(Age.survival$lclDSR),1),
  main =paste(c("Daily survival nest with age nest & mesic habitat- CONTROL", unique(g$AN))),
  xlab = "Age of nests",ylab=paste(c("DSR",unique(g$AN)))))
grid()
axis.break(axis=2,breakpos=min(Age.survival$lclDSR)+0.005,style='slash')
with(data=Age.survival,points(Age[HAB=="MES"],lclDSR[HAB=="MES"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="MES"],uclDSR[HAB=="MES"],'l',lty=3))

with(data=Age.survival,plot(Age[HAB=="WET"],DSR[HAB=="WET"],'l',ylim=c(min(Age.survival$lclDSR),1),
  main = paste(c("Daily survival nest with age nest & wetland habitat - CONTROL - ", unique(g$AN))),
  xlab = "Age of nests", ylab=paste(c("DSR",unique(g$AN)))))
grid()
axis.break(axis=2,breakpos=min(Age.survival$lclDSR)+0.005,style='slash')
with(data=Age.survival,points(Age[HAB=="WET"],lclDSR[HAB=="WET"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="WET"],uclDSR[HAB=="WET"],'l',lty=3))

