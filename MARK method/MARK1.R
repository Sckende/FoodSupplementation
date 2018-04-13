##### Killdeer example#####
# This example is excluded from testing to reduce package check time
# EXAMPLE CODE FOR CONVERSION OF .INP TO NECESSARY DATA STRUCTURE
# read in killdeer.inp file
#killdeer=scan("killdeer.inp",what="character",sep="\n")
# strip out ; and write out all but first 2 lines which contain comments
#write(sub(";","",killdeer[3:20]),"killdeer.txt")
# read in as a dataframe and assign names
#killdeer=read.table("killdeer.txt")
#names(killdeer)=c("id","FirstFound","LastPresent","LastChecked","Fate","Freq")

#
# EXAMPLE CODE TO RUN MODELS CONTAINED IN THE MARK KILLDEER.DBF
data(killdeer)
# produce summary
summary(killdeer)
# Define function to run models that are in killdeer.dbf
# You must specify either the number of occasions (nocc) or the time.intervals
# between the occasions.
#run.killdeer=function()
#{
  Sdot=mark(killdeer,model="Nest",nocc=40, delete = T)
  STime=mark(killdeer,model="Nest",
             model.parameters=list(S=list(formula=~I(Time+1))),nocc=40,threads=2, delete = T)
  STimesq=mark(killdeer,model="Nest",
               model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2))),nocc=40,threads=2, delete = T)
  STime3=mark(killdeer,model="Nest",
              model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2)+I((Time+1)^3))),
              nocc=40,threads=2, delete = T)
#  return(collect.models())
#}
# run defined models

killdeer.results=run.killdeer()
killdeer.results$Sdot

#####Mallard example#####

# This example is excluded from testing to reduce package check time
require(RMark)
require(plotrix)
# Retrieve data
data(mallard)
mallard
# Treat dummy variables for habitat types as factors
mallard$Native=factor(mallard$Native)
mallard$Planted=factor(mallard$Planted)
mallard$Wetland=factor(mallard$Wetland)
mallard$Roadside=factor(mallard$Roadside)
# Examine a summary of the dataset
summary(mallard)
# Write a function for evaluating a set of competing models
run.mallard=function()
{
  # 1. A model of constant daily survival rate (DSR)
  Dot=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~1)))
  # 2. DSR varies by habitat type - treats habitats as factors
  # and the output provides S-hats for each habitat type
  Hab=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~Native+Planted+Wetland)),
           groups=c("Native","Planted","Wetland"))
  # 3. DSR varies with vegetation thickness (Robel reading)
  # Note: coefficients are estimated using the actual covariate
  # values. They are not based on standardized covariate values.
  Robel=mark(mallard,nocc=90,model="Nest",
             model.parameters=list(S=list(formula=~Robel)))
  # 4. DSR varies with the amount of native vegetation in the surrounding area
  # Note: coefficients are estimated using the actual covariate
  # values. They are not based on standardized covariate values.
  PpnGr=mark(mallard,nocc=90,model="Nest",
             model.parameters=list(S=list(formula=~PpnGrass)))
  # 5. DSR follows a trend through time
  TimeTrend=mark(mallard,nocc=90,model="Nest",
                 model.parameters=list(S=list(formula=~Time)))
  # 6. DSR varies with nest age
  Age=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~NestAge)))
  # 7. DSR varies with nest age & habitat type
  AgeHab=mark(mallard,nocc=90,model="Nest",
              model.parameters=list(S=list(formula=~NestAge+Native+Planted+Wetland)),
              groups=c("Native","Planted","Wetland"))
  # 8. DSR varies with nest age & vegetation thickness
  AgeRobel=mark(mallard,nocc=90,model="Nest",
                model.parameters=list(S=list(formula=~NestAge+Robel)))
  # 9. DSR varies with nest age & amount of native vegetation in surrounding area
  AgePpnGrass=mark(mallard,nocc=90,model="Nest",
                   model.parameters=list(S=list(formula=~NestAge+PpnGrass)))
  #
  # Return model table and list of models
  #
  return(collect.models() )
}
# The next line runs the 9 models above and takes a minute or 2
mallard.results=run.mallard()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
export.MARK(mallard.results$Age$data,"MallDSR",mallard.results,replace=TRUE,ind.covariates="all")
mallard.results # print model-selection table to screen
options(width=100) # set page width to 100 characters
sink("results.table.txt") # capture screen output to file
print(mallard.results) # send output to file
sink() # return output to screen
# remove "#" on next line to see output in notepad
system("notepad results.table.txt",invisible=FALSE,wait=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$Dot # print MARK output to designated text editor
mallard.results$Dot$results$beta # view estimated beta for model in R
mallard.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$Hab # print MARK output to designated text editor
mallard.results$Hab$design.matrix # view the design matrix that was used
mallard.results$Hab$results$beta # view estimated beta for model in R
mallard.results$Hab$results$beta.vcv # view variance-covariance matrix for beta's
mallard.results$Hab$results$real # view the estimates of Daily Survival Rate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for best model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$AgePpnGrass # print MARK output to designated text editor
mallard.results$AgePpnGrass$results$beta # view estimated beta's in R
mallard.results$AgePpnGrass$results$beta.vcv # view estimated var-cov matrix in R


# To obtain estimates of DSR for various values of 'NestAge' and 'PpnGrass'
# some work additional work is needed.
# Store model results in object with simpler name
AgePpnGrass=mallard.results$AgePpnGrass

# Build design matrix with ages and ppn grass values of interest
# Relevant ages are age 1 to 35 for mallards
# For ppngrass, use a value of 0.5
fc=find.covariates(AgePpnGrass,mallard)

fc$value[1:35]=1:35 # assign 1:35 to 1st 35 nest ages

fc$value[fc$var=="PpnGrass"]=0.1 # assign new value to PpnGrass
design=fill.covariates(AgePpnGrass,fc) # fill design matrix with values

# extract 1st 35 rows of output
AgePpn.survival=compute.real(AgePpnGrass,design=design)[1:35,]

# insert covariate columns
AgePpn.survival=cbind(design[1:35,c(2:3)],AgePpn.survival)
colnames(AgePpn.survival)=c("Age","PpnGrass","DSR","seDSR","lclDSR","uclDSR")
AgePpn.survival # view estimates of DSR for each age and PpnGrass combo
# Plot results
with(data=AgePpn.survival,plot(Age,DSR,'l',ylim=c(0.88,1)))
grid()
axis.break(axis=2,breakpos=0.879,style='slash')
with(data=AgePpn.survival,points(Age,lclDSR,'l',lty=3))
with(data=AgePpn.survival,points(Age,uclDSR,'l',lty=3))
# assign 17 to 1st 50 nest ages
fc$value[1:89]=17
# assign range of values to PpnGrass
fc$value[fc$var=="PpnGrass"]=seq(0.01,0.99,length=89)
design=fill.covariates(AgePpnGrass,fc) # fill design matrix with values
AgePpn.survival=compute.real(AgePpnGrass,design=design)
# insert covariate columns
AgePpn.survival=cbind(design[,c(2:3)],AgePpn.survival)
colnames(AgePpn.survival)=
  c("Age","PpnGrass","DSR","seDSR","lclDSR","uclDSR")
# view estimates of DSR for each age and PpnGrass combo
AgePpn.survival
# Plot results
# open new graphics window for new plot
dev.new()
with(data=AgePpn.survival,plot(PpnGrass,DSR,'l',ylim=c(0.88,1)))
grid()
axis.break(axis=2,breakpos=0.879,style='slash')
with(data=AgePpn.survival,points(PpnGrass,lclDSR,'l',lty=3))
with(data=AgePpn.survival,points(PpnGrass,uclDSR,'l',lty=3))
# The "rm" command can be used to remove all objects from the .Rdata file.
# Cleaning up objects as shown in this script is good programming
# practice and a good idea as long as the computations are not time consuming.
# However, if your analysis is taking several hours or days in MARK then
# clearly you'll want to hang onto the resulting objects in R and you
# won't want to use the following command. It has been commented out for
# this example; the "#" on the next line needs to be removed to do the clean up.
# rm(list=ls(all=TRUE))
# The next line deletes orphaned output files from MARK.
# ?cleanup will give a more complete description of this function.
cleanup(ask=FALSE)
####################DONNÉES 2015####################
#####Snow Geese - CONTROL#####
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

#####Snow Geese - EXPERIMENTATION - MARK2#####
#fichier d'analyse de Rmark pour le calcul et l'étude du taux de survie journalier des nids supplémentés#
#attention pour SM2J prédation observée le jour même=>FirstFound=LastPresent et LastChecked=LastPresent+1#
#pour que la ligne soit valide#
#require(RMark)

#getwd()
#setwd("/Users/nicolas/Desktop/Claire/MARK_analysis")
#supp<-read.table("Mark2-2015.txt", h=T, dec=",", sep="\t")
#str(supp)
#dim(supp)
#summary(supp)

# Treat dummy variables for experimentation types as factors
#supp$FOOD=factor(supp$FOOD)
#supp$WATER=factor(supp$WATER)
#supp$FOWA=factor(supp$FOWA)
#supp$WET=factor(supp$WET)
#supp$MES=factor(supp$MES)

#summary(subset(supp,FOOD=="1"& MES=="1"))#pour obtenir les effectifs dans les diff?rents traitements et milieux


#summary(supp)
#run.expe=function ()
#{# 1. A model of constant daily survival rate (DSR)
 # Dot=mark(supp,nocc=27,model="Nest",model.parameters=list(S=list(formula=~1)))
  # 2. DSR varies by experimentation type - treats experimentation as factors
  # and the output provides S-expe for each experimentation type
  #EXPE=mark(supp,nocc=27,model="Nest", model.parameters=list(S=list(formula=~FOOD+WATER+FOWA)),groups=c("FOOD","WATER","FOWA"))
  # 3. DSR varies with habitat type
  #Habitat=mark(supp,nocc=27,model="Nest",model.parameters=list(S=list(formula=~MES+WET)),groups=c("MES","WET"))
  #EXPEHAB=mark(supp,nocc=27, model="Nest",model.parameters = list(S=list(formula=~FOOD+WATER+FOWA+MES+WET)),groups=c("FOOD","WATER","FOWA","MES","WET"))
  #Age=mark(supp,nocc=27,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
  #Predation=mark(supp,nocc=27,model="Nest",model.parameters=list(S=list(formula=~ParPred)))
  #ExperimentDate=mark(supp,nocc=27,model = "Nest", model.parameters = list(S=list(formula=~ExpDate)))
#           }

#expe.results=run.expe()
#expe.results


#####Snow Geese - EXPERIMENTATION - MARK3#####
#idem Fichier Rmark 2 mais sans les nids avec date d'initiation#
#aberrante pour utiliser l'analyse "AgeDay1" (SH36J, SM3J, SM11J, SM13J, SH16R)#
require(RMark)

getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
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
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
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

